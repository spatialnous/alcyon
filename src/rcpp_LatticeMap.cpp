// SPDX-FileCopyrightText: 2024-2025 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "rcpp_LatticeMap.hpp"

#include "salalib/gridproperties.hpp"

#include "communicator.hpp"
#include "helper_nullablevalue.hpp"

RCPP_EXPOSED_CLASS(LatticeMap);

// [[Rcpp::export("Rcpp_LatticeMap_createFromGrid")]]
Rcpp::XPtr<LatticeMap> createFromGrid(const double minX, const double minY, const double maxX,
                                      const double maxY, const double gridSize) {

    if (gridSize <= 0) {
        Rcpp::stop("gridSize can not be less or equal to zero (%d given)", gridSize);
    }
    // Create a new lattice map and set tha grid
    Region4f r(Point2f(minX, minY) /* bottom left */, Point2f(maxX, maxY) /* top right */);

    Rcpp::XPtr<LatticeMap> latticeMap = Rcpp::XPtr<LatticeMap>(new LatticeMap(r, "LatticeMap"));

    GridProperties gp(std::max(r.width(), r.height()));
    if (gridSize > gp.getMax() || gridSize < gp.getMin()) {
        Rcpp::stop("Chosen grid spacing %d is outside of the expected interval of"
                   "%d <= spacing <= %d",
                   gridSize, gp.getMin(), gp.getMax());
    }

    latticeMap->setGrid(gridSize, Point2f(0.0, 0.0));

    return latticeMap;
}

// [[Rcpp::export("Rcpp_LatticeMap_blockLines")]]
Rcpp::List blockLines(Rcpp::XPtr<LatticeMap> latticeMapPtr, Rcpp::XPtr<ShapeMap> boundaryMapPtr,
                      const Rcpp::Nullable<bool> copyMapNV = R_NilValue) {
    auto copyMap = NullableValue::get(copyMapNV, true);
    if (copyMap) {
        auto prevLatticeMap = latticeMapPtr;
        const auto &prevRegion = prevLatticeMap->getRegion();
        latticeMapPtr = Rcpp::XPtr(new LatticeMap(prevRegion));
        latticeMapPtr->copy(*prevLatticeMap, true, true);
    }
    std::vector<Line4f> lines;
    for (auto line : boundaryMapPtr->getAllShapesAsLines()) {
        lines.emplace_back(line.start(), line.end());
    }
    latticeMapPtr->blockLines(lines);

    return Rcpp::List::create(Rcpp::Named("completed") = true,
                              Rcpp::Named("newAttributes") = std::vector<std::string>(),
                              Rcpp::Named("newProperties") = std::vector<std::string>{"blocked"},
                              Rcpp::Named("mapPtr") = latticeMapPtr);
}

// [[Rcpp::export("Rcpp_LatticeMap_fill")]]
Rcpp::List fill(Rcpp::XPtr<LatticeMap> latticeMapPtr, Rcpp::NumericMatrix pointCoords,
                const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
                const Rcpp::Nullable<bool> progressNV = R_NilValue) {
    if (pointCoords.rows() == 0) {
        Rcpp::stop("No data provided in point coordinates matrix");
    }

    auto copyMap = NullableValue::get(copyMapNV, true);
    auto progress = NullableValue::get(progressNV, true);

    if (copyMap) {
        auto prevLatticeMap = latticeMapPtr;
        const auto &prevRegion = prevLatticeMap->getRegion();
        latticeMapPtr = Rcpp::XPtr(new LatticeMap(prevRegion));
        latticeMapPtr->copy(*prevLatticeMap, true, true);
    }
    auto region = latticeMapPtr->getRegion();
    for (int r = 0; r < pointCoords.rows(); ++r) {
        auto coordRow = pointCoords.row(r);
        Point2f p(coordRow[0], coordRow[1]);
        if (!region.contains(p)) {
            Rcpp::stop("Point (%d %d) outside of target lattice map region.", p.x, p.y);
        }
    }

    for (int r = 0; r < pointCoords.rows(); ++r) {
        auto coordRow = pointCoords.row(r);
        Point2f p(coordRow[0], coordRow[1]);
        latticeMapPtr->makePoints(p, 0, getCommunicator(progress).get());
    }

    return Rcpp::List::create(
        Rcpp::Named("completed") = true, Rcpp::Named("newAttributes") = std::vector<std::string>(),
        Rcpp::Named("newProperties") = std::vector<std::string>{"filled", "contextfilled"},
        Rcpp::Named("mapPtr") = latticeMapPtr);
}

// [[Rcpp::export("Rcpp_LatticeMap_makeGraph")]]
Rcpp::List makeGraph(Rcpp::XPtr<LatticeMap> latticeMapPtr, const bool boundaryGraph,
                     const double maxVisibility, const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
                     const Rcpp::Nullable<bool> progressNV = R_NilValue) {
    auto copyMap = NullableValue::get(copyMapNV, true);
    auto progress = NullableValue::get(progressNV, false);
    if (copyMap) {
        auto prevLatticeMap = latticeMapPtr;
        const auto &prevRegion = prevLatticeMap->getRegion();
        latticeMapPtr = Rcpp::XPtr(new LatticeMap(prevRegion));
        latticeMapPtr->copy(*prevLatticeMap, true, true);
    }
    auto prevAttributes = getLatticeMapAttributeNames(latticeMapPtr);
    try {
        latticeMapPtr->sparkGraph2(getCommunicator(progress).get(), boundaryGraph, maxVisibility);
    } catch (Communicator::CancelledException &) {
        return Rcpp::List::create(Rcpp::Named("completed") = false);
    }

    auto newAttributes = getLatticeMapAttributeNames(latticeMapPtr);

    for (auto prevAttribute : prevAttributes) {
        auto it = std::find(newAttributes.begin(), newAttributes.end(), prevAttribute);
        if (it != newAttributes.end()) {
            newAttributes.erase(it);
        }
    }

    return Rcpp::List::create(Rcpp::Named("completed") = true,
                              Rcpp::Named("newAttributes") = newAttributes,
                              Rcpp::Named("newProperties") = std::vector<std::string>{},
                              Rcpp::Named("mapPtr") = latticeMapPtr);
}

// [[Rcpp::export("Rcpp_LatticeMap_unmakeGraph")]]
Rcpp::List unmakeGraph(Rcpp::XPtr<LatticeMap> latticeMapPtr, bool removeLinksWhenUnmaking,
                       const Rcpp::Nullable<bool> copyMapNV = R_NilValue) {
    auto copyMap = NullableValue::get(copyMapNV, true);
    if (copyMap) {
        auto prevLatticeMap = latticeMapPtr;
        const auto &prevRegion = prevLatticeMap->getRegion();
        latticeMapPtr = Rcpp::XPtr(new LatticeMap(prevRegion));
        latticeMapPtr->copy(*prevLatticeMap, true, true);
    }
    if (!latticeMapPtr->isProcessed()) {
        Rcpp::stop("Current map has not had its graph "
                   "made so there's nothing to unmake");
    }

    bool unmade = latticeMapPtr->unmake(removeLinksWhenUnmaking);
    return Rcpp::List::create(Rcpp::Named("completed") = unmade,
                              Rcpp::Named("newAttributes") = std::vector<std::string>{},
                              Rcpp::Named("newProperties") = std::vector<std::string>{},
                              Rcpp::Named("mapPtr") = latticeMapPtr);
}

// [[Rcpp::export("Rcpp_LatticeMap_getName")]]
std::string latticeMapGetName(Rcpp::XPtr<LatticeMap> latticeMapPtr) {
    return latticeMapPtr->getName();
}

// [[Rcpp::export("Rcpp_LatticeMap_getLinks")]]
Rcpp::IntegerMatrix latticeMapGetLinks(Rcpp::XPtr<LatticeMap> latticeMapPtr) {
    auto mergedPixelPairs = latticeMapPtr->getMergedPixelPairs();
    Rcpp::IntegerMatrix linkData(mergedPixelPairs.size(), 2L);
    Rcpp::colnames(linkData) = Rcpp::CharacterVector({"from", "to"});
    int rowIdx = 0;
    for (auto link : mergedPixelPairs) {
        const Rcpp::IntegerMatrix::Row &row = linkData(rowIdx, Rcpp::_);
        row[0] = link.first;
        row[1] = link.second;
        rowIdx++;
    }
    return linkData;
}

// [[Rcpp::export("Rcpp_LatticeMap_getConnections")]]
Rcpp::IntegerMatrix latticeMapGetConnections(Rcpp::XPtr<LatticeMap> latticeMapPtr) {
    auto &points = latticeMapPtr->getPoints();
    int numConnections = 0;
    for (size_t i = 0; i < points.columns(); i++) {
        for (size_t j = 0; j < points.rows(); j++) {
            Point &pnt = points(static_cast<size_t>(j), static_cast<size_t>(i));
            if (pnt.filled() && pnt.hasNode()) {
                PixelRef pix(i, j);
                PixelRefVector connections;
                pnt.getNode().contents(connections);
                numConnections += connections.size();
            }
        }
    }

    Rcpp::IntegerMatrix connectionData(numConnections, 2L);
    Rcpp::colnames(connectionData) = Rcpp::CharacterVector({"from", "to"});
    int rowIdx = 0;
    for (size_t i = 0; i < points.columns(); i++) {
        for (size_t j = 0; j < points.rows(); j++) {
            Point &pnt = points(static_cast<size_t>(j), static_cast<size_t>(i));
            if (pnt.filled() && pnt.hasNode()) {
                PixelRef pix(i, j);
                PixelRefVector hood;
                pnt.getNode().contents(hood);
                for (PixelRef &p : hood) {
                    const Rcpp::IntegerMatrix::Row &row = connectionData(rowIdx, Rcpp::_);
                    row[0] = pix;
                    row[1] = p;
                    rowIdx++;
                }
            }
        }
    }
    return connectionData;
}

// [[Rcpp::export("Rcpp_LatticeMap_getGridCoordinates")]]
Rcpp::NumericMatrix getGridCoordinates(Rcpp::XPtr<LatticeMap> latticeMapPtr) {
    Rcpp::NumericMatrix coords(latticeMapPtr->getRows() * latticeMapPtr->getCols(), 3);
    Rcpp::CharacterVector colNames(3);
    colNames[0] = "x";
    colNames[1] = "y";
    colNames[2] = "Ref";
    Rcpp::colnames(coords) = colNames;
    int rowIdx = 0;
    for (size_t i = 0; i < latticeMapPtr->getRows(); i++) {
        for (size_t j = 0; j < latticeMapPtr->getCols(); j++) {
            PixelRef ref(j, i);
            const auto &point = latticeMapPtr->getPoint(ref);
            const Rcpp::NumericMatrix::Row &row = coords(rowIdx, Rcpp::_);
            row[0] = point.getLocation().x;
            row[1] = point.getLocation().y;
            row[2] = static_cast<int>(ref);
            rowIdx++;
        }
    }
    return coords;
}

std::vector<std::string> getLatticeMapAttributeNames(LatticeMap *latticeMap) {
    std::vector<std::string> names;
    auto &attributes = latticeMap->getAttributeTable();
    int numCols = attributes.getNumColumns();
    // + 1 for the key column
    names.reserve(1 + numCols);
    names.push_back(attributes.getColumnName(size_t(-1)));
    for (size_t i = 0; i < attributes.getNumColumns(); ++i) {
        names.push_back(attributes.getColumnName(i));
    }
    return names;
}

// [[Rcpp::export("Rcpp_LatticeMap_getAttributeNames")]]
std::vector<std::string> getLatticeMapAttributeNames(Rcpp::XPtr<LatticeMap> latticeMap) {
    return getLatticeMapAttributeNames(latticeMap.get());
}

// [[Rcpp::export("Rcpp_LatticeMap_getAttributeData")]]
std::map<std::string, std::vector<double>>
getLatticeMapAttributeData(Rcpp::XPtr<LatticeMap> latticeMap,
                           std::vector<std::string> attributeNames) {
    auto &attrbs = latticeMap->getAttributeTable();
    std::map<std::string, std::vector<double>> data;
    for (auto &attributeName : attributeNames) {
        auto &attributeData = data[attributeName];
        attributeData.reserve(latticeMap->getRows() * latticeMap->getCols());
        if (attributeName == attrbs.getColumnName(size_t(-1))) {
            for (size_t i = 0; i < latticeMap->getRows(); i++) {
                for (size_t j = 0; j < latticeMap->getCols(); j++) {
                    PixelRef ref(j, i);
                    const auto &point = latticeMap->getPoint(ref);
                    if (point.filled()) {
                        attributeData.push_back(ref);
                    } else {
                        attributeData.push_back(nan(""));
                    }
                }
            }
        } else {
            size_t colIdx = attrbs.getColumnIndex(attributeName);

            for (size_t i = 0; i < latticeMap->getRows(); i++) {
                for (size_t j = 0; j < latticeMap->getCols(); j++) {
                    PixelRef ref(j, i);
                    const auto &point = latticeMap->getPoint(ref);
                    if (point.filled()) {
                        const auto &row = latticeMap->getAttributeTable().getRow(AttributeKey(ref));
                        attributeData.push_back(row.getValue(colIdx));
                    } else {
                        attributeData.push_back(nan(""));
                    }
                }
            }
        }
    }
    return data;
}

// [[Rcpp::export("Rcpp_LatticeMap_getPropertyData")]]
std::map<std::string, std::vector<double>>
getLatticeMapPropertyData(Rcpp::XPtr<LatticeMap> latticeMap,
                          std::vector<std::string> propertyNames) {
    std::vector<std::string> cellProperties{"x",    "y",  "filled", "blocked", "contextfilled",
                                            "edge", "Ref"};

    for (auto &propertyName : propertyNames) {
        if (std::find(cellProperties.begin(), cellProperties.end(), propertyName) ==
            cellProperties.end()) {
            Rcpp::Rcerr << "Property \"" << propertyName << "\" is not known\n";
        }
    }
    std::map<std::string, std::vector<double>> data;
    for (auto &propertyName : propertyNames) {
        auto &propertyData = data[propertyName];
        propertyData.reserve(latticeMap->getCols() * latticeMap->getRows());
        for (size_t i = 0; i < latticeMap->getRows(); i++) {
            for (size_t j = 0; j < latticeMap->getCols(); j++) {
                PixelRef ref(j, i);
                double propertyValue = -1;
                if (propertyName == "Ref") {
                    propertyValue = ref;
                } else {
                    const auto &p = latticeMap->getPoint(ref);
                    if (propertyName == "x") {
                        propertyValue = p.getLocation().x;
                    } else if (propertyName == "y") {
                        propertyValue = p.getLocation().y;
                    } else if (propertyName == "filled") {
                        propertyValue = p.filled();
                    } else if (propertyName == "blocked") {
                        propertyValue = p.blocked();
                    } else if (propertyName == "contextfilled") {
                        propertyValue = p.contextfilled();
                    } else if (propertyName == "edge") {
                        propertyValue = p.edge();
                    } else if (propertyName == "augmented") {
                        propertyValue = p.augmented();
                    }
                }
                propertyData.push_back(propertyValue);
            }
        }
    }
    return data;
}

// [[Rcpp::export("Rcpp_LatticeMap_getFilledPoints")]]
Rcpp::NumericMatrix getFilledPoints(Rcpp::XPtr<LatticeMap> latticeMapPtr) {
    const auto &attrTable = latticeMapPtr->getAttributeTable();
    int numCols = attrTable.getNumColumns();
    std::vector<std::string> cellProperties{"x",    "y",  "filled", "blocked", "contextfilled",
                                            "edge", "Ref"};
    Rcpp::NumericMatrix coordsData(latticeMapPtr->getFilledPointCount(),
                                   cellProperties.size() + numCols);
    Rcpp::CharacterVector colNames(cellProperties.size() + numCols);
    {
        int i = 0;
        for (auto prop : cellProperties) {
            colNames[i] = prop;
            ++i;
        }
    }
    for (int i = 0; i < numCols; ++i) {
        colNames[cellProperties.size() + i] = attrTable.getColumnName(i);
    }

    Rcpp::colnames(coordsData) = colNames;
    const auto &points = latticeMapPtr->getPoints();

    int rowIdx = 0;
    auto attrRowIt = attrTable.begin();
    for (auto &point : points) {
        if (!point.filled())
            continue;
        const Rcpp::NumericMatrix::Row &row = coordsData(rowIdx, Rcpp::_);
        row[0] = point.getLocation().x;
        row[1] = point.getLocation().y;
        row[2] = point.filled();
        row[3] = point.blocked();
        row[4] = point.contextfilled();
        row[5] = point.edge();
        row[6] = attrRowIt->getKey().value;
        for (int i = 0; i < numCols; ++i) {
            row[cellProperties.size() + i] = attrRowIt->getRow().getValue(i);
        }
        rowIdx++;
        attrRowIt++;
    }
    return coordsData;
}
