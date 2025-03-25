// SPDX-FileCopyrightText: 2024-2025 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "rcpp_PointMap.hpp"

#include "salalib/gridproperties.hpp"

#include "communicator.hpp"
#include "helper_nullablevalue.hpp"

RCPP_EXPOSED_CLASS(PointMap);

// [[Rcpp::export("Rcpp_PointMap_createFromGrid")]]
Rcpp::XPtr<PointMap> createFromGrid(const double minX, const double minY, const double maxX,
                                    const double maxY, const double gridSize) {

    if (gridSize <= 0) {
        Rcpp::stop("gridSize can not be less or equal to zero (%d given)", gridSize);
    }
    // Create a new pointmap and set tha grid
    Region4f r(Point2f(minX, minY) /* bottom left */, Point2f(maxX, maxY) /* top right */);

    Rcpp::XPtr<PointMap> pointMap = Rcpp::XPtr<PointMap>(new PointMap(r, "PointMap"));

    GridProperties gp(std::max(r.width(), r.height()));
    if (gridSize > gp.getMax() || gridSize < gp.getMin()) {
        Rcpp::stop("Chosen grid spacing %d is outside of the expected interval of"
                   "%d <= spacing <= %d",
                   gridSize, gp.getMin(), gp.getMax());
    }

    pointMap->setGrid(gridSize, Point2f(0.0, 0.0));

    return pointMap;
}

// [[Rcpp::export("Rcpp_PointMap_blockLines")]]
Rcpp::List blockLines(Rcpp::XPtr<PointMap> pointMapPtr, Rcpp::XPtr<ShapeMap> boundaryMapPtr,
                      const Rcpp::Nullable<bool> copyMapNV = R_NilValue) {
    auto copyMap = NullableValue::get(copyMapNV, true);
    if (copyMap) {
        auto prevPointMap = pointMapPtr;
        const auto &prevRegion = prevPointMap->getRegion();
        pointMapPtr = Rcpp::XPtr(new PointMap(prevRegion));
        pointMapPtr->copy(*prevPointMap, true, true);
    }
    std::vector<Line4f> lines;
    for (auto line : boundaryMapPtr->getAllShapesAsLines()) {
        lines.emplace_back(line.start(), line.end());
    }
    pointMapPtr->blockLines(lines);

    return Rcpp::List::create(Rcpp::Named("completed") = true,
                              Rcpp::Named("newAttributes") = std::vector<std::string>(),
                              Rcpp::Named("newProperties") = std::vector<std::string>{"blocked"},
                              Rcpp::Named("mapPtr") = pointMapPtr);
}

// [[Rcpp::export("Rcpp_PointMap_fill")]]
Rcpp::List fill(Rcpp::XPtr<PointMap> pointMapPtr, Rcpp::NumericMatrix pointCoords,
                const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
                const Rcpp::Nullable<bool> progressNV = R_NilValue) {
    if (pointCoords.rows() == 0) {
        Rcpp::stop("No data provided in point coordinates matrix");
    }

    auto copyMap = NullableValue::get(copyMapNV, true);
    auto progress = NullableValue::get(progressNV, true);

    if (copyMap) {
        auto prevPointMap = pointMapPtr;
        const auto &prevRegion = prevPointMap->getRegion();
        pointMapPtr = Rcpp::XPtr(new PointMap(prevRegion));
        pointMapPtr->copy(*prevPointMap, true, true);
    }
    auto region = pointMapPtr->getRegion();
    for (int r = 0; r < pointCoords.rows(); ++r) {
        auto coordRow = pointCoords.row(r);
        Point2f p(coordRow[0], coordRow[1]);
        if (!region.contains(p)) {
            Rcpp::stop("Point (%d %d) outside of target pointmap region.", p.x, p.y);
        }
    }

    for (int r = 0; r < pointCoords.rows(); ++r) {
        auto coordRow = pointCoords.row(r);
        Point2f p(coordRow[0], coordRow[1]);
        pointMapPtr->makePoints(p, 0, getCommunicator(progress).get());
    }

    return Rcpp::List::create(
        Rcpp::Named("completed") = true, Rcpp::Named("newAttributes") = std::vector<std::string>(),
        Rcpp::Named("newProperties") = std::vector<std::string>{"filled", "contextfilled"},
        Rcpp::Named("mapPtr") = pointMapPtr);
}

// [[Rcpp::export("Rcpp_PointMap_makeGraph")]]
Rcpp::List makeGraph(Rcpp::XPtr<PointMap> pointMapPtr, const bool boundaryGraph,
                     const double maxVisibility, const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
                     const Rcpp::Nullable<bool> progressNV = R_NilValue) {
    auto copyMap = NullableValue::get(copyMapNV, true);
    auto progress = NullableValue::get(progressNV, false);
    if (copyMap) {
        auto prevPointMap = pointMapPtr;
        const auto &prevRegion = prevPointMap->getRegion();
        pointMapPtr = Rcpp::XPtr(new PointMap(prevRegion));
        pointMapPtr->copy(*prevPointMap, true, true);
    }
    auto prevAttributes = getPointMapAttributeNames(pointMapPtr);
    try {
        pointMapPtr->sparkGraph2(getCommunicator(progress).get(), boundaryGraph, maxVisibility);
    } catch (Communicator::CancelledException &) {
        return Rcpp::List::create(Rcpp::Named("completed") = false);
    }

    auto newAttributes = getPointMapAttributeNames(pointMapPtr);

    for (auto prevAttribute : prevAttributes) {
        auto it = std::find(newAttributes.begin(), newAttributes.end(), prevAttribute);
        if (it != newAttributes.end()) {
            newAttributes.erase(it);
        }
    }

    return Rcpp::List::create(Rcpp::Named("completed") = true,
                              Rcpp::Named("newAttributes") = newAttributes,
                              Rcpp::Named("newProperties") = std::vector<std::string>{},
                              Rcpp::Named("mapPtr") = pointMapPtr);
}

// [[Rcpp::export("Rcpp_PointMap_unmakeGraph")]]
Rcpp::List unmakeGraph(Rcpp::XPtr<PointMap> pointMapPtr, bool removeLinksWhenUnmaking,
                       const Rcpp::Nullable<bool> copyMapNV = R_NilValue) {
    auto copyMap = NullableValue::get(copyMapNV, true);
    if (copyMap) {
        auto prevPointMap = pointMapPtr;
        const auto &prevRegion = prevPointMap->getRegion();
        pointMapPtr = Rcpp::XPtr(new PointMap(prevRegion));
        pointMapPtr->copy(*prevPointMap, true, true);
    }
    if (!pointMapPtr->isProcessed()) {
        Rcpp::stop("Current map has not had its graph "
                   "made so there's nothing to unmake");
    }

    bool unmade = pointMapPtr->unmake(removeLinksWhenUnmaking);
    return Rcpp::List::create(Rcpp::Named("completed") = unmade,
                              Rcpp::Named("newAttributes") = std::vector<std::string>{},
                              Rcpp::Named("newProperties") = std::vector<std::string>{},
                              Rcpp::Named("mapPtr") = pointMapPtr);
}

// [[Rcpp::export("Rcpp_PointMap_getName")]]
std::string pointMapGetName(Rcpp::XPtr<PointMap> pointMapPtr) { return pointMapPtr->getName(); }

// [[Rcpp::export("Rcpp_PointMap_getLinks")]]
Rcpp::IntegerMatrix pointMapGetLinks(Rcpp::XPtr<PointMap> pointMapPtr) {
    auto mergedPixelPairs = pointMapPtr->getMergedPixelPairs();
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

// [[Rcpp::export("Rcpp_PointMap_getConnections")]]
Rcpp::IntegerMatrix pointMapGetConnections(Rcpp::XPtr<PointMap> pointMapPtr) {
    auto &points = pointMapPtr->getPoints();
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

// [[Rcpp::export("Rcpp_PointMap_getGridCoordinates")]]
Rcpp::NumericMatrix getGridCoordinates(Rcpp::XPtr<PointMap> pointMapPtr) {
    Rcpp::NumericMatrix coords(pointMapPtr->getRows() * pointMapPtr->getCols(), 3);
    Rcpp::CharacterVector colNames(3);
    colNames[0] = "x";
    colNames[1] = "y";
    colNames[2] = "Ref";
    Rcpp::colnames(coords) = colNames;
    int rowIdx = 0;
    for (size_t i = 0; i < pointMapPtr->getRows(); i++) {
        for (size_t j = 0; j < pointMapPtr->getCols(); j++) {
            PixelRef ref(j, i);
            const auto &point = pointMapPtr->getPoint(ref);
            const Rcpp::NumericMatrix::Row &row = coords(rowIdx, Rcpp::_);
            row[0] = point.getLocation().x;
            row[1] = point.getLocation().y;
            row[2] = static_cast<int>(ref);
            rowIdx++;
        }
    }
    return coords;
}

std::vector<std::string> getPointMapAttributeNames(PointMap *pointMap) {
    std::vector<std::string> names;
    auto &attributes = pointMap->getAttributeTable();
    int numCols = attributes.getNumColumns();
    // + 1 for the key column
    names.reserve(1 + numCols);
    names.push_back(attributes.getColumnName(size_t(-1)));
    for (size_t i = 0; i < attributes.getNumColumns(); ++i) {
        names.push_back(attributes.getColumnName(i));
    }
    return names;
}

// [[Rcpp::export("Rcpp_PointMap_getAttributeNames")]]
std::vector<std::string> getPointMapAttributeNames(Rcpp::XPtr<PointMap> pointMap) {
    return getPointMapAttributeNames(pointMap.get());
}

// [[Rcpp::export("Rcpp_PointMap_getAttributeData")]]
std::map<std::string, std::vector<double>>
getPointMapAttributeData(Rcpp::XPtr<PointMap> pointMap, std::vector<std::string> attributeNames) {
    auto &attrbs = pointMap->getAttributeTable();
    std::map<std::string, std::vector<double>> data;
    for (auto &attributeName : attributeNames) {
        auto &attributeData = data[attributeName];
        attributeData.reserve(pointMap->getRows() * pointMap->getCols());
        if (attributeName == attrbs.getColumnName(size_t(-1))) {
            for (size_t i = 0; i < pointMap->getRows(); i++) {
                for (size_t j = 0; j < pointMap->getCols(); j++) {
                    PixelRef ref(j, i);
                    const auto &point = pointMap->getPoint(ref);
                    if (point.filled()) {
                        attributeData.push_back(ref);
                    } else {
                        attributeData.push_back(nan(""));
                    }
                }
            }
        } else {
            size_t colIdx = attrbs.getColumnIndex(attributeName);

            for (size_t i = 0; i < pointMap->getRows(); i++) {
                for (size_t j = 0; j < pointMap->getCols(); j++) {
                    PixelRef ref(j, i);
                    const auto &point = pointMap->getPoint(ref);
                    if (point.filled()) {
                        const auto &row = pointMap->getAttributeTable().getRow(AttributeKey(ref));
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

// [[Rcpp::export("Rcpp_PointMap_getPropertyData")]]
std::map<std::string, std::vector<double>>
getPointMapPropertyData(Rcpp::XPtr<PointMap> pointMap, std::vector<std::string> propertyNames) {
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
        propertyData.reserve(pointMap->getCols() * pointMap->getRows());
        for (size_t i = 0; i < pointMap->getRows(); i++) {
            for (size_t j = 0; j < pointMap->getCols(); j++) {
                PixelRef ref(j, i);
                double propertyValue = -1;
                if (propertyName == "Ref") {
                    propertyValue = ref;
                } else {
                    const auto &p = pointMap->getPoint(ref);
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

// [[Rcpp::export("Rcpp_PointMap_getFilledPoints")]]
Rcpp::NumericMatrix getFilledPoints(Rcpp::XPtr<PointMap> pointMapPtr) {
    const auto &attrTable = pointMapPtr->getAttributeTable();
    int numCols = attrTable.getNumColumns();
    std::vector<std::string> cellProperties{"x",    "y",  "filled", "blocked", "contextfilled",
                                            "edge", "Ref"};
    Rcpp::NumericMatrix coordsData(pointMapPtr->getFilledPointCount(),
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
    const auto &points = pointMapPtr->getPoints();

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
