// SPDX-FileCopyrightText: 2024-2025 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "salalib/pointmap.hpp"
#include "salalib/shapegraph.hpp"

#include "communicator.hpp"
#include "helper_nullablevalue.hpp"

#include <Rcpp.h>

// [[Rcpp::export("Rcpp_ShapeGraph_linkCoords")]]
Rcpp::List shapeGraphLinkCoords(Rcpp::XPtr<ShapeGraph> shapeGraphPtr, Rcpp::NumericMatrix coords,
                                const Rcpp::Nullable<bool> copyMapNV = R_NilValue) {
    if (coords.cols() != 4) {
        Rcpp::stop("The coords matrix needs to have 4 columns: x1, y1, x2, y2");
    }
    auto copyMap = NullableValue::get(copyMapNV, true);
    if (copyMap) {
        auto prevShapeGraph = shapeGraphPtr;
        shapeGraphPtr = Rcpp::XPtr(new ShapeGraph());
        shapeGraphPtr->copy(*prevShapeGraph, ShapeMap::COPY_ALL, true);
    }
    bool completed = true;
    for (int i = 0; i < coords.rows(); ++i) {
        const Rcpp::NumericMatrix::Row &row = coords(i, Rcpp::_);
        Region4f region(Point2f(row[0], row[1]), Point2f(row[0], row[1]));
        auto shapesInRegion = shapeGraphPtr->getShapesInRegion(region);
        completed &=
            shapeGraphPtr->linkShapes(Point2f(row[2], row[3]), shapesInRegion.begin()->first);
    }
    return Rcpp::List::create(Rcpp::Named("completed") = completed,
                              Rcpp::Named("newAttributes") = std::vector<std::string>(),
                              Rcpp::Named("mapPtr") = shapeGraphPtr);
}

// [[Rcpp::export("Rcpp_ShapeGraph_linkRefs")]]
Rcpp::List shapeGraphLinkRefs(Rcpp::XPtr<ShapeGraph> shapeGraphPtr, Rcpp::IntegerMatrix refs,
                              const Rcpp::Nullable<bool> copyMapNV = R_NilValue) {
    if (refs.cols() != 2) {
        Rcpp::stop("The refs matrix needs to have 2 columns: fromRef, toRef");
    }
    auto copyMap = NullableValue::get(copyMapNV, true);
    if (copyMap) {
        auto prevShapeGraph = shapeGraphPtr;
        shapeGraphPtr = Rcpp::XPtr(new ShapeGraph());
        shapeGraphPtr->copy(*prevShapeGraph, ShapeMap::COPY_ALL, true);
    }
    for (int i = 0; i < refs.rows(); ++i) {
        const Rcpp::IntegerMatrix::Row &row = refs(i, Rcpp::_);
        shapeGraphPtr->linkShapesFromRefs(row[0], row[1]);
    }
    return Rcpp::List::create(Rcpp::Named("completed") = true,
                              Rcpp::Named("newAttributes") = std::vector<std::string>(),
                              Rcpp::Named("mapPtr") = shapeGraphPtr);
}

// [[Rcpp::export("Rcpp_PointMap_linkCoords")]]
Rcpp::List pointMapLinkCoords(Rcpp::XPtr<PointMap> pointMapPtr, Rcpp::NumericMatrix coords,
                              const Rcpp::Nullable<bool> copyMapNV = R_NilValue) {
    if (coords.cols() != 4) {
        Rcpp::stop("The coords matrix needs to have 4 columns: x1, y1, x2, y2");
    }
    auto copyMap = NullableValue::get(copyMapNV, true);
    if (copyMap) {
        auto prevPointMap = pointMapPtr;
        const auto &prevRegion = prevPointMap->getRegion();
        pointMapPtr = Rcpp::XPtr(new PointMap(prevRegion));
        pointMapPtr->copy(*prevPointMap, true, true);
    }
    for (int i = 0; i < coords.rows(); ++i) {
        const Rcpp::NumericMatrix::Row &row = coords(i, Rcpp::_);
        const PixelRef &a = pointMapPtr->pixelate(Point2f(row[0], row[1]), false);
        const PixelRef &b = pointMapPtr->pixelate(Point2f(row[2], row[3]), false);

        if (!pointMapPtr->includes(a) || !pointMapPtr->getPoint(a).filled()) {
            Rcpp::stop("Point on line %d (%f, %f) not on filled analysis space", i, row[0], row[1]);
        }

        if (!pointMapPtr->includes(b) || !pointMapPtr->getPoint(b).filled()) {
            Rcpp::stop("Point on line %d (%f, %f) not on filled analysis space", i, row[2], row[3]);
        }

        if (pointMapPtr->isPixelMerged(a)) {
            Rcpp::stop("Point on line %d (%f, %f) is already part of a link", i, row[0], row[1]);
        }

        if (pointMapPtr->isPixelMerged(b)) {
            Rcpp::stop("Point on line %d (%f, %f) is already part of a link", i, row[2], row[3]);
        }

        pointMapPtr->mergePixels(a, b);
    }
    return Rcpp::List::create(Rcpp::Named("completed") = true,
                              Rcpp::Named("newAttributes") = std::vector<std::string>(),
                              Rcpp::Named("newProperties") = std::vector<std::string>(),
                              Rcpp::Named("mapPtr") = pointMapPtr);
}

// [[Rcpp::export("Rcpp_PointMap_linkRefs")]]
Rcpp::List pointMapLinkRefs(Rcpp::XPtr<PointMap> pointMapPtr, Rcpp::IntegerMatrix refs,
                            const Rcpp::Nullable<bool> copyMapNV = R_NilValue) {
    if (refs.cols() != 2) {
        Rcpp::stop("The refs matrix needs to have 2 columns: fromRef, toRef");
    }
    auto copyMap = NullableValue::get(copyMapNV, true);
    if (copyMap) {
        auto prevPointMap = pointMapPtr;
        const auto &prevRegion = prevPointMap->getRegion();
        pointMapPtr = Rcpp::XPtr(new PointMap(prevRegion));
        pointMapPtr->copy(*prevPointMap, true, true);
    }
    for (int i = 0; i < refs.rows(); ++i) {
        const Rcpp::IntegerMatrix::Row &row = refs(i, Rcpp::_);

        const PixelRef a(row[0]);
        const PixelRef b(row[1]);

        if (!pointMapPtr->includes(a) || !pointMapPtr->getPoint(a).filled()) {
            Rcpp::stop("Point on line %d (%d) not on filled analysis space", i, row[0]);
        }

        if (!pointMapPtr->includes(b) || !pointMapPtr->getPoint(b).filled()) {
            Rcpp::stop("Point on line %d (%d) not on filled analysis space", i, row[1]);
        }

        if (pointMapPtr->isPixelMerged(a)) {
            Rcpp::stop("Point on line %d (%d) is already part of a link", i, row[0]);
        }

        if (pointMapPtr->isPixelMerged(b)) {
            Rcpp::stop("Point on line %d (%d) is already part of a link", i, row[1]);
        }

        pointMapPtr->mergePixels(row[0], row[1]);
    }
    return Rcpp::List::create(Rcpp::Named("completed") = true,
                              Rcpp::Named("newAttributes") = std::vector<std::string>(),
                              Rcpp::Named("newProperties") = std::vector<std::string>(),
                              Rcpp::Named("mapPtr") = pointMapPtr);
}

// [[Rcpp::export("Rcpp_ShapeGraph_unlinkCoords")]]
Rcpp::List shapeMapUnlinkCoords(Rcpp::XPtr<ShapeGraph> shapeGraphPtr, Rcpp::NumericMatrix coords,
                                const Rcpp::Nullable<bool> copyMapNV = R_NilValue) {
    if (coords.cols() != 4) {
        Rcpp::stop("The coords matrix needs to have 4 columns: x1, y1, x2, y2");
    }
    auto copyMap = NullableValue::get(copyMapNV, true);
    if (copyMap) {
        auto prevShapeGraph = shapeGraphPtr;
        shapeGraphPtr = Rcpp::XPtr(new ShapeGraph());
        shapeGraphPtr->copy(*prevShapeGraph, ShapeMap::COPY_ALL, true);
    }
    for (int i = 0; i < coords.rows(); ++i) {
        const Rcpp::NumericMatrix::Row &row = coords(i, Rcpp::_);
        shapeGraphPtr->unlinkShapes(Point2f(row[0], row[1]), Point2f(row[2], row[3]));
    }
    return Rcpp::List::create(Rcpp::Named("completed") = true,
                              Rcpp::Named("newAttributes") = std::vector<std::string>(),
                              Rcpp::Named("mapPtr") = shapeGraphPtr);
}

// [[Rcpp::export("Rcpp_ShapeGraph_unlinkAtCrossPoint")]]
Rcpp::List shapeGraphUnlinkAtCrossPoint(Rcpp::XPtr<ShapeGraph> shapeGraphPtr,
                                        Rcpp::NumericMatrix coords,
                                        const Rcpp::Nullable<bool> copyMapNV = R_NilValue) {
    if (coords.cols() != 2) {
        Rcpp::stop("The coords matrix needs to have 2 columns: x, y");
    }
    auto copyMap = NullableValue::get(copyMapNV, true);
    if (copyMap) {
        auto prevShapeGraph = shapeGraphPtr;
        shapeGraphPtr = Rcpp::XPtr(new ShapeGraph());
        shapeGraphPtr->copy(*prevShapeGraph, ShapeMap::COPY_ALL, true);
    }
    for (int i = 0; i < coords.rows(); ++i) {
        const Rcpp::NumericMatrix::Row &row = coords(i, Rcpp::_);
        shapeGraphPtr->unlinkAtPoint(Point2f(row[0], row[1]));
    }
    return Rcpp::List::create(Rcpp::Named("completed") = true,
                              Rcpp::Named("newAttributes") = std::vector<std::string>(),
                              Rcpp::Named("mapPtr") = shapeGraphPtr);
}

// [[Rcpp::export("Rcpp_ShapeGraph_unlinkRefs")]]
Rcpp::List shapeMapUnlinkRefs(Rcpp::XPtr<ShapeGraph> shapeGraphPtr, Rcpp::IntegerMatrix refs,
                              const Rcpp::Nullable<bool> copyMapNV = R_NilValue) {
    if (refs.cols() != 2) {
        Rcpp::stop("The refs matrix needs to have 2 columns: fromRef, toRef");
    }
    auto copyMap = NullableValue::get(copyMapNV, true);
    if (copyMap) {
        auto prevShapeGraph = shapeGraphPtr;
        shapeGraphPtr = Rcpp::XPtr(new ShapeGraph());
        shapeGraphPtr->copy(*prevShapeGraph, ShapeMap::COPY_ALL, true);
    }
    for (int i = 0; i < refs.rows(); ++i) {
        const Rcpp::IntegerMatrix::Row &row = refs(i, Rcpp::_);
        shapeGraphPtr->unlinkShapesFromRefs(row[0], row[1]);
    }
    return Rcpp::List::create(Rcpp::Named("completed") = true,
                              Rcpp::Named("newAttributes") = std::vector<std::string>(),
                              Rcpp::Named("mapPtr") = shapeGraphPtr);
}

// [[Rcpp::export("Rcpp_PointMap_unlinkCoords")]]
Rcpp::List pointMapUnlinkCoords(Rcpp::XPtr<PointMap> pointMapPtr, Rcpp::NumericMatrix coords,
                                const Rcpp::Nullable<bool> copyMapNV = R_NilValue) {
    if (coords.cols() != 4) {
        Rcpp::stop("The coords matrix needs to have 4 columns: x1, y1, x2, y2");
    }
    auto copyMap = NullableValue::get(copyMapNV, true);
    if (copyMap) {
        auto prevPointMap = pointMapPtr;
        const auto &prevRegion = prevPointMap->getRegion();
        pointMapPtr = Rcpp::XPtr(new PointMap(prevRegion));
        pointMapPtr->copy(*prevPointMap, true, true);
    }
    for (int i = 0; i < coords.rows(); ++i) {
        const Rcpp::NumericMatrix::Row &row = coords(i, Rcpp::_);
        const PixelRef &a = pointMapPtr->pixelate(Point2f(row[0], row[1]), false);
        const PixelRef &b = pointMapPtr->pixelate(Point2f(row[2], row[3]), false);

        if (!pointMapPtr->includes(a) || !pointMapPtr->getPoint(a).filled()) {
            Rcpp::stop("Point on line %d (%f, %f) not on filled analysis space", i, row[0], row[1]);
        }

        if (!pointMapPtr->includes(b) || !pointMapPtr->getPoint(b).filled()) {
            Rcpp::stop("Point on line %d (%f, %f) not on filled analysis space", i, row[2], row[3]);
        }

        if (!pointMapPtr->isPixelMerged(a)) {
            Rcpp::stop("Point on line %d (%f, %f) is not part of a link", i, row[0], row[1]);
        }

        if (!pointMapPtr->isPixelMerged(b)) {
            Rcpp::stop("Point on line %d (%f, %f) is not part of a link", i, row[2], row[3]);
        }

        pointMapPtr->unmergePixel(a);
    }
    return Rcpp::List::create(Rcpp::Named("completed") = true,
                              Rcpp::Named("newAttributes") = std::vector<std::string>(),
                              Rcpp::Named("newProperties") = std::vector<std::string>(),
                              Rcpp::Named("mapPtr") = pointMapPtr);
}

// [[Rcpp::export("Rcpp_PointMap_unlinkRefs")]]
Rcpp::List pointMapUnlinkRefs(Rcpp::XPtr<PointMap> pointMapPtr, Rcpp::IntegerMatrix refs,
                              const Rcpp::Nullable<bool> copyMapNV = R_NilValue) {
    if (refs.cols() != 2) {
        Rcpp::stop("The refs matrix needs to have 2 columns: fromRef, toRef");
    }
    auto copyMap = NullableValue::get(copyMapNV, true);
    if (copyMap) {
        auto prevPointMap = pointMapPtr;
        const auto &prevRegion = prevPointMap->getRegion();
        pointMapPtr = Rcpp::XPtr(new PointMap(prevRegion));
        pointMapPtr->copy(*prevPointMap, true, true);
    }
    for (int i = 0; i < refs.rows(); ++i) {
        const Rcpp::IntegerMatrix::Row &row = refs(i, Rcpp::_);

        const PixelRef a(row[0]);
        const PixelRef b(row[1]);

        if (!pointMapPtr->includes(a) || !pointMapPtr->getPoint(a).filled()) {
            Rcpp::stop("Point on line %d (%d) not on filled analysis space", i, row[0]);
        }

        if (!pointMapPtr->includes(b) || !pointMapPtr->getPoint(b).filled()) {
            Rcpp::stop("Point on line %d (%d) not on filled analysis space", i, row[1]);
        }

        if (!pointMapPtr->isPixelMerged(a)) {
            Rcpp::stop("Point on line %d (%f, %f) is not part of a link", i, row[0], row[1]);
        }

        if (!pointMapPtr->isPixelMerged(b)) {
            Rcpp::stop("Point on line %d (%f, %f) is not part of a link", i, row[2], row[3]);
        }

        pointMapPtr->unmergePixel(a);
    }
    return Rcpp::List::create(Rcpp::Named("completed") = true,
                              Rcpp::Named("newAttributes") = std::vector<std::string>(),
                              Rcpp::Named("newProperties") = std::vector<std::string>(),
                              Rcpp::Named("mapPtr") = pointMapPtr);
}
