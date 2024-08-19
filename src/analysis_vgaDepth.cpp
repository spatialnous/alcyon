// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "salalib/pointmap.h"
#include "salalib/vgamodules/vgaangulardepth.h"
#include "salalib/vgamodules/vgametricdepth.h"
#include "salalib/vgamodules/vgavisualglobaldepth.h"

#include "helper_nullablevalue.h"
#include "helper_runAnalysis.h"

#include "communicator.h"

#include <Rcpp.h>

// [[Rcpp::export("Rcpp_VGA_visualDepth")]]
Rcpp::List vgaVisualDepth(Rcpp::XPtr<PointMap> mapPtr, Rcpp::NumericMatrix stepDepthPoints,
                          const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
                          const Rcpp::Nullable<bool> progressNV = R_NilValue) {
    auto copyMap = NullableValue::get(copyMapNV, true);
    auto progress = NullableValue::get(progressNV, false);

    mapPtr = RcppRunner::copyMapWithRegion(mapPtr, copyMap);

    return RcppRunner::runAnalysis<PointMap>(
        mapPtr, progress, [&stepDepthPoints](Communicator *comm, Rcpp::XPtr<PointMap> mapPtr) {
            std::set<PixelRef> origins;
            for (int r = 0; r < stepDepthPoints.rows(); ++r) {
                auto coordRow = stepDepthPoints.row(r);
                Point2f p(coordRow[0], coordRow[1]);
                auto pixref = mapPtr->pixelate(p);
                if (!mapPtr->includes(pixref)) {
                    Rcpp::stop("Origin point (%d %d) outside of target pointmap region.", p.x, p.y);
                }
                if (!mapPtr->getPoint(pixref).filled()) {
                    Rcpp::stop("Origin point (%d %d) not pointing to a filled cell.", p.x, p.y);
                }
                origins.insert(pixref);
            }

            auto analysis = VGAVisualGlobalDepth(*mapPtr, origins);
            auto analysisResult = analysis.run(comm);
            analysis.copyResultToMap(analysisResult.getAttributes(),
                                     std::move(analysisResult.getAttributeData()), *mapPtr,
                                     analysisResult.columnStats);
            return analysisResult;
        });
}

// [[Rcpp::export("Rcpp_VGA_metricDepth")]]
Rcpp::List vgaMetricDepth(Rcpp::XPtr<PointMap> mapPtr, Rcpp::NumericMatrix stepDepthPoints,
                          const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
                          const Rcpp::Nullable<bool> progressNV = R_NilValue) {
    auto copyMap = NullableValue::get(copyMapNV, true);
    auto progress = NullableValue::get(progressNV, false);

    mapPtr = RcppRunner::copyMapWithRegion(mapPtr, copyMap);

    return RcppRunner::runAnalysis<PointMap>(
        mapPtr, progress, [&stepDepthPoints](Communicator *comm, Rcpp::XPtr<PointMap> mapPtr) {
            std::set<PixelRef> origins;
            for (int r = 0; r < stepDepthPoints.rows(); ++r) {
                auto coordRow = stepDepthPoints.row(r);
                Point2f p(coordRow[0], coordRow[1]);
                auto pixref = mapPtr->pixelate(p);
                if (!mapPtr->includes(pixref)) {
                    Rcpp::stop("Origin point (%d %d) outside of target pointmap region.", p.x, p.y);
                }
                if (!mapPtr->getPoint(pixref).filled()) {
                    Rcpp::stop("Origin point (%d %d) not pointing to a filled cell.", p.x, p.y);
                }
                origins.insert(pixref);
            }

            auto analysis = VGAMetricDepth(*mapPtr, origins);
            auto analysisResult = analysis.run(comm);
            analysis.copyResultToMap(analysisResult.getAttributes(),
                                     std::move(analysisResult.getAttributeData()), *mapPtr,
                                     analysisResult.columnStats);
            return analysisResult;
        });
}

// [[Rcpp::export("Rcpp_VGA_angularDepth")]]
Rcpp::List vgaAngularDepth(Rcpp::XPtr<PointMap> mapPtr, Rcpp::NumericMatrix stepDepthPoints,
                           const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
                           const Rcpp::Nullable<bool> progressNV = R_NilValue) {
    auto copyMap = NullableValue::get(copyMapNV, true);
    auto progress = NullableValue::get(progressNV, false);

    mapPtr = RcppRunner::copyMapWithRegion(mapPtr, copyMap);

    return RcppRunner::runAnalysis<PointMap>(
        mapPtr, progress, [&stepDepthPoints](Communicator *comm, Rcpp::XPtr<PointMap> mapPtr) {
            std::set<PixelRef> origins;
            for (int r = 0; r < stepDepthPoints.rows(); ++r) {
                auto coordRow = stepDepthPoints.row(r);
                Point2f p(coordRow[0], coordRow[1]);
                auto pixref = mapPtr->pixelate(p);
                if (!mapPtr->includes(pixref)) {
                    Rcpp::stop("Origin point (%d %d) outside of target pointmap region.", p.x, p.y);
                }
                if (!mapPtr->getPoint(pixref).filled()) {
                    Rcpp::stop("Origin point (%d %d) not pointing to a filled cell.", p.x, p.y);
                }
                origins.insert(pixref);
            }
            auto analysis = VGAAngularDepth(*mapPtr, origins);
            auto analysisResult = analysis.run(comm);
            analysis.copyResultToMap(analysisResult.getAttributes(),
                                     std::move(analysisResult.getAttributeData()), *mapPtr,
                                     analysisResult.columnStats);
            return analysisResult;
        });
}
