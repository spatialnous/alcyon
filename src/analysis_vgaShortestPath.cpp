// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "salalib/pointmap.h"
#include "salalib/vgamodules/vgaangularshortestpath.h"
#include "salalib/vgamodules/vgametricshortestpath.h"
#include "salalib/vgamodules/vgametricshortestpathtomany.h"
#include "salalib/vgamodules/vgavisualshortestpath.h"

#include "helper_nullablevalue.h"
#include "helper_runAnalysis.h"

#include "communicator.h"

#include <Rcpp.h>

// [[Rcpp::export("Rcpp_VGA_visualShortestPath")]]
Rcpp::List vgaVisualShortestPath(Rcpp::XPtr<PointMap> mapPtr, Rcpp::NumericMatrix origPoints,
                                 Rcpp::NumericMatrix destPoints,
                                 const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
                                 const Rcpp::Nullable<bool> verboseNV = R_NilValue,
                                 const Rcpp::Nullable<bool> progressNV = R_NilValue) {
    auto copyMap = NullableValue::get(copyMapNV, true);
    auto verbose = NullableValue::get(verboseNV, false);
    auto progress = NullableValue::get(progressNV, false);

    if (origPoints.rows() != destPoints.rows()) {
        Rcpp::stop("Different number of origins and destinations provided (%d %d).",
                   origPoints.rows(), destPoints.rows());
    }

    mapPtr = RcppRunner::copyMapWithRegion(mapPtr, copyMap);

    return RcppRunner::runAnalysis<PointMap>(
        mapPtr, progress,
        [&origPoints, &destPoints](Communicator *comm, Rcpp::XPtr<PointMap> mapPtr) {
            std::set<PixelRef> origins;
            for (int r = 0; r < origPoints.rows(); ++r) {
                auto coordRow = origPoints.row(r);
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
            std::set<PixelRef> destinations;
            for (int r = 0; r < destPoints.rows(); ++r) {
                auto coordRow = destPoints.row(r);
                Point2f p(coordRow[0], coordRow[1]);
                auto pixref = mapPtr->pixelate(p);
                if (!mapPtr->includes(pixref)) {
                    Rcpp::stop("Destination point (%d %d) outside of target pointmap region.", p.x,
                               p.y);
                }
                if (!mapPtr->getPoint(pixref).filled()) {
                    Rcpp::stop("Destination point (%d %d) not pointing to a filled cell.", p.x,
                               p.y);
                }
                destinations.insert(pixref);
            }

            AppendableAnalysisResult allAnalysisResult;
            auto destIt = destinations.begin();
            for (auto &origin : origins) {
                auto analysis = VGAVisualShortestPath(*mapPtr, origin, *destIt);
                auto analysisResult = analysis.run(comm);
                analysis.copyResultToMap(analysisResult.getAttributes(),
                                         std::move(analysisResult.getAttributeData()), *mapPtr,
                                         analysisResult.columnStats);
                allAnalysisResult.append(analysisResult);
                destIt++;
            }
            return allAnalysisResult;
        });
}

// [[Rcpp::export("Rcpp_VGA_metricShortestPath")]]
Rcpp::List vgaMetricShortestPath(Rcpp::XPtr<PointMap> mapPtr, Rcpp::NumericMatrix origPoints,
                                 Rcpp::NumericMatrix destPoints,
                                 const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
                                 const Rcpp::Nullable<bool> verboseNV = R_NilValue,
                                 const Rcpp::Nullable<bool> progressNV = R_NilValue) {
    auto copyMap = NullableValue::get(copyMapNV, true);
    auto verbose = NullableValue::get(verboseNV, false);
    auto progress = NullableValue::get(progressNV, false);

    if (origPoints.rows() != destPoints.rows()) {
        Rcpp::stop("Different number of origins and destinations provided (%d %d).",
                   origPoints.rows(), destPoints.rows());
    }

    mapPtr = RcppRunner::copyMapWithRegion(mapPtr, copyMap);

    return RcppRunner::runAnalysis<PointMap>(
        mapPtr, progress,
        [&origPoints, &destPoints](Communicator *comm, Rcpp::XPtr<PointMap> mapPtr) {
            std::set<PixelRef> origins;
            for (int r = 0; r < origPoints.rows(); ++r) {
                auto coordRow = origPoints.row(r);
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
            std::set<PixelRef> destinations;
            for (int r = 0; r < destPoints.rows(); ++r) {
                auto coordRow = destPoints.row(r);
                Point2f p(coordRow[0], coordRow[1]);
                auto pixref = mapPtr->pixelate(p);
                if (!mapPtr->includes(pixref)) {
                    Rcpp::stop("Destination point (%d %d) outside of target pointmap region.", p.x,
                               p.y);
                }
                if (!mapPtr->getPoint(pixref).filled()) {
                    Rcpp::stop("Destination point (%d %d) not pointing to a filled cell.", p.x,
                               p.y);
                }
                destinations.insert(pixref);
            }

            AppendableAnalysisResult allAnalysisResult;
            auto destIt = destinations.begin();
            for (auto &origin : origins) {
                auto analysis = VGAMetricShortestPath(*mapPtr, std::set<PixelRef>{origin}, *destIt);
                auto analysisResult = analysis.run(comm);
                analysis.copyResultToMap(analysisResult.getAttributes(),
                                         std::move(analysisResult.getAttributeData()), *mapPtr,
                                         analysisResult.columnStats);
                allAnalysisResult.append(analysisResult);
                destIt++;
            }
            return allAnalysisResult;
        });
}

// [[Rcpp::export("Rcpp_VGA_angularShortestPath")]]
Rcpp::List vgaAngularShortestPath(Rcpp::XPtr<PointMap> mapPtr, Rcpp::NumericMatrix origPoints,
                                  Rcpp::NumericMatrix destPoints,
                                  const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
                                  const Rcpp::Nullable<bool> verboseNV = R_NilValue,
                                  const Rcpp::Nullable<bool> progressNV = R_NilValue) {
    auto copyMap = NullableValue::get(copyMapNV, true);
    auto verbose = NullableValue::get(verboseNV, false);
    auto progress = NullableValue::get(progressNV, false);

    if (origPoints.rows() != destPoints.rows()) {
        Rcpp::stop("Different number of origins and destinations provided (%d %d).",
                   origPoints.rows(), destPoints.rows());
    }

    mapPtr = RcppRunner::copyMapWithRegion(mapPtr, copyMap);

    return RcppRunner::runAnalysis<PointMap>(
        mapPtr, progress,
        [&origPoints, &destPoints](Communicator *comm, Rcpp::XPtr<PointMap> mapPtr) {
            std::set<PixelRef> origins;
            for (int r = 0; r < origPoints.rows(); ++r) {
                auto coordRow = origPoints.row(r);
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
            std::set<PixelRef> destinations;
            for (int r = 0; r < destPoints.rows(); ++r) {
                auto coordRow = destPoints.row(r);
                Point2f p(coordRow[0], coordRow[1]);
                auto pixref = mapPtr->pixelate(p);
                if (!mapPtr->includes(pixref)) {
                    Rcpp::stop("Destination point (%d %d) outside of target pointmap region.", p.x,
                               p.y);
                }
                if (!mapPtr->getPoint(pixref).filled()) {
                    Rcpp::stop("Destination point (%d %d) not pointing to a filled cell.", p.x,
                               p.y);
                }
                destinations.insert(pixref);
            }

            AppendableAnalysisResult allAnalysisResult;
            auto destIt = destinations.begin();
            for (auto &origin : origins) {
                auto analysis = VGAAngularShortestPath(*mapPtr, origin, *destIt);
                auto analysisResult = analysis.run(comm);
                analysis.copyResultToMap(analysisResult.getAttributes(),
                                         std::move(analysisResult.getAttributeData()), *mapPtr,
                                         analysisResult.columnStats);
                allAnalysisResult.append(analysisResult);
                destIt++;
            }
            return allAnalysisResult;
        });
}
