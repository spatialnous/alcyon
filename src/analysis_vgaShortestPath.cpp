// SPDX-FileCopyrightText: 2024-2025 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "salalib/latticemap.hpp"
#include "salalib/vgamodules/vgaangularshortestpath.hpp"
#include "salalib/vgamodules/vgametricshortestpath.hpp"
#include "salalib/vgamodules/vgametricshortestpathtomany.hpp"
#include "salalib/vgamodules/vgavisualshortestpath.hpp"

#include "helper_nullablevalue.hpp"
#include "helper_runAnalysis.hpp"

#include "communicator.hpp"

#include <Rcpp.h>

// [[Rcpp::export("Rcpp_VGA_visualShortestPath")]]
Rcpp::List vgaVisualShortestPath(Rcpp::XPtr<LatticeMap> mapPtr, Rcpp::NumericMatrix origPoints,
                                 Rcpp::NumericMatrix destPoints,
                                 const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
                                 const Rcpp::Nullable<bool> verboseNV = R_NilValue,
                                 const Rcpp::Nullable<bool> progressNV = R_NilValue) {
    auto copyMap = NullableValue::get(copyMapNV, true);
    // auto verbose = NullableValue::get(verboseNV, false);
    auto progress = NullableValue::get(progressNV, false);

    if (origPoints.rows() != destPoints.rows()) {
        Rcpp::stop("Different number of origins and destinations provided (%d %d).",
                   origPoints.rows(), destPoints.rows());
    }

    mapPtr = RcppRunner::copyMapWithRegion(mapPtr, copyMap);

    return RcppRunner::runAnalysis<LatticeMap>(
        mapPtr, progress,
        [&origPoints, &destPoints](Communicator *comm, Rcpp::XPtr<LatticeMap> mapPtr) {
            std::set<PixelRef> origins;
            for (int r = 0; r < origPoints.rows(); ++r) {
                auto coordRow = origPoints.row(r);
                Point2f p(coordRow[0], coordRow[1]);
                auto pixref = mapPtr->pixelate(p);
                if (!mapPtr->includes(pixref)) {
                    Rcpp::stop("Origin point (%d %d) outside of target lattice map region.", p.x,
                               p.y);
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
                    Rcpp::stop("Destination point (%d %d) outside of target lattice map region.",
                               p.x, p.y);
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
Rcpp::List vgaMetricShortestPath(Rcpp::XPtr<LatticeMap> mapPtr, Rcpp::NumericMatrix origPoints,
                                 Rcpp::NumericMatrix destPoints,
                                 const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
                                 const Rcpp::Nullable<bool> verboseNV = R_NilValue,
                                 const Rcpp::Nullable<bool> progressNV = R_NilValue) {
    auto copyMap = NullableValue::get(copyMapNV, true);
    // auto verbose = NullableValue::get(verboseNV, false);
    auto progress = NullableValue::get(progressNV, false);

    if (origPoints.rows() != destPoints.rows()) {
        Rcpp::stop("Different number of origins and destinations provided (%d %d).",
                   origPoints.rows(), destPoints.rows());
    }

    mapPtr = RcppRunner::copyMapWithRegion(mapPtr, copyMap);

    return RcppRunner::runAnalysis<LatticeMap>(
        mapPtr, progress,
        [&origPoints, &destPoints](Communicator *comm, Rcpp::XPtr<LatticeMap> mapPtr) {
            std::set<PixelRef> origins;
            for (int r = 0; r < origPoints.rows(); ++r) {
                auto coordRow = origPoints.row(r);
                Point2f p(coordRow[0], coordRow[1]);
                auto pixref = mapPtr->pixelate(p);
                if (!mapPtr->includes(pixref)) {
                    Rcpp::stop("Origin point (%d %d) outside of target lattice map region.", p.x,
                               p.y);
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
                    Rcpp::stop("Destination point (%d %d) outside of target lattice map region.",
                               p.x, p.y);
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
Rcpp::List vgaAngularShortestPath(Rcpp::XPtr<LatticeMap> mapPtr, Rcpp::NumericMatrix origPoints,
                                  Rcpp::NumericMatrix destPoints,
                                  const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
                                  const Rcpp::Nullable<bool> verboseNV = R_NilValue,
                                  const Rcpp::Nullable<bool> progressNV = R_NilValue) {
    auto copyMap = NullableValue::get(copyMapNV, true);
    // auto verbose = NullableValue::get(verboseNV, false);
    auto progress = NullableValue::get(progressNV, false);

    if (origPoints.rows() != destPoints.rows()) {
        Rcpp::stop("Different number of origins and destinations provided (%d %d).",
                   origPoints.rows(), destPoints.rows());
    }

    mapPtr = RcppRunner::copyMapWithRegion(mapPtr, copyMap);

    return RcppRunner::runAnalysis<LatticeMap>(
        mapPtr, progress,
        [&origPoints, &destPoints](Communicator *comm, Rcpp::XPtr<LatticeMap> mapPtr) {
            std::set<PixelRef> origins;
            for (int r = 0; r < origPoints.rows(); ++r) {
                auto coordRow = origPoints.row(r);
                Point2f p(coordRow[0], coordRow[1]);
                auto pixref = mapPtr->pixelate(p);
                if (!mapPtr->includes(pixref)) {
                    Rcpp::stop("Origin point (%d %d) outside of target lattice map region.", p.x,
                               p.y);
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
                    Rcpp::stop("Destination point (%d %d) outside of target lattice map region.",
                               p.x, p.y);
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
