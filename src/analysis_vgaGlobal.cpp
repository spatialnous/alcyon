// SPDX-FileCopyrightText: 2024-2025 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "salalib/latticemap.hpp"
#include "salalib/vgamodules/vgaangular.hpp"
#include "salalib/vgamodules/vgaangularopenmp.hpp"
#include "salalib/vgamodules/vgametric.hpp"
#include "salalib/vgamodules/vgametricopenmp.hpp"
#include "salalib/vgamodules/vgavisualglobal.hpp"
#include "salalib/vgamodules/vgavisualglobalopenmp.hpp"

#include "helper_nullablevalue.hpp"
#include "helper_runAnalysis.hpp"

#include "communicator.hpp"

#include <Rcpp.h>

// [[Rcpp::plugins(openmp)]]

// [[Rcpp::export("Rcpp_VGA_angular")]]
Rcpp::List vgaAngular(Rcpp::XPtr<LatticeMap> mapPtr, double radius,
                      const Rcpp::Nullable<bool> gatesOnlyNV = R_NilValue,
                      const Rcpp::Nullable<int> nthreadsNV = R_NilValue,
                      const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
                      const Rcpp::Nullable<bool> progressNV = R_NilValue) {
    auto gatesOnly = NullableValue::get(gatesOnlyNV, false);
    auto nthreads = NullableValue::get(nthreadsNV, 1);
    auto copyMap = NullableValue::get(copyMapNV, true);
    auto progress = NullableValue::get(progressNV, false);

    mapPtr = RcppRunner::copyMapWithRegion(mapPtr, copyMap);

    return RcppRunner::runAnalysis<LatticeMap>(
        mapPtr, progress,
        [&nthreads, &radius, &gatesOnly](Communicator *comm, Rcpp::XPtr<LatticeMap> mapPtr) {
            AnalysisResult analysisResult;
            if (nthreads == 1) {
                // original algorithm
                auto analysis = VGAAngular(*mapPtr, radius, gatesOnly);
                analysisResult = analysis.run(comm);
                analysis.copyResultToMap(analysisResult.getAttributes(),
                                         std::move(analysisResult.getAttributeData()), *mapPtr,
                                         analysisResult.columnStats);
            } else {
                // openmp algorithm
                auto analysis = VGAAngularOpenMP(
                    *mapPtr, radius, gatesOnly,
                    nthreads == 0 ? std::nullopt : std::make_optional(nthreads), true);
                analysisResult = analysis.run(comm);
                analysis.copyResultToMap(analysisResult.getAttributes(),
                                         std::move(analysisResult.getAttributeData()), *mapPtr,
                                         analysisResult.columnStats);
            }
            return analysisResult;
        });
}

// [[Rcpp::export("Rcpp_VGA_metric")]]
Rcpp::List vgaMetric(Rcpp::XPtr<LatticeMap> mapPtr, double radius,
                     const Rcpp::Nullable<bool> gatesOnlyNV = R_NilValue,
                     const Rcpp::Nullable<int> nthreadsNV = R_NilValue,
                     const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
                     const Rcpp::Nullable<bool> progressNV = R_NilValue) {
    if (radius != -1.0 && radius <= 0) {
        Rcpp::stop("Radius for metric vga must be n (-1) for the whole range or a "
                   "positive number. Got %d",
                   radius);
    }
    auto gatesOnly = NullableValue::get(gatesOnlyNV, false);
    auto nthreads = NullableValue::get(nthreadsNV, 1);
    auto copyMap = NullableValue::get(copyMapNV, true);
    auto progress = NullableValue::get(progressNV, false);

    mapPtr = RcppRunner::copyMapWithRegion(mapPtr, copyMap);

    return RcppRunner::runAnalysis<LatticeMap>(
        mapPtr, progress,
        [&nthreads, &radius, &gatesOnly](Communicator *comm, Rcpp::XPtr<LatticeMap> &mapPtr) {
            AnalysisResult analysisResult;
            if (nthreads == 1) {
                // original algorithm
                auto analysis = VGAMetric(*mapPtr, radius, gatesOnly);
                analysisResult = analysis.run(comm);
                analysis.copyResultToMap(analysisResult.getAttributes(),
                                         std::move(analysisResult.getAttributeData()), *mapPtr,
                                         analysisResult.columnStats);
            } else {
                // openmp algorithm
                auto analysis = VGAMetricOpenMP(
                    *mapPtr, radius, gatesOnly,
                    nthreads == 0 ? std::nullopt : std::make_optional(nthreads), true);
                analysisResult = analysis.run(comm);
                analysis.copyResultToMap(analysisResult.getAttributes(),
                                         std::move(analysisResult.getAttributeData()), *mapPtr,
                                         analysisResult.columnStats);
            }
            return analysisResult;
        });
}

// [[Rcpp::export("Rcpp_VGA_visualGlobal")]]
Rcpp::List vgaVisualGlobal(Rcpp::XPtr<LatticeMap> mapPtr, int radius,
                           const Rcpp::Nullable<bool> gatesOnlyNV = R_NilValue,
                           const Rcpp::Nullable<int> nthreadsNV = R_NilValue,
                           const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
                           const Rcpp::Nullable<bool> progressNV = R_NilValue) {
    if (radius != -1 && (radius < 1 || radius > 99)) {
        Rcpp::stop("Radius for visibility analysis must be n (-1) for the whole "
                   "range or an integer between 1 and 99 inclusive. Got %i",
                   radius);
    }
    auto gatesOnly = NullableValue::get(gatesOnlyNV, false);
    auto nthreads = NullableValue::get(nthreadsNV, 1);
    auto copyMap = NullableValue::get(copyMapNV, true);
    auto progress = NullableValue::get(progressNV, false);

    mapPtr = RcppRunner::copyMapWithRegion(mapPtr, copyMap);

    return RcppRunner::runAnalysis<LatticeMap>(
        mapPtr, progress,
        [&nthreads, &radius, &gatesOnly](Communicator *comm, Rcpp::XPtr<LatticeMap> mapPtr) {
            AnalysisResult analysisResult;
            if (nthreads == 1) {
                // original algorithm
                auto analysis = VGAVisualGlobal(*mapPtr, radius, gatesOnly);
                analysisResult = analysis.run(comm);
                analysis.copyResultToMap(analysisResult.getAttributes(),
                                         std::move(analysisResult.getAttributeData()), *mapPtr,
                                         analysisResult.columnStats);
            } else {
                // openmp algorithm
                analysisResult = VGAVisualGlobalOpenMP(*mapPtr, radius, gatesOnly,
                                                       nthreads == 0 ? std::nullopt
                                                                     : std::make_optional(nthreads),
                                                       true)
                                     .run(comm);
            }
            return analysisResult;
        });
}
