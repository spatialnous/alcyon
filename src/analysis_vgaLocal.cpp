// SPDX-FileCopyrightText: 2024-2025 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "salalib/vgamodules/vgathroughvision.hpp"
#include "salalib/vgamodules/vgavisuallocal.hpp"
#include "salalib/vgamodules/vgavisuallocaladjmatrix.hpp"
#include "salalib/vgamodules/vgavisuallocalopenmp.hpp"

#include "enum_VGALocalAlgorithm.hpp"

#include "helper_nullablevalue.hpp"
#include "helper_runAnalysis.hpp"

#include "communicator.hpp"

#include <Rcpp.h>

// [[Rcpp::plugins(openmp)]]

// [[Rcpp::export("Rcpp_VGA_visualLocal")]]
Rcpp::List vgaVisualLocal(Rcpp::XPtr<LatticeMap> mapPtr,
                          const Rcpp::Nullable<bool> gatesOnlyNV = R_NilValue,
                          const Rcpp::Nullable<int> nthreadsNV = R_NilValue,
                          const Rcpp::Nullable<int> algorithmNV = R_NilValue,
                          const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
                          const Rcpp::Nullable<bool> progressNV = R_NilValue) {
    auto gatesOnly = NullableValue::get(gatesOnlyNV, false);
    auto nthreads = NullableValue::get(nthreadsNV, 1);
    auto algorithm = NullableValue::castIntGet(algorithmNV, VGALocalAlgorithm::Standard);
    auto copyMap = NullableValue::get(copyMapNV, true);
    auto progress = NullableValue::get(progressNV, false);

    if (nthreads < 0) {
        Rcpp::stop("Number of threads has to be >= 1 or 0 for all (" + std::to_string(nthreads) +
                   " provided)");
    }

    if (algorithm != VGALocalAlgorithm::Standard && algorithm != VGALocalAlgorithm::AdjacencyMatrix)
        Rcpp::stop("Unknown algorithm provided: " + std::to_string(static_cast<int>(algorithm)));

    mapPtr = RcppRunner::copyMapWithRegion(mapPtr, copyMap);

    return RcppRunner::runAnalysis<LatticeMap>(
        mapPtr, progress,
        [&nthreads, &algorithm, &gatesOnly](Communicator *comm, Rcpp::XPtr<LatticeMap> mapPtr) {
            AnalysisResult analysisResult;
            if (algorithm == VGALocalAlgorithm::Standard) {
                if (nthreads == 1) {
                    // original algorithm
                    auto analysis = VGAVisualLocal(*mapPtr, gatesOnly);
                    analysisResult = analysis.run(comm);
                    analysis.copyResultToMap(analysisResult.getAttributes(),
                                             std::move(analysisResult.getAttributeData()), *mapPtr,
                                             analysisResult.columnStats);
                } else {
                    // openmp algorithm
                    analysisResult =
                        VGAVisualLocalOpenMP(
                            *mapPtr, nthreads == 0 ? std::nullopt : std::make_optional(nthreads),
                            true)
                            .run(comm);
                }
            } else if (algorithm == VGALocalAlgorithm::AdjacencyMatrix) {
                // adjacency matrix algorithm
                analysisResult =
                    VGAVisualLocalAdjMatrix(
                        *mapPtr, gatesOnly,
                        nthreads == 0 ? std::nullopt : std::make_optional(nthreads), true)
                        .run(comm);
            }
            return analysisResult;
        });
}

// [[Rcpp::export("Rcpp_VGA_throughVision")]]
Rcpp::List vgaThroughVision(Rcpp::XPtr<LatticeMap> mapPtr,
                            const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
                            const Rcpp::Nullable<bool> progressNV = R_NilValue) {
    auto copyMap = NullableValue::get(copyMapNV, true);
    auto progress = NullableValue::get(progressNV, false);

    mapPtr = RcppRunner::copyMapWithRegion(mapPtr, copyMap);

    return RcppRunner::runAnalysis<LatticeMap>(
        mapPtr, progress, [](Communicator *comm, Rcpp::XPtr<LatticeMap> mapPtr) {
            auto analysis = VGAThroughVision(*mapPtr);
            auto analysisResult = analysis.run(comm);
            analysis.copyResultToMap(analysisResult.getAttributes(),
                                     std::move(analysisResult.getAttributeData()), *mapPtr,
                                     analysisResult.columnStats);
            return analysisResult;
        });
}
