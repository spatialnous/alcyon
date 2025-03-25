// SPDX-FileCopyrightText: 2024-2025 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "salalib/shapegraph.hpp"

#include "salalib/radiustype.hpp"
#include "salalib/segmmodules/segmmetricshortestpath.hpp"
#include "salalib/segmmodules/segmtopologicalshortestpath.hpp"
#include "salalib/segmmodules/segmtulipshortestpath.hpp"

#include "communicator.hpp"
#include "enum_TraversalType.hpp"
#include "helper_enum.hpp"
#include "helper_nullablevalue.hpp"
#include "helper_runAnalysis.hpp"

#include <Rcpp.h>

// [[Rcpp::export("Rcpp_segmentShortestPath")]]
Rcpp::List segmentShortestPath(Rcpp::XPtr<ShapeGraph> mapPtr, const int stepType,
                               Rcpp::NumericMatrix origPoints, Rcpp::NumericMatrix destPoints,
                               const Rcpp::Nullable<int> tulipBinsNV = R_NilValue,
                               const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
                               const Rcpp::Nullable<bool> verboseNV = R_NilValue,
                               const Rcpp::Nullable<bool> progressNV = R_NilValue) {
    auto tulipBins = NullableValue::get(tulipBinsNV, 0);
    auto copyMap = NullableValue::get(copyMapNV, true);
    auto verbose = NullableValue::get(verboseNV, false);
    auto progress = NullableValue::get(progressNV, false);

    if (origPoints.rows() != destPoints.rows()) {
        Rcpp::stop("Different number of origins and destinations provided (%d %d).",
                   origPoints.rows(), destPoints.rows());
    }

    auto traversalStepType = getAsValidEnum<TraversalType>(stepType);

    mapPtr = RcppRunner::copyMap(mapPtr, copyMap);

    return RcppRunner::runAnalysis<ShapeGraph>(
        mapPtr, progress,
        [&traversalStepType, &origPoints, &destPoints, &tulipBins,
         &verbose](Communicator *comm, Rcpp::XPtr<ShapeGraph> mapPtr) {
            if (verbose) {
                Rcpp::Rcout << "ok\nSelecting cells... " << '\n';
            }

            std::set<int> origins;
            for (int r = 0; r < origPoints.rows(); ++r) {
                auto coordRow = origPoints.row(r);
                Point2f p(coordRow[0], coordRow[1]);
                auto graphRegion = mapPtr->getRegion();
                if (!graphRegion.contains(p)) {
                    Rcpp::stop("Point outside of target region");
                }
                Region4f region(p, p);
                origins.insert(mapPtr->getShapesInRegion(region).begin()->first);
            }

            std::set<int> destinations;
            for (int r = 0; r < destPoints.rows(); ++r) {
                auto coordRow = destPoints.row(r);
                Point2f p(coordRow[0], coordRow[1]);
                auto graphRegion = mapPtr->getRegion();
                if (!graphRegion.contains(p)) {
                    Rcpp::stop("Point outside of target region");
                }
                Region4f region(p, p);
                destinations.insert(mapPtr->getShapesInRegion(region).begin()->first);
            }

            if (verbose) {
                Rcpp::Rcout << "ok\nCalculating shortest-paths.. " << '\n';
            }

            AppendableAnalysisResult analysisResult;
            auto destIt = destinations.begin();
            for (auto &origin : origins) {
                switch (traversalStepType) {
                case TraversalType::Angular:
                    if (tulipBins != 0) {
                        analysisResult.append(
                            SegmentTulipShortestPath(*mapPtr, tulipBins, origin, *destIt)
                                .run(comm));
                    } else {
                        // full angular was never created as a step-function
                        // do normal tulip
                        Rcpp::stop("Full angular depth not implemented, "
                                   "provide tulipBins for quantization");
                    }
                    break;
                case TraversalType::Metric: {
                    analysisResult.append(
                        SegmentMetricShortestPath(*mapPtr, origin, *destIt).run(comm));
                    break;
                }
                case TraversalType::Topological: {
                    analysisResult.append(
                        SegmentTopologicalShortestPath(*mapPtr, origin, *destIt).run(comm));
                    break;
                }
                case TraversalType::None: {
                    Rcpp::stop("Error, unsupported step type");
                }
                }
                destIt++;
            }
            return analysisResult;
        });
}
