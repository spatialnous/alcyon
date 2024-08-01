// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "salalib/shapegraph.h"

#include "salalib/radiustype.h"
#include "salalib/segmmodules/segmmetricpd.h"
#include "salalib/segmmodules/segmtopologicalpd.h"
#include "salalib/segmmodules/segmtulipdepth.h"

#include "communicator.h"
#include "enum_TraversalType.h"
#include "helper_enum.h"
#include "helper_nullablevalue.h"
#include "helper_runAnalysis.h"

#include <Rcpp.h>

// [[Rcpp::export("Rcpp_segmentStepDepth")]]
Rcpp::List segmentStepDepth(Rcpp::XPtr<ShapeGraph> mapPtr, const int stepType,
                            const std::vector<double> stepDepthPointsX,
                            const std::vector<double> stepDepthPointsY,
                            const Rcpp::Nullable<int> tulipBinsNV = R_NilValue,
                            const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
                            const Rcpp::Nullable<bool> verboseNV = R_NilValue,
                            const Rcpp::Nullable<bool> progressNV = R_NilValue) {
    auto tulipBins = NullableValue::get(tulipBinsNV, 0);
    auto copyMap = NullableValue::get(copyMapNV, true);
    auto verbose = NullableValue::get(verboseNV, false);
    auto progress = NullableValue::get(progressNV, false);

    auto traversalStepType = getAsValidEnum<TraversalType>(stepType);

    mapPtr = RcppRunner::copyMap(mapPtr, copyMap);

    return RcppRunner::runAnalysis<ShapeGraph>(
        mapPtr, progress,
        [&traversalStepType, &stepDepthPointsX, &stepDepthPointsY, &tulipBins,
         &verbose](Communicator *comm, Rcpp::XPtr<ShapeGraph> mapPtr) {
            if (verbose) {
                Rcpp::Rcout << "ok\nSelecting cells... " << '\n';
            }

            std::set<int> origins;
            for (int i = 0; i < stepDepthPointsX.size(); ++i) {
                Point2f p2f(stepDepthPointsX[i], stepDepthPointsY[i]);
                auto graphRegion = mapPtr->getRegion();
                if (!graphRegion.contains(p2f)) {
                    Rcpp::stop("Point outside of target region");
                }
                QtRegion r(p2f, p2f);
                origins.insert(mapPtr->getShapesInRegion(r).begin()->first);
            }

            if (verbose) {
                Rcpp::Rcout << "ok\nCalculating step-depth... " << '\n';
            }
            AnalysisResult analysisResult;
            switch (traversalStepType) {
            case TraversalType::Angular:
                if (tulipBins != 0) {
                    analysisResult = SegmentTulipDepth(tulipBins, origins)
                                         .run(comm, *mapPtr, false /* simple mode */
                                         );
                } else {
                    // full angular was never created as a step-function
                    // do normal tulip
                    Rcpp::stop("Full angular depth not implemented, "
                               "provide tulipBins for quantization");
                }
                break;
            case TraversalType::Metric: {
                analysisResult = SegmentMetricPD(origins).run(comm, *mapPtr, false /* simple mode */
                );
                break;
            }
            case TraversalType::Topological: {
                analysisResult =
                    SegmentTopologicalPD(origins).run(comm, *mapPtr, false /* simple mode */
                    );
                break;
            }
            case TraversalType::None: {
                Rcpp::stop("No traversal type has been set");
            }
            }
            return analysisResult;
        });
}
