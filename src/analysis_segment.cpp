// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "salalib/shapegraph.h"

#include "salalib/radiustype.h"
#include "salalib/segmmodules/segmangular.h"
#include "salalib/segmmodules/segmmetric.h"
#include "salalib/segmmodules/segmtopological.h"
#include "salalib/segmmodules/segmtulip.h"

#include "communicator.h"
#include "enum_TraversalType.h"
#include "helper_enum.h"
#include "helper_nullablevalue.h"
#include "helper_runAnalysis.h"

#include <Rcpp.h>

// [[Rcpp::export("Rcpp_runSegmentAnalysis")]]
Rcpp::List
runSegmentAnalysis(Rcpp::XPtr<ShapeGraph> mapPtr, const Rcpp::NumericVector radii,
                   const int radiusStepType, const int analysisStepType,
                   const Rcpp::Nullable<std::string> weightedMeasureColNameNV = R_NilValue,
                   const Rcpp::Nullable<bool> includeChoiceNV = R_NilValue,
                   const Rcpp::Nullable<int> tulipBinsNV = R_NilValue,
                   const Rcpp::Nullable<bool> selOnlyNV = R_NilValue,
                   const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
                   const Rcpp::Nullable<bool> verboseNV = R_NilValue,
                   const Rcpp::Nullable<bool> progressNV = R_NilValue) {

    auto weightedMeasureColName = NullableValue::getOptional(weightedMeasureColNameNV);
    auto includeChoice = NullableValue::get(includeChoiceNV, false);
    auto tulipBins = NullableValue::get(tulipBinsNV, 0);
    // TODO: Instead of expecting things to be selected,
    // provide indices to select
    auto selOnly = NullableValue::get(selOnlyNV, false);
    auto copyMap = NullableValue::get(copyMapNV, true);
    auto verbose = NullableValue::get(verboseNV, false);
    auto progress = NullableValue::get(progressNV, false);

    auto radiusTraversalType = getAsValidEnum<TraversalType>(radiusStepType);
    auto analysisTraversalType = getAsValidEnum<TraversalType>(analysisStepType);

    mapPtr = RcppRunner::copyMap(mapPtr, copyMap);

    return RcppRunner::runAnalysis<ShapeGraph>(
        mapPtr, progress,
        [&radii, &radiusTraversalType, &analysisTraversalType, &includeChoice,
         &weightedMeasureColName, &tulipBins,
         &verbose](Communicator *comm, Rcpp::XPtr<ShapeGraph> mapPtr) {
            if (verbose) {
                Rcpp::Rcout << "Running segment analysis... " << '\n';
            }

            std::set<double> radius_set;
            radius_set.insert(radii.begin(), radii.end());

            int weightedMeasureColIdx = -1;

            if (weightedMeasureColName.has_value()) {
                const AttributeTable &table = mapPtr->getAttributeTable();
                for (int i = 0; i < table.getNumColumns(); i++) {
                    if (weightedMeasureColName == table.getColumnName(i).c_str()) {
                        weightedMeasureColIdx = i;
                    }
                }
                if (weightedMeasureColIdx == -1) {
                    Rcpp::stop("Given attribute (" + weightedMeasureColName.value() +
                               ") does not exist in " + "currently selected map");
                }
            }

            RadiusType radiusType = RadiusType::NONE;
            std::map<double, std::string> radiusSuffixes;
            radiusSuffixes[-1] = "";

            switch (radiusTraversalType) {
            case TraversalType::Topological: {
                radiusType = RadiusType::TOPOLOGICAL;
                for (auto radius : radii) {
                    if (radius != -1) {
                        radiusSuffixes[radius] = " R" + std::to_string(int(radius));
                    }
                }
                break;
            }
            case TraversalType::Metric: {
                radiusType = RadiusType::METRIC;
                for (auto radius : radii) {
                    if (radius != -1) {
                        radiusSuffixes[radius] = " R" + std::to_string(radius) + " metric";
                    }
                }
                break;
            }
            case TraversalType::Angular: {
                radiusType = RadiusType::ANGULAR;
                for (auto radius : radii) {
                    if (radius != -1) {
                        radiusSuffixes[radius] = " R" + std::to_string(radius);
                    }
                }
                break;
            }
            case TraversalType::None: {
                Rcpp::stop("No radius analysis type given");
            }
            }

            AnalysisResult analysisResult;
            switch (analysisTraversalType) {
            case TraversalType::Angular: {
                if (tulipBins > 0) {
                    analysisResult = SegmentTulip(radius_set, std::nullopt, tulipBins,
                                                  weightedMeasureColIdx, radiusType, includeChoice)
                                         .run(comm, *mapPtr, false /* interactive */);
                } else {
                    analysisResult =
                        SegmentAngular(radius_set).run(comm, *mapPtr, false /* unused */);
                }
                break;
            }
            case TraversalType::Topological: {
                bool first = true;
                for (auto radius : radius_set) {
                    auto radiusAnalysisResult = SegmentTopological(radius, std::nullopt)
                                                    .run(comm, *mapPtr, false /* unused */);
                    if (first) {
                        analysisResult.completed = radiusAnalysisResult.completed;
                        first = false;
                    } else {
                        analysisResult.completed =
                            analysisResult.completed & radiusAnalysisResult.completed;
                    }
                    for (auto column : radiusAnalysisResult.getAttributes())
                        analysisResult.addAttribute(column);
                }
                break;
            }
            case TraversalType::Metric: {
                bool first = true;
                for (auto radius : radius_set) {
                    auto radiusAnalysisResult =
                        SegmentMetric(radius, std::nullopt).run(comm, *mapPtr, false /* unused */);
                    if (first) {
                        analysisResult.completed = radiusAnalysisResult.completed;
                        first = false;
                    } else {
                        analysisResult.completed =
                            analysisResult.completed & radiusAnalysisResult.completed;
                    }
                    for (auto column : radiusAnalysisResult.getAttributes())
                        analysisResult.addAttribute(column);
                }
                break;
            }
            case TraversalType::None: {
                Rcpp::stop("No segment analysis type given");
            }
            }
            if (verbose) {
                Rcpp::Rcout << "ok" << '\n';
            }
            return analysisResult;
        });
}
