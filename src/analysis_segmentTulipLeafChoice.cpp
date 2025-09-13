// SPDX-FileCopyrightText: 2024-2025 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "salalib/shapegraph.hpp"

#include "salalib/radiustype.hpp"
#include "salalib/segmmodules/segmtulipleafchoice.hpp"

#include "communicator.hpp"
#include "enum_TraversalType.hpp"
#include "helper_enum.hpp"
#include "helper_nullablevalue.hpp"
#include "helper_runAnalysis.hpp"

#include <Rcpp.h>

// [[Rcpp::export("Rcpp_runSegmentTulipLeafChoice")]]
Rcpp::List
runSegmentTulipLeafChoice(Rcpp::XPtr<ShapeGraph> mapPtr, const Rcpp::NumericVector radii,
                          const int radiusStepType,
                          const Rcpp::Nullable<std::string> weightedMeasureColNameNV = R_NilValue,
                          const Rcpp::Nullable<int> tulipBinsNV = R_NilValue,
                          const Rcpp::Nullable<bool> selOnlyNV = R_NilValue,
                          const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
                          const Rcpp::Nullable<bool> verboseNV = R_NilValue,
                          const Rcpp::Nullable<bool> progressNV = R_NilValue) {

    auto weightedMeasureColName = NullableValue::getOptional(weightedMeasureColNameNV);
    auto tulipBins = NullableValue::get(tulipBinsNV, 0);
    // TODO: Instead of expecting things to be selected,
    // provide indices to select
    // auto selOnly = NullableValue::get(selOnlyNV, false);
    auto copyMap = NullableValue::get(copyMapNV, true);
    auto verbose = NullableValue::get(verboseNV, false);
    auto progress = NullableValue::get(progressNV, false);

    auto radiusTraversalType = getAsValidEnum<TraversalType>(radiusStepType);

    mapPtr = RcppRunner::copyMap(mapPtr, copyMap);

    return RcppRunner::runAnalysis<ShapeGraph>(
        mapPtr, progress,
        [&radii, &radiusTraversalType, &weightedMeasureColName, &tulipBins,
         &verbose](Communicator *comm, Rcpp::XPtr<ShapeGraph> mapPtr) {
            if (verbose) {
                Rcpp::Rcout << "Running segment analysis... " << '\n';
            }

            std::set<double> radius_set;
            radius_set.insert(radii.begin(), radii.end());

            int weightedMeasureColIdx = -1;

            if (weightedMeasureColName.has_value()) {
                const AttributeTable &table = mapPtr->getAttributeTable();
                for (size_t i = 0; i < table.getNumColumns(); i++) {
                    if (weightedMeasureColName == table.getColumnName(i).c_str()) {
                        weightedMeasureColIdx = static_cast<int>(i);
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
            if (tulipBins > 0) {
                analysisResult = SegmentTulipLeafChoice(radius_set, std::nullopt, tulipBins,
                                                        weightedMeasureColIdx, radiusType)
                                     .run(comm, *mapPtr, false /* interactive */);
            } else {
                Rcpp::stop("Tulip bins can not be 0");
            }
            if (verbose) {
                Rcpp::Rcout << "ok" << '\n';
            }
            return analysisResult;
        });
}
