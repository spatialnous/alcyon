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
                          const Rcpp::Nullable<std::vector<int>> selectedOriginRefsNV = R_NilValue,
                          const Rcpp::Nullable<bool> recordSelLeafsNV = R_NilValue,
                          const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
                          const Rcpp::Nullable<bool> verboseNV = R_NilValue,
                          const Rcpp::Nullable<bool> progressNV = R_NilValue) {

    const auto weightedMeasureColName = NullableValue::getOptional(weightedMeasureColNameNV);
    const auto tulipBins = NullableValue::get(tulipBinsNV, 0);
    const auto selectedOriginRefs =
        selectedOriginRefsNV == R_NilValue ? std::nullopt : [&selectedOriginRefsNV]() {
            auto selectedOriginRefsVec =
                NullableValue::get(selectedOriginRefsNV, std::vector<int>());
            return std::make_optional(
                std::set<int>(selectedOriginRefsVec.begin(), selectedOriginRefsVec.end()));
        }();

    const auto recordSelLeafs = NullableValue::get(recordSelLeafsNV, true);
    const auto copyMap = NullableValue::get(copyMapNV, true);
    const auto verbose = NullableValue::get(verboseNV, false);
    const auto progress = NullableValue::get(progressNV, false);

    const auto radiusTraversalType = getAsValidEnum<TraversalType>(radiusStepType);

    mapPtr = RcppRunner::copyMap(mapPtr, copyMap);

    return RcppRunner::runAnalysis<ShapeGraph>(
        mapPtr, progress,
        [&radii, &radiusTraversalType, &weightedMeasureColName, &tulipBins, &selectedOriginRefs,
         &recordSelLeafs, &verbose](Communicator *comm, Rcpp::XPtr<ShapeGraph> mapPtr) {
            if (verbose) {
                Rcpp::Rcout << "Running segment analysis... " << '\n';
            }

            std::set<double> radiusSet;
            radiusSet.insert(radii.begin(), radii.end());

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
                analysisResult = SegmentTulipLeafChoice(radiusSet, selectedOriginRefs, tulipBins,
                                                        weightedMeasureColIdx, radiusType, -1, -1,
                                                        recordSelLeafs)
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
