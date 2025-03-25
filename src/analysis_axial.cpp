// SPDX-FileCopyrightText: 2024-2025 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "salalib/axialmodules/axialintegration.hpp"
#include "salalib/axialmodules/axiallocal.hpp"
#include "salalib/axialmodules/axialstepdepth.hpp"

#include "salalib/shapegraph.hpp"
#include "salalib/shapemap.hpp"

#include "communicator.hpp"
#include "enum_TraversalType.hpp"
#include "helper_enum.hpp"
#include "helper_nullablevalue.hpp"
#include "helper_runAnalysis.hpp"

#include <Rcpp.h>

// [[Rcpp::export("Rcpp_runAxialAnalysis")]]
Rcpp::List runAxialAnalysis(Rcpp::XPtr<ShapeGraph> mapPtr, const Rcpp::NumericVector radii,
                            const Rcpp::Nullable<std::string> weightedMeasureColNameNV = R_NilValue,
                            const Rcpp::Nullable<bool> includeChoiceNV = R_NilValue,
                            const Rcpp::Nullable<bool> includeIntermediateMetricsNV = R_NilValue,
                            const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
                            const Rcpp::Nullable<bool> verboseNV = R_NilValue,
                            const Rcpp::Nullable<bool> progressNV = R_NilValue) {

    auto weightedMeasureColName = NullableValue::getOptional(weightedMeasureColNameNV);
    auto includeChoice = NullableValue::get(includeChoiceNV, false);
    auto includeIntermediateMetrics = NullableValue::get(includeIntermediateMetricsNV, false);
    // The normal behaviour of R is to copy objects wholesale when applying a
    // function to them. In C++ this is to be avoided unless absolutely
    // necessary. However, since this function is to be used in R and for it to
    // have the same effects as any other R function, we will copy the given map
    // and provide a new pointer if instructed to do so.
    auto copyMap = NullableValue::get(copyMapNV, true);
    auto verbose = NullableValue::get(verboseNV, false);
    auto progress = NullableValue::get(progressNV, false);

    mapPtr = RcppRunner::copyMap(mapPtr, copyMap);

    return RcppRunner::runAnalysis<ShapeGraph>(
        mapPtr, progress,
        [&radii, &weightedMeasureColName, &includeChoice, &includeIntermediateMetrics,
         &verbose](Communicator *comm, Rcpp::XPtr<ShapeGraph> mapPtr) {
            if (verbose)
                Rcpp::Rcout << "Running axial analysis... " << '\n';

            int weightedMeasureColIdx = -1;

            if (weightedMeasureColName.has_value()) {
                const AttributeTable &table = mapPtr->getAttributeTable();
                for (size_t i = 0; i < table.getNumColumns(); i++) {
                    if (*weightedMeasureColName == table.getColumnName(i).c_str()) {
                        weightedMeasureColIdx = static_cast<int>(i);
                    }
                }
                if (weightedMeasureColIdx == -1) {
                    throw depthmapX::RuntimeException(
                        "Given attribute (" + *weightedMeasureColName + ") does not exist in " +
                        "currently selected map");
                }
            }

            std::set<double> radius_set;
            radius_set.insert(radii.begin(), radii.end());
            auto analysis = AxialIntegration(radius_set, weightedMeasureColIdx, includeChoice,
                                             includeIntermediateMetrics);
            AnalysisResult analysisResult = analysis.run(comm, *mapPtr, false /* simple version*/);
            return analysisResult;
        });
}

// [[Rcpp::export("Rcpp_runAxialLocalAnalysis")]]
Rcpp::List runAxialLocalAnalysis(Rcpp::XPtr<ShapeGraph> mapPtr,
                                 const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
                                 const Rcpp::Nullable<bool> verboseNV = R_NilValue,
                                 const Rcpp::Nullable<bool> progressNV = R_NilValue) {
    auto copyMap = NullableValue::get(copyMapNV, true);
    auto verbose = NullableValue::get(verboseNV, false);
    auto progress = NullableValue::get(progressNV, false);

    mapPtr = RcppRunner::copyMap(mapPtr, copyMap);

    return RcppRunner::runAnalysis<ShapeGraph>(
        mapPtr, progress, [&verbose](Communicator *comm, Rcpp::XPtr<ShapeGraph> mapPtr) {
            if (verbose)
                Rcpp::Rcout << "Running axial analysis... " << '\n';

            auto analysis = AxialLocal();
            AnalysisResult analysisResult = analysis.run(comm, *mapPtr, false /* simple version*/);
            return analysisResult;
        });
}

// [[Rcpp::export("Rcpp_axialStepDepth")]]
Rcpp::List axialStepDepth(Rcpp::XPtr<ShapeGraph> mapPtr, const int stepType,
                          const std::vector<double> stepDepthPointsX,
                          const std::vector<double> stepDepthPointsY,
                          const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
                          const Rcpp::Nullable<bool> verboseNV = R_NilValue,
                          const Rcpp::Nullable<bool> progressNV = R_NilValue) {
    auto copyMap = NullableValue::get(copyMapNV, true);
    auto verbose = NullableValue::get(verboseNV, false);
    auto progress = NullableValue::get(progressNV, false);

    auto traversalStepType = getAsValidEnum<TraversalType>(stepType);

    mapPtr = RcppRunner::copyMap(mapPtr, copyMap);

    return RcppRunner::runAnalysis<ShapeGraph>(
        mapPtr, progress,
        [&stepDepthPointsX, &stepDepthPointsY, traversalStepType,
         &verbose](Communicator *comm, Rcpp::XPtr<ShapeGraph> mapPtr) {
            if (verbose)
                Rcpp::Rcout << "ok\nSelecting cells... " << '\n';

            std::set<int> origins;
            for (size_t i = 0; i < stepDepthPointsX.size(); ++i) {
                Point2f p2f(stepDepthPointsX[i], stepDepthPointsY[i]);
                auto graphRegion = mapPtr->getRegion();
                if (!graphRegion.contains(p2f)) {
                    throw depthmapX::RuntimeException("Point outside of target region");
                }
                Region4f r(p2f, p2f);
                origins.insert(mapPtr->getShapesInRegion(r).begin()->first);
            }

            if (verbose)
                Rcpp::Rcout << "ok\nCalculating step-depth... " << '\n';

            Rcpp::List result = Rcpp::List::create(Rcpp::Named("completed") = false);

            AnalysisResult analysisResult;
            switch (traversalStepType) {
            case TraversalType::Topological:
                // currently axial only allows for topological analysis
                analysisResult =
                    AxialStepDepth(origins).run(comm, *mapPtr, false /* simple mode */);
                break;
            case TraversalType::Angular:
            case TraversalType::Metric:
                // never really supported for axial maps
                throw depthmapX::RuntimeException("Error, unsupported step type");
            case TraversalType::None: {
                throw depthmapX::RuntimeException("Error, unsupported step type");
            }
            }
            return analysisResult;
        });
}
