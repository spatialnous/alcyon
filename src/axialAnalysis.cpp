// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-or-later


#include "salalib/axialmodules/axialintegration.h"
#include "salalib/axialmodules/axialstepdepth.h"

#include "salalib/shapemap.h"
#include "salalib/shapegraph.h"
#include "genlib/p2dpoly.h"

#include <Rcpp.h>

namespace AxialAnalysis {


std::unique_ptr<Communicator> getCommunicator(const bool printProgress) {
    // if (printProgress) {
    //   return std::unique_ptr<Communicator>(new PrintCommunicator());
    // }
    return nullptr;
}

enum StepType {
    TOPOLOGICAL,
    METRIC,
    ANGULAR
};

}

// [[Rcpp::export("Rcpp_runAxialAnalysis")]]
bool runAxialAnalysis(
        Rcpp::XPtr<ShapeGraph> shapeGraph,
        const Rcpp::NumericVector radii,
        const Rcpp::Nullable<std::string> weightedMeasureColNameNV = R_NilValue,
        const Rcpp::Nullable<bool> includeChoiceNV = R_NilValue,
        const Rcpp::Nullable<bool> includeLocalNV = R_NilValue,
        const Rcpp::Nullable<bool> includeIntermediateMetricsNV = R_NilValue,
        const Rcpp::Nullable<bool> verboseNV = R_NilValue,
        const Rcpp::Nullable<bool> progressNV = R_NilValue) {
    std::string weightedMeasureColName = "";
    if (weightedMeasureColNameNV.isNotNull()) {
        weightedMeasureColName = Rcpp::as<std::string>(weightedMeasureColNameNV);
    }
    bool includeChoice = false;
    if (includeChoiceNV.isNotNull()) {
        includeChoice = Rcpp::as<bool>(includeChoiceNV);
    }
    bool includeLocal = false;
    if (includeLocalNV.isNotNull()) {
        includeLocal = Rcpp::as<bool>(includeLocalNV);
    }
    bool includeIntermediateMetrics = false;
    if (includeIntermediateMetricsNV.isNotNull()) {
        includeIntermediateMetrics = Rcpp::as<bool>(includeIntermediateMetricsNV);
    }
    bool verbose = false;
    if (verboseNV.isNotNull()) {
        verbose = Rcpp::as<bool>(verboseNV);
    }
    bool progress = false;
    if (progressNV.isNotNull()) {
        progress = Rcpp::as<bool>(progressNV);
    }

    if (verbose)
        Rcpp::Rcout << "Running axial analysis... " << '\n';

    int weightedMeasureColIdx = -1;

    if (!weightedMeasureColName.empty()) {
        const AttributeTable &table = shapeGraph->getAttributeTable();
        for (int i = 0; i < table.getNumColumns(); i++) {
            if (weightedMeasureColName == table.getColumnName(i).c_str()) {
                weightedMeasureColIdx = i;
            }
        }
        if (weightedMeasureColIdx == -1) {
            throw depthmapX::RuntimeException("Given attribute (" +
                                              weightedMeasureColName +
                                              ") does not exist in " +
                                              "currently selected map");
        }
    }

    bool analysisCompleted = false;

    try {
        std::set<double> radius_set;
        radius_set.insert(radii.begin(), radii.end());
        auto analysis = AxialIntegration(radius_set,
                                         weightedMeasureColIdx,
                                         includeChoice,
                                         includeIntermediateMetrics,
                                         includeLocal);
        analysisCompleted = analysis.run(
            AxialAnalysis::getCommunicator(progress).get(),
            *shapeGraph,
            false /* simple version*/
        );
    } catch (Communicator::CancelledException) {
        analysisCompleted = false;
    }
    return analysisCompleted;
}


// [[Rcpp::export("Rcpp_axialStepDepth")]]
bool axialStepDepth(
        Rcpp::XPtr<ShapeGraph> shapeGraph,
        const std::vector<double> stepDepthPointsX,
        const std::vector<double> stepDepthPointsY,
        const int stepType,
        const Rcpp::Nullable<bool> verboseNV = R_NilValue,
        const Rcpp::Nullable<bool> progressNV = R_NilValue) {
    bool verbose = false;
    if (verboseNV.isNotNull()) {
        verbose = Rcpp::as<bool>(verboseNV);
    }
    bool progress = false;
    if (progressNV.isNotNull()) {
        progress = Rcpp::as<bool>(progressNV);
    }

    Rcpp::Rcout << "ok\nSelecting cells... " << '\n';

    for (int i = 0; i < stepDepthPointsX.size(); ++i) {
        Point2f p2f(stepDepthPointsX[i], stepDepthPointsY[i]);
        auto graphRegion = shapeGraph->getRegion();
        if (!graphRegion.contains(p2f)) {
            throw depthmapX::RuntimeException("Point outside of target region");
        }
        QtRegion r(p2f, p2f);
        shapeGraph->setCurSel(r, true);
    }

    Rcpp::Rcout << "ok\nCalculating step-depth... " << '\n';

    bool analysisCompleted = false;

    try {
        switch (stepType) {
        // never really supported for axial maps
        // case AxialAnalysis::AxialStepType::ANGULAR:
        //     pointDepthType = 3;
        //     break;
        // case AxialAnalysis::AxialStepType::METRIC:
        //     pointDepthType = 2;
        //     break;
        case AxialAnalysis::StepType::TOPOLOGICAL:
            // currently axial only allows for topological analysis
            analysisCompleted = AxialStepDepth().run(
                AxialAnalysis::getCommunicator(progress).get(),
                *shapeGraph,
                false /* simple mode */
            );
            break;
        default: {
                throw depthmapX::RuntimeException("Error, unsupported step type");
            }
        }

    } catch (Communicator::CancelledException) {
        analysisCompleted = false;
    }

    return analysisCompleted;
}
