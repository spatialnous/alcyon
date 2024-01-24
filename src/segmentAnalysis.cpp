// Copyright 2024 Petros Koutsolampros
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program. If not, see <https://www.gnu.org/licenses/>.

#include "salalib/shapegraph.h"
#include "salalib/options.h"

#include "salalib/segmmodules/segmtopological.h"
#include "salalib/segmmodules/segmtulip.h"
#include "salalib/segmmodules/segmmetric.h"
#include "salalib/segmmodules/segmangular.h"
#include "salalib/segmmodules/segmtopologicalpd.h"
#include "salalib/segmmodules/segmtulipdepth.h"
#include "salalib/segmmodules/segmmetricpd.h"

#include <Rcpp.h>

namespace SegmentAnalysis {
// traversal type
enum AnalysisStepType {
    ANGULAR_TULIP,
    ANGULAR_FULL,
    TOPOLOGICAL,
    METRIC
};

// limit type
enum RadiusStepType {
    RADIUS_STEPS,
    RADIUS_METRIC,
    RADIUS_ANGULAR
};

std::unique_ptr<Communicator> getCommunicator(const bool printProgress) {
    // if (printProgress) {
    //   return std::unique_ptr<Communicator>(new PrintCommunicator());
    // }
    return nullptr;
}

}

// [[Rcpp::export]]
bool runSegmentAnalysis(
        Rcpp::XPtr<ShapeGraph> shapeGraph,
        const Rcpp::NumericVector radii,
        const int radiusStepType,
        const int analysisStepType,
        const Rcpp::Nullable<std::string> weightedMeasureColNameNV = R_NilValue,
        const Rcpp::Nullable<bool> includeChoiceNV = false,
        const Rcpp::Nullable<int> tulipBinsNV = false,
        const Rcpp::Nullable<bool> verboseNV = false,
        const Rcpp::Nullable<bool> selOnlyNV = false,
        const Rcpp::Nullable<bool> progressNV = false) {

    std::string weightedMeasureColName = "";
    if (weightedMeasureColNameNV.isNotNull()) {
        weightedMeasureColName = Rcpp::as<std::string>(weightedMeasureColNameNV);
    }
    bool includeChoice = false;
    if (includeChoiceNV.isNotNull()) {
        includeChoice = Rcpp::as<bool>(includeChoiceNV);
    }
    int tulipBins = 1024;
    if (tulipBinsNV.isNotNull()) {
        tulipBins = Rcpp::as<int>(tulipBinsNV);
    }
    // TODO: Instead of expecting things to be selected,
    // provide indices to select
    bool selOnly = false;
    if (selOnlyNV.isNotNull()) {
        selOnly = Rcpp::as<bool>(selOnlyNV);
    }
    bool verbose = false;
    if (verboseNV.isNotNull()) {
        verbose = Rcpp::as<bool>(verboseNV);
    }
    bool progress = false;
    if (progressNV.isNotNull()) {
        progress = Rcpp::as<bool>(progressNV);
    }

    std::cout << "Running segment analysis... " << std::flush;


    std::set<double> radius_set;
    radius_set.insert(radii.begin(), radii.end());

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

    int radius_type = -1;

    switch (radiusStepType) {
    case SegmentAnalysis::RadiusStepType::RADIUS_STEPS: {
        int radius_type = Options::RADIUS_STEPS;
        break;
    }
    case SegmentAnalysis::RadiusStepType::RADIUS_METRIC: {
        radius_type = Options::RADIUS_METRIC;
        break;
    }
    case SegmentAnalysis::RadiusStepType::RADIUS_ANGULAR: {
        radius_type = Options::RADIUS_ANGULAR;
        break;
    }
    default:
        throw depthmapX::RuntimeException("No radius analysis type given");
    }

    bool analysisCompleted = false;

    try {
        switch (analysisStepType) {
        case SegmentAnalysis::AnalysisStepType::ANGULAR_TULIP: {
            analysisCompleted =
                SegmentTulip(radius_set, selOnly,
                             tulipBins, weightedMeasureColIdx,
                             radius_type, includeChoice).run(
                                     SegmentAnalysis::getCommunicator(progress).get(),
                                     *shapeGraph, false /* interactive */);
            break;
        }
        case SegmentAnalysis::AnalysisStepType::ANGULAR_FULL: {
            analysisCompleted =
                SegmentAngular(radius_set).run(
                        SegmentAnalysis::getCommunicator(progress).get(),
                        *shapeGraph,
                        false /* unused */);
            break;
        }
        case SegmentAnalysis::AnalysisStepType::TOPOLOGICAL: {
            // TODO allow multiple radii
            analysisCompleted =
                SegmentTopological(*radius_set.begin(), selOnly).run(
                        SegmentAnalysis::getCommunicator(progress).get(),
                        *shapeGraph, false /* unused */);
            break;
        }
        case SegmentAnalysis::AnalysisStepType::METRIC: {
            // TODO allow multiple radii
            analysisCompleted =
                SegmentMetric(*radius_set.begin(), selOnly).run(
                        SegmentAnalysis::getCommunicator(progress).get(),
                        *shapeGraph, false /* unused */);
            break;
        }
        default:
            throw depthmapX::RuntimeException("No segment analysis type given");
        }
    } catch (Communicator::CancelledException) {
        analysisCompleted = false;
    }
    std::cout << "ok\n" << std::flush;


    return analysisCompleted;
}


// [[Rcpp::export]]
bool segmentStepDepth(
        Rcpp::XPtr<ShapeGraph> shapeGraph,
        const std::vector<double> stepDepthPointsX,
        const std::vector<double> stepDepthPointsY,
        const int stepType,
        const Rcpp::Nullable<bool> verboseNV = false,
        const Rcpp::Nullable<bool> progressNV = false) {
    bool verbose = false;
    if (verboseNV.isNotNull()) {
        verbose = Rcpp::as<bool>(verboseNV);
    }
    bool progress = false;
    if (progressNV.isNotNull()) {
        progress = Rcpp::as<bool>(progressNV);
    }

    std::cout << "ok\nSelecting cells... " << std::flush;

    for (int i = 0; i < stepDepthPointsX.size(); ++i) {
        Point2f p2f(stepDepthPointsX[i], stepDepthPointsY[i]);
        auto graphRegion = shapeGraph->getRegion();
        if (!graphRegion.contains(p2f)) {
            throw depthmapX::RuntimeException("Point outside of target region");
        }
        QtRegion r(p2f, p2f);
        shapeGraph->setCurSel(r, true);
    }

    std::cout << "ok\nCalculating step-depth... " << std::flush;

    bool analysisCompleted = false;

    try {

        switch (stepType) {
        case SegmentAnalysis::AnalysisStepType::ANGULAR_FULL:
            // full angular was never created as a step-function
            // do normal tulip
        case SegmentAnalysis::AnalysisStepType::ANGULAR_TULIP:
            analysisCompleted = SegmentMetricPD().run(
                SegmentAnalysis::getCommunicator(progress).get(),
                *shapeGraph,
                false /* simple mode */
            );
            break;
        case SegmentAnalysis::AnalysisStepType::METRIC:
            analysisCompleted = SegmentTulipDepth().run(
                SegmentAnalysis::getCommunicator(progress).get(),
                *shapeGraph,
                false /* simple mode */
            );
            break;
        case SegmentAnalysis::AnalysisStepType::TOPOLOGICAL:
            analysisCompleted = SegmentTulipDepth().run(
                SegmentAnalysis::getCommunicator(progress).get(),
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

