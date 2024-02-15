// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "salalib/shapegraph.h"
#include "salalib/options.h"

#include "salalib/segmmodules/segmtopological.h"
#include "salalib/segmmodules/segmtulip.h"
#include "salalib/segmmodules/segmmetric.h"
#include "salalib/segmmodules/segmangular.h"
#include "salalib/segmmodules/segmtopologicalpd.h"
#include "salalib/segmmodules/segmtulipdepth.h"
#include "salalib/segmmodules/segmmetricpd.h"

#include "communicator.h"
#include "traversal.h"

#include <Rcpp.h>

// [[Rcpp::export("Rcpp_runSegmentAnalysis")]]
bool runSegmentAnalysis(
        Rcpp::XPtr<ShapeGraph> shapeGraph,
        const Rcpp::NumericVector radii,
        const int radiusStepType,
        const int analysisStepType,
        const Rcpp::Nullable<std::string> weightedMeasureColNameNV = R_NilValue,
        const Rcpp::Nullable<bool> includeChoiceNV = R_NilValue,
        const Rcpp::Nullable<int> tulipBinsNV = R_NilValue,
        const Rcpp::Nullable<bool> verboseNV = R_NilValue,
        const Rcpp::Nullable<bool> selOnlyNV = R_NilValue,
        const Rcpp::Nullable<bool> progressNV = R_NilValue) {

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

    if (verbose) {
        Rcpp::Rcout << "Running segment analysis... " << '\n';
    }


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

    int radiusType = -1;

    switch (static_cast<Traversal>(radiusStepType)) {
    case Traversal::Topological: {
        int radiusType = Options::RADIUS_STEPS;
        break;
    }
    case Traversal::Metric: {
        radiusType = Options::RADIUS_METRIC;
        break;
    }
    case Traversal::Angular: {
        radiusType = Options::RADIUS_ANGULAR;
        break;
    }
    default:
        throw depthmapX::RuntimeException("No radius analysis type given");
    }

    bool analysisCompleted = false;

    try {
        switch (static_cast<Traversal>(analysisStepType)) {
        case Traversal::Tulip: {
            analysisCompleted =
                SegmentTulip(radius_set, selOnly,
                             tulipBins, weightedMeasureColIdx,
                             radiusType, includeChoice).run(
                                     getCommunicator(progress).get(),
                                     *shapeGraph, false /* interactive */);
            break;
        }
        case Traversal::Angular: {
            analysisCompleted =
                SegmentAngular(radius_set).run(
                        getCommunicator(progress).get(),
                        *shapeGraph,
                        false /* unused */);
            break;
        }
        case Traversal::Topological: {
            // TODO allow multiple radii
            analysisCompleted =
                SegmentTopological(*radius_set.begin(), selOnly).run(
                        getCommunicator(progress).get(),
                        *shapeGraph, false /* unused */);
            break;
        }
        case Traversal::Metric: {
            // TODO allow multiple radii
            analysisCompleted =
                SegmentMetric(*radius_set.begin(), selOnly).run(
                        getCommunicator(progress).get(),
                        *shapeGraph, false /* unused */);
            break;
        }
        default:
            throw depthmapX::RuntimeException("No segment analysis type given");
        }
    } catch (Communicator::CancelledException) {
        analysisCompleted = false;
    }
    if (verbose) {
        Rcpp::Rcout << "ok" << '\n';
    }


    return analysisCompleted;
}


// [[Rcpp::export("Rcpp_segmentStepDepth")]]
bool segmentStepDepth(
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

    if (verbose) {
        Rcpp::Rcout << "ok\nSelecting cells... " << '\n';
    }

    for (int i = 0; i < stepDepthPointsX.size(); ++i) {
        Point2f p2f(stepDepthPointsX[i], stepDepthPointsY[i]);
        auto graphRegion = shapeGraph->getRegion();
        if (!graphRegion.contains(p2f)) {
            throw depthmapX::RuntimeException("Point outside of target region");
        }
        QtRegion r(p2f, p2f);
        shapeGraph->setCurSel(r, true);
    }

    if (verbose) {
        Rcpp::Rcout << "ok\nCalculating step-depth... " << '\n';
    }

    bool analysisCompleted = false;

    try {

        switch (static_cast<Traversal>(stepType)) {
        case Traversal::Angular:
            // full angular was never created as a step-function
            // do normal tulip
        case Traversal::Metric:
            analysisCompleted = SegmentMetricPD().run(
                getCommunicator(progress).get(),
                *shapeGraph,
                false /* simple mode */
            );
            break;
        case Traversal::Tulip:
            analysisCompleted = SegmentTulipDepth().run(
                getCommunicator(progress).get(),
                *shapeGraph,
                false /* simple mode */
            );
            break;
        case Traversal::Topological:
            analysisCompleted = SegmentTopologicalPD().run(
                getCommunicator(progress).get(),
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

