// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "salalib/shapegraph.h"

#include "salalib/radiustype.h"
#include "salalib/segmmodules/segmtopological.h"
#include "salalib/segmmodules/segmtulip.h"
#include "salalib/segmmodules/segmmetric.h"
#include "salalib/segmmodules/segmangular.h"

#include "communicator.h"
#include "TraversalType.h"

#include <Rcpp.h>

// [[Rcpp::export("Rcpp_runSegmentAnalysis")]]
Rcpp::List runSegmentAnalysis(
        Rcpp::XPtr<ShapeGraph> shapeGraph,
        const Rcpp::NumericVector radii,
        const int radiusStepType,
        const int analysisStepType,
        const Rcpp::Nullable<std::string> weightedMeasureColNameNV = R_NilValue,
        const Rcpp::Nullable<bool> includeChoiceNV = R_NilValue,
        const Rcpp::Nullable<int> tulipBinsNV = R_NilValue,
        const Rcpp::Nullable<bool> selOnlyNV = R_NilValue,
        const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
        const Rcpp::Nullable<bool> verboseNV = R_NilValue,
        const Rcpp::Nullable<bool> progressNV = R_NilValue) {

    std::optional<std::string> weightedMeasureColName = std::nullopt;
    if (weightedMeasureColNameNV.isNotNull()) {
        weightedMeasureColName = Rcpp::as<std::string>(weightedMeasureColNameNV);
    }
    bool includeChoice = false;
    if (includeChoiceNV.isNotNull()) {
        includeChoice = Rcpp::as<bool>(includeChoiceNV);
    }
    int tulipBins = 0;
    if (tulipBinsNV.isNotNull()) {
        tulipBins = Rcpp::as<int>(tulipBinsNV);
    }
    // TODO: Instead of expecting things to be selected,
    // provide indices to select
    bool selOnly = false;
    if (selOnlyNV.isNotNull()) {
        selOnly = Rcpp::as<bool>(selOnlyNV);
    }
    bool copyMap = true;
    if (copyMapNV.isNotNull()) {
        copyMap = Rcpp::as<bool>(copyMapNV);
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

    if (copyMap) {
        auto prevShapeGraph = shapeGraph;
        shapeGraph = Rcpp::XPtr(new ShapeGraph());
        shapeGraph->copy(*prevShapeGraph, ShapeMap::COPY_ALL, true);
    }

    std::set<double> radius_set;
    radius_set.insert(radii.begin(), radii.end());

    int weightedMeasureColIdx = -1;

    if (weightedMeasureColName.has_value()) {
        const AttributeTable &table = shapeGraph->getAttributeTable();
        for (int i = 0; i < table.getNumColumns(); i++) {
            if (weightedMeasureColName == table.getColumnName(i).c_str()) {
                weightedMeasureColIdx = i;
            }
        }
        if (weightedMeasureColIdx == -1) {
            Rcpp::stop("Given attribute (" +
                weightedMeasureColName.value() +
                ") does not exist in " +
                "currently selected map");
        }
    }

    RadiusType radiusType = RadiusType::NONE;
    std::map<double, std::string> radiusSuffixes;
    radiusSuffixes[-1] = "";

    switch (static_cast<TraversalType>(radiusStepType)) {
    case TraversalType::Topological: {
        radiusType = RadiusType::TOPOLOGICAL;
        for (auto radius: radii) {
            if (radius != -1) {
                radiusSuffixes[radius] = " R" + std::to_string(int(radius));
            }
        }
        break;
    }
    case TraversalType::Metric: {
        radiusType = RadiusType::METRIC;
        for (auto radius: radii) {
            if (radius != -1) {
                radiusSuffixes[radius] = " R" + std::to_string(radius) +
                    " metric";
            }
        }
        break;
    }
    case TraversalType::Angular: {
        radiusType = RadiusType::ANGULAR;
        for (auto radius: radii) {
            if (radius != -1) {
                radiusSuffixes[radius] = " R" + std::to_string(radius);
            }
        }
        break;
    }
    default:
        Rcpp::stop("No radius analysis type given");
    }

    Rcpp::List result = Rcpp::List::create(
        Rcpp::Named("completed") = false
    );


    try {
        AnalysisResult analysisResult;
        switch (static_cast<TraversalType>(analysisStepType)) {
        case TraversalType::Angular: {
            if (tulipBins > 0) {
            analysisResult =
                SegmentTulip(radius_set, std::nullopt,
                             tulipBins, weightedMeasureColIdx,
                             radiusType, includeChoice).run(
                                     getCommunicator(progress).get(),
                                     *shapeGraph, false /* interactive */);
        } else {
            analysisResult =
                SegmentAngular(radius_set).run(
                        getCommunicator(progress).get(),
                        *shapeGraph,
                        false /* unused */);
        }
        break;
        }
        case TraversalType::Topological: {
            bool first = true;
            for (auto radius: radius_set) {
                auto radiusAnalysisResult =
                    SegmentTopological(radius, std::nullopt).run(
                            getCommunicator(progress).get(),
                            *shapeGraph, false /* unused */);
                if (first) {
                    analysisResult.completed = radiusAnalysisResult.completed;
                    first = false;
                } else {
                    analysisResult.completed = analysisResult.completed &
                        radiusAnalysisResult.completed;
                }
                for (auto column: radiusAnalysisResult.getAttributes())
                    analysisResult.addAttribute(column);
            }
            break;
        }
        case TraversalType::Metric: {
            bool first = true;
            for (auto radius: radius_set) {
                auto radiusAnalysisResult =
                    SegmentMetric(radius, std::nullopt).run(
                            getCommunicator(progress).get(),
                            *shapeGraph, false /* unused */);
                if (first) {
                    analysisResult.completed = radiusAnalysisResult.completed;
                    first = false;
                } else {
                    analysisResult.completed = analysisResult.completed &
                        radiusAnalysisResult.completed;
                }
                for (auto column: radiusAnalysisResult.getAttributes())
                    analysisResult.addAttribute(column);
            }
            break;
        }
        default:
            Rcpp::stop("No segment analysis type given");
        }
        result["completed"] = analysisResult.completed;
        result["newAttributes"] = analysisResult.getAttributes();
        result["mapPtr"] = shapeGraph;
    } catch (Communicator::CancelledException) {
        Rcpp::stop("Analysis cancelled");
    }
    if (verbose) {
        Rcpp::Rcout << "ok" << '\n';
    }


    return result;
}

