// SPDX-FileCopyrightText: 2024-2025 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "salalib/agents/agentanalysis.hpp"

#include "enum_AgentLookMode.hpp"

#include "communicator.hpp"
#include "helper_enum.hpp"
#include "helper_nullablevalue.hpp"
#include "helper_runAnalysis.hpp"

#include <Rcpp.h>

// [[Rcpp::export("Rcpp_agentAnalysis")]]
Rcpp::List agentAnalysis(Rcpp::XPtr<PointMap> mapPtr, int systemTimesteps, float releaseRate,
                         int agentLifeTimesteps, int agentFov, int agentStepsToDecision,
                         int agentLookMode, Rcpp::NumericMatrix agentReleaseLocations,
                         int randomReleaseLocationSeed, int recordTrailForAgents,
                         bool getGateCounts, const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
                         const Rcpp::Nullable<bool> verboseNV = R_NilValue,
                         const Rcpp::Nullable<bool> progressNV = R_NilValue) {
    auto copyMap = NullableValue::get(copyMapNV, true);
    auto verbose = NullableValue::get(verboseNV, false);
    auto progress = NullableValue::get(progressNV, false);

    mapPtr = RcppRunner::copyMapWithRegion(mapPtr, copyMap);

    int agentAlgorithm = AgentProgram::SEL_STANDARD;
    switch (static_cast<AgentLookMode>(agentLookMode)) {
    case AgentLookMode::None:
    case AgentLookMode::Standard:
        agentAlgorithm = AgentProgram::SEL_STANDARD;
        break;
    case AgentLookMode::LineOfSightLength:
        agentAlgorithm = AgentProgram::SEL_LOS;
        break;
    case AgentLookMode::OcclusionLength:
        agentAlgorithm = AgentProgram::SEL_LOS_OCC;
        break;
    case AgentLookMode::OcclusionAny:
        agentAlgorithm = AgentProgram::SEL_OCC_ALL;
        break;
    case AgentLookMode::OcclusionGroup45:
        agentAlgorithm = AgentProgram::SEL_OCC_BIN45;
        break;
    case AgentLookMode::OcclusionGroup60:
        agentAlgorithm = AgentProgram::SEL_OCC_BIN60;
        break;
    case AgentLookMode::OcclusionFurthest:
        agentAlgorithm = AgentProgram::SEL_OCC_STANDARD;
        break;
    case AgentLookMode::BinFarDistance:
        agentAlgorithm = AgentProgram::SEL_OCC_WEIGHT_DIST;
        break;
    case AgentLookMode::BinAngle:
        agentAlgorithm = AgentProgram::SEL_OCC_WEIGHT_ANG;
        break;
    case AgentLookMode::BinFarDistanceAngle:
        agentAlgorithm = AgentProgram::SEL_OCC_WEIGHT_DIST_ANG;
        break;
    case AgentLookMode::BinMemory:
        agentAlgorithm = AgentProgram::SEL_OCC_MEMORY;
        break;
    }

    std::vector<Point2f> releasePoints;
    for (int r = 0; r < agentReleaseLocations.rows(); ++r) {
        auto coordRow = agentReleaseLocations.row(r);
        releasePoints.emplace_back(coordRow[0], coordRow[1]);
    }

    // the ui and code suggest that the results can be put on a separate
    // 'data map', but the functionality does not seem to actually be
    // there thus it is skipped for now
    // eng.m_gatelayer = m_gatelayer;

    // note, trails currently per run, but output per engine
    if (verbose) {
        Rcpp::Rcout << "ok\nRunning agent analysis... " << std::flush;
    }

    // the ui and code suggest that the results can be put on a separate
    // 'data map', but the functionality does not seem to actually be
    // there thus it is skipped for now
    std::optional<std::reference_wrapper<ShapeMap>> gateLayer = std::nullopt;

    ShapeMap trailMap("Agent Trails");

    std::optional<AgentAnalysis::TrailRecordOptions> recordTrails =
        recordTrailForAgents >= 0
            ? std::make_optional(AgentAnalysis::TrailRecordOptions{
                  recordTrailForAgents == 0
                      ? std::nullopt
                      : std::make_optional(static_cast<size_t>(recordTrailForAgents)),
                  std::ref(trailMap)})
            : std::nullopt;

    RcppAnalysisResults result(mapPtr);

    try {
        auto analysisResult =
            AgentAnalysis(*mapPtr, systemTimesteps, releaseRate, agentLifeTimesteps, agentFov,
                          agentStepsToDecision, agentAlgorithm, randomReleaseLocationSeed,
                          releasePoints, gateLayer, recordTrails)
                .run(getCommunicator(progress).get());

        result.setFromResult(std::move(analysisResult));

        if (recordTrailForAgents > 0) {
            result.getData()["newShapeMaps"] = Rcpp::List::create(
                Rcpp::Named("trailMap") = Rcpp::XPtr<ShapeMap>(new ShapeMap(std::move(trailMap))));
        }

    } catch (Communicator::CancelledException &) {
        result.cancel();
    }
    return result.getData();
}
