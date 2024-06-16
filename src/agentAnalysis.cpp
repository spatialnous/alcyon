// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "salalib/agents/agentengine.h"

#include "AgentLookMode.h"
#include "communicator.h"

#include <Rcpp.h>

// [[Rcpp::export("Rcpp_agentAnalysis")]]
Rcpp::List agentAnalysis(Rcpp::XPtr<PointMap> pointMapPtr,
                         int systemTimesteps,
                         float releaseRate,
                         int agentLifeTimesteps,
                         int agentFov,
                         int agentStepsToDecision,
                         int agentLookMode,
                         Rcpp::NumericMatrix agentReleaseLocations,
                         int randomReleaseLocationSeed,
                         int recordTrailForAgents,
                         bool getGateCounts,
                         const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
                         const Rcpp::Nullable<bool> verboseNV = R_NilValue,
                         const Rcpp::Nullable<bool> progressNV = R_NilValue) {

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

  if (copyMap) {
    auto prevPointMap = pointMapPtr;
    const auto &prevRegion = prevPointMap->getRegion();
    pointMapPtr = Rcpp::XPtr(new PointMap(prevRegion));
    pointMapPtr->copy(*prevPointMap, true, true);
  }

  AgentEngine eng;

  if (!eng.agentSets.size()) {
    eng.agentSets.push_back(AgentSet());
  }

  eng.m_timesteps = systemTimesteps;
  eng.agentSets.back().m_release_rate = releaseRate;
  eng.agentSets.back().m_lifetime = agentLifeTimesteps;
  if (agentFov == 32) {
    eng.agentSets.back().m_vbin = -1;
  } else {
    eng.agentSets.back().m_vbin = (agentFov - 1) / 2;
  }
  eng.agentSets.back().m_steps = agentStepsToDecision;
  switch (static_cast<AgentLookMode>(agentLookMode)) {
  case AgentLookMode::None:
  case AgentLookMode::Standard:
    eng.agentSets.back().m_sel_type = AgentProgram::SEL_STANDARD;
    break;
  case AgentLookMode::LineOfSightLength:
    eng.agentSets.back().m_sel_type = AgentProgram::SEL_LOS;
    break;
  case AgentLookMode::OcclusionLength:
    eng.agentSets.back().m_sel_type = AgentProgram::SEL_LOS_OCC;
    break;
  case AgentLookMode::OcclusionAny:
    eng.agentSets.back().m_sel_type = AgentProgram::SEL_OCC_ALL;
    break;
  case AgentLookMode::OcclusionGroup45:
    eng.agentSets.back().m_sel_type = AgentProgram::SEL_OCC_BIN45;
    break;
  case AgentLookMode::OcclusionGroup60:
    eng.agentSets.back().m_sel_type = AgentProgram::SEL_OCC_BIN60;
    break;
  case AgentLookMode::OcclusionFurthest:
    eng.agentSets.back().m_sel_type = AgentProgram::SEL_OCC_STANDARD;
    break;
  case AgentLookMode::BinFarDistance:
    eng.agentSets.back().m_sel_type = AgentProgram::SEL_OCC_WEIGHT_DIST;
    break;
  case AgentLookMode::BinAngle:
    eng.agentSets.back().m_sel_type = AgentProgram::SEL_OCC_WEIGHT_ANG;
    break;
  case AgentLookMode::BinFarDistanceAngle:
    eng.agentSets.back().m_sel_type = AgentProgram::SEL_OCC_WEIGHT_DIST_ANG;
    break;
  case AgentLookMode::BinMemory:
    eng.agentSets.back().m_sel_type = AgentProgram::SEL_OCC_MEMORY;
    break;
  }

  // if the m_release_locations is not set the locations are
  // set later by picking random pixels
  if (randomReleaseLocationSeed >= 0) {
    eng.agentSets.back().m_release_locations_seed = randomReleaseLocationSeed;
  } else {
    eng.agentSets.back().m_release_locations.clear();

    for (int r = 0; r < agentReleaseLocations.rows(); ++r) {
      auto coordRow = agentReleaseLocations.row(r);
      Point2f p(coordRow[0], coordRow[1]);
      eng.agentSets.back().m_release_locations.push_back(
          pointMapPtr->pixelate(p, false));
    }
  }

  // the ui and code suggest that the results can be put on a separate
  // 'data map', but the functionality does not seem to actually be
  // there thus it is skipped for now
  // eng.m_gatelayer = m_gatelayer;

  // note, trails currently per run, but output per engine
  if (recordTrailForAgents == 0) {
    eng.m_record_trails = true;
  } else if (recordTrailForAgents > 0) {
    eng.m_record_trails = true;
    eng.m_trail_count = recordTrailForAgents;
  }
  if (verbose) {
    Rcpp::Rcout << "ok\nRunning agent analysis... " << std::flush;
  }
  eng.run(getCommunicator(true).get(), pointMapPtr);

  Rcpp::List result = Rcpp::List::create(
    Rcpp::Named("completed") = true,
    Rcpp::Named("newAttributes") = Rcpp::CharacterVector("Gate Counts"),
    Rcpp::Named("mapPtr") = pointMapPtr
  );
  if (recordTrailForAgents > 0) {
    ShapeMap trailMap("Agent Trails");
    eng.insertTrailsInMap(trailMap);
    result["newShapeMaps"] = Rcpp::List::create(
      Rcpp::Named("trailMap") = Rcpp::XPtr<ShapeMap>(
        new ShapeMap(std::move(trailMap))
      )
    );
  }
  return result;
}
