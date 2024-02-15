// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "salalib/shapegraph.h"

#include <Rcpp.h>

RCPP_EXPOSED_CLASS(ShapeGraph);


// [[Rcpp::export("Rcpp_ShapeGraph_getAxialConnections")]]
std::map<std::string, std::vector<int>> getAxialConnections(
    Rcpp::XPtr<ShapeMap> shapeGraph) {
  auto &connectors = shapeGraph->getConnections();
  std::map<std::string, std::vector<int>> axialConnections;
  std::vector<int> &axialConnectionsFrom = axialConnections["from"];
  std::vector<int> &axialConnectionsTo = axialConnections["to"];
  for (int i = 0; i < connectors.size(); i++) {
    const std::vector<int> &connections = connectors[i].m_connections;
    for (int connection : connections) {
      axialConnectionsFrom.push_back(i);
      axialConnectionsTo.push_back(connection);
    }
  }
  return axialConnections;
}

// [[Rcpp::export("Rcpp_ShapeGraph_getSegmentConnections")]]
std::map<std::string, std::vector<int>> getSegmentConnections(
    Rcpp::XPtr<ShapeMap> shapeGraph) {

  auto &connectors = shapeGraph->getConnections();
  std::map<std::string, std::vector<int>> segmentConnections;
  std::vector<int> &segmentConnectionsFrom = segmentConnections["from"];
  std::vector<int> &segmentConnectionsTo = segmentConnections["to"];
  std::vector<int> &segmentConnectionsSSWeight =
    segmentConnections["ss_weight"];
  std::vector<int> &segmentConnectionsBackward =
    segmentConnections["backward"];
  std::vector<int> &segmentConnectionsDirection =
    segmentConnections["direction"];

  // directed links
  for (size_t i = 0; i < connectors.size(); i++) {
    for (auto &segconn : connectors[i].m_forward_segconns) {
      segmentConnectionsFrom.push_back(i);
      segmentConnectionsTo.push_back(segconn.first.ref);
      segmentConnectionsSSWeight.push_back(segconn.second);
      segmentConnectionsBackward.push_back(0);
      segmentConnectionsDirection.push_back(int(segconn.first.dir));
    }

    for (auto &segconn : connectors[i].m_back_segconns) {
      segmentConnectionsFrom.push_back(i);
      segmentConnectionsTo.push_back(segconn.first.ref);
      segmentConnectionsSSWeight.push_back(segconn.second);
      segmentConnectionsBackward.push_back(1);
      segmentConnectionsDirection.push_back(int(segconn.first.dir));
    }
  }
  return segmentConnections;
}

