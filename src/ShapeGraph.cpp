// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "salalib/shapegraph.h"

#include <Rcpp.h>

RCPP_EXPOSED_CLASS(ShapeGraph);


// [[Rcpp::export("Rcpp_ShapeGraph_getAxialConnections")]]
std::map<std::string, std::vector<int>> getAxialConnections(
    Rcpp::XPtr<ShapeMap> shapeGraphPtr) {
  auto &connectors = shapeGraphPtr->getConnections();
  std::map<std::string, std::vector<int>> axialConnections;
  std::vector<int> &axialConnectionsFrom = axialConnections["from"];
  std::vector<int> &axialConnectionsTo = axialConnections["to"];
  for (int i = 0; i < connectors.size(); i++) {
    const auto &connections = connectors[i].m_connections;
    for (int connection : connections) {
      axialConnectionsFrom.push_back(i);
      axialConnectionsTo.push_back(connection);
    }
  }
  return axialConnections;
}

// [[Rcpp::export("Rcpp_ShapeGraph_getSegmentConnections")]]
std::map<std::string, std::vector<int>> getSegmentConnections(
    Rcpp::XPtr<ShapeMap> shapeGraphPtr) {
  auto &connectors = shapeGraphPtr->getConnections();
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

// [[Rcpp::export("Rcpp_ShapeGraph_getLinksUnlinks")]]
Rcpp::NumericMatrix getLinksUnlinks(
    Rcpp::XPtr<ShapeGraph> shapeGraphPtr) {
  const auto &links = shapeGraphPtr->getLinks();
  const auto &unlinks = shapeGraphPtr->getUnlinks();
  Rcpp::NumericMatrix linkUnlinkData(
      links.size() + unlinks.size(), 3L
  );
  Rcpp::colnames(linkUnlinkData) = Rcpp::CharacterVector({
    "from", "to", "isunlink"
  });
  int rowIdx = 0;
  for (auto link: links) {
    const Rcpp::NumericMatrix::Row &row = linkUnlinkData( rowIdx , Rcpp::_ );
    row[0] = link.a;
    row[1] = link.b;
    row[2] = 0; // link
    rowIdx++;
  }
  for (auto unlink: unlinks) {
    const Rcpp::NumericMatrix::Row &row = linkUnlinkData( rowIdx , Rcpp::_ );
    row[0] = unlink.a;
    row[1] = unlink.b;
    row[2] = 1; // unlink
    rowIdx++;
  }
  return linkUnlinkData;
}
