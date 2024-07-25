// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "salalib/vgamodules/vgaisovist.h"

#include "communicator.h"

#include <Rcpp.h>

// [[Rcpp::export("Rcpp_VGA_isovist")]]
Rcpp::List vgaIsovist(Rcpp::XPtr<PointMap> pointMapPtr,
                      Rcpp::XPtr<ShapeMap> shapeMapPtr,
                      const Rcpp::Nullable<bool> copyMapNV = R_NilValue) {
  bool copyMap = true;
  if (copyMapNV.isNotNull()) {
    copyMap = Rcpp::as<bool>(copyMapNV);
  }
  if (copyMap) {
    auto prevPointMap = pointMapPtr;
    const auto &prevRegion = prevPointMap->getRegion();
    pointMapPtr = Rcpp::XPtr(new PointMap(prevRegion));
    pointMapPtr->copy(*prevPointMap, true, true);
  }

  Rcpp::List result = Rcpp::List::create(
    Rcpp::Named("completed") = false
  );
  auto shapeMap = shapeMapPtr->getAllShapes();

  std::vector<SalaShape> shapes;
  shapes.reserve(shapeMap.size());
  for(auto it = shapeMap.begin(); it != shapeMap.end(); ++it) {
    shapes.push_back(it->second);
  }

  auto analysisResult = VGAIsovist(shapes)
  .run(getCommunicator(true).get(), *pointMapPtr, false);
  result["completed"] = analysisResult.completed;
  result["newAttributes"] = analysisResult.getAttributes();
  result["mapPtr"] = pointMapPtr;
  return result;
}
