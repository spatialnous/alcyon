// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include "salalib/pointdata.h"
#include "salalib/vgamodules/vgametric.h"
#include "salalib/vgamodules/vgavisualglobal.h"
#include "salalib/vgamodules/vgavisuallocal.h"
#include "salalib/vgamodules/vgathroughvision.h"
#include "salalib/vgamodules/vgaangular.h"
#include "salalib/vgamodules/vgaisovist.h"

#include "communicator.h"

#include <Rcpp.h>

// [[Rcpp::export("Rcpp_VGA_throughVision")]]
bool vgaThroughVision(Rcpp::XPtr<PointMap> pointMapPtr) {
  return
  VGAThroughVision().run(getCommunicator(true).get(), *pointMapPtr, false);
}

// [[Rcpp::export("Rcpp_VGA_angular")]]
bool vgaAngular(Rcpp::XPtr<PointMap> pointMapPtr,
                double radius, bool gatesOnly) {

  return VGAAngular(radius, gatesOnly)
  .run(getCommunicator(true).get(), *pointMapPtr, false);
}

// [[Rcpp::export("Rcpp_VGA_metric")]]
bool vgaMetric(Rcpp::XPtr<PointMap> pointMapPtr,
               double radius, bool gatesOnly) {

  if (radius != -1.0 && radius <= 0) {
    Rcpp::stop("Radius for metric vga must be n (-1) for the whole range or a "
                 "positive number. Got %d", radius);
  }
  return VGAMetric(radius, gatesOnly)
    .run(getCommunicator(true).get(), *pointMapPtr, false);
}

// [[Rcpp::export("Rcpp_VGA_visualGlobal")]]
bool vgaVisualGlobal(Rcpp::XPtr<PointMap> pointMapPtr,
                     int radius, bool gatesOnly) {


  if (radius != -1 && (radius < 1 || radius > 99)) {
    Rcpp::stop("Radius for visibility analysis must be n (-1) for the whole "
                 "range or an integer between 1 and 99 inclusive. Got %i",
                 radius);
  }

  return VGAVisualGlobal(radius, gatesOnly)
    .run(getCommunicator(true).get(), *pointMapPtr, false);
}

// [[Rcpp::export("Rcpp_VGA_visualLocal")]]
bool vgaVisualLocal(Rcpp::XPtr<PointMap> pointMapPtr, bool gatesOnly) {

  return VGAVisualLocal(gatesOnly)
  .run(getCommunicator(true).get(), *pointMapPtr, false);
}

// [[Rcpp::export("Rcpp_VGA_isovist")]]
bool vgaIsovist(Rcpp::XPtr<PointMap> pointMapPtr,
                Rcpp::XPtr<ShapeMap> shapeMapPtr) {

  auto shapeMap = shapeMapPtr->getAllShapes();

  std::vector<SalaShape> shapes;
  shapes.reserve(shapeMap.size());
  for(auto it = shapeMap.begin(); it != shapeMap.end(); ++it) {
    shapes.push_back(it->second);
  }

  return VGAIsovist(shapes)
    .run(getCommunicator(true).get(), *pointMapPtr, false);
}

