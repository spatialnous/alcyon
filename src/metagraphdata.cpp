// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "salalib/shapemap.h"
#include "salalib/shapegraph.h"
#include "salalib/pointdata.h"
#include "salalib/mgraph.h"

#include <Rcpp.h>

// [[Rcpp::export("Rcpp_MetaGraph_read")]]
Rcpp::List readMetaGraph(
    std::string fileName,
    Rcpp::Nullable<bool> verboseNV = R_NilValue) {

  bool verbose = false;
  if (verboseNV.isNotNull()) {
    verbose = Rcpp::as<bool>(verboseNV);
  }

  if (verbose) {
    Rcpp::Rcout << "Loading MetaGraph at: " << fileName << std::endl;
  }
  auto m = std::unique_ptr<MetaGraph>(new MetaGraph(fileName));
  m->readFromFile(fileName);
  if (verbose) {
    Rcpp::Rcout << "- bb: " << m->getBoundingBox().area() << std::endl;
  }

  Rcpp::GenericVector shapeMaps;
  Rcpp::GenericVector shapeGraphs;
  Rcpp::GenericVector pointMaps;

  for (auto & drawingFile : m->m_drawingFiles) {
    if (verbose) {
      Rcpp::Rcout << " - drawingName: "
                  << drawingFile.getName() << std::endl;
    }
    for (auto it = drawingFile.m_spacePixels.begin();
         it != drawingFile.m_spacePixels.end(); ++it) {
      if (verbose) {
        Rcpp::Rcout << " - shapeMap name: "
                    << it->getName() << std::endl;
      }
      shapeMaps.push_back(
        Rcpp::List::create(
          Rcpp::Named("group") = "shape_" + drawingFile.getName(),
          Rcpp::Named("ptr") =
            Rcpp::XPtr<ShapeMap>(
              new ShapeMap(std::move(*it)), true)
        )
      );
    }
  }
  for (auto it = m->getDataMaps().begin();
       it != m->getDataMaps().end(); ++it) {
    if (verbose) {
      Rcpp::Rcout << " - dataMap name: "
                  << it->getName() << std::endl;
    }
    shapeMaps.push_back(
      Rcpp::List::create(
        Rcpp::Named("group") = "data",
        Rcpp::Named("ptr") =
          Rcpp::XPtr<ShapeMap>(
            new ShapeMap(std::move(*it)), true)
      )
    );
  }
  for (auto it = m->getShapeGraphs().begin();
       it != m->getShapeGraphs().end(); ++it) {
    if (verbose) {
      Rcpp::Rcout << " - dataMap name: "
                  << (*it)->getName() << std::endl;
    }
    std::string mapType = "none";
    switch((*it)->getMapType()) {
    case ShapeMap::AXIALMAP:
      mapType = "axial";
      break;
    case ShapeMap::ALLLINEMAP:
      mapType = "allline";
      break;
    case ShapeMap::SEGMENTMAP:
      mapType = "segment";
      break;
    case ShapeMap::CONVEXMAP:
      mapType = "convex";
      break;
    case ShapeMap::PESHMAP:
      mapType = "pesh";
      break;
    }
    shapeGraphs.push_back(
      Rcpp::List::create(
        Rcpp::Named("type") = mapType,
        Rcpp::Named("ptr") =
          Rcpp::XPtr<ShapeGraph>(
            std::move(*it).release(), true)
      )
    );
  }
  for (auto it = m->getPointMaps().begin();
       it != m->getPointMaps().end(); ++it) {
    if (verbose) {
      Rcpp::Rcout << " - dataMap name: "
                  << it->getName() << std::endl;
    }
    pointMaps.push_back(
      Rcpp::List::create(
        Rcpp::Named("ptr") =
          Rcpp::XPtr<PointMap>(
            new PointMap(std::move(*it)), true)
      )
    );
  }

  return Rcpp::List::create(Rcpp::Named("shapeMaps") = shapeMaps,
                            Rcpp::Named("shapeGraphs") = shapeGraphs,
                            Rcpp::Named("pointMaps") = pointMaps);
}
