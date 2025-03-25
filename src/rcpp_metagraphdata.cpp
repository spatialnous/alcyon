// SPDX-FileCopyrightText: 2024-2025 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "salalib/metagraphreadwrite.hpp"
#include "salalib/pointmap.hpp"
#include "salalib/shapegraph.hpp"
#include "salalib/shapemap.hpp"

#include "helper_nullablevalue.hpp"

#include <Rcpp.h>

// [[Rcpp::export("Rcpp_MetaGraph_read")]]
Rcpp::List readMetaGraph(std::string fileName, const Rcpp::Nullable<bool> verboseNV = R_NilValue) {

    auto verbose = NullableValue::get(verboseNV, false);

    if (verbose) {
        Rcpp::Rcout << "Loading MetaGraph at: " << fileName << std::endl;
    }
    auto mgd = MetaGraphReadWrite::readFromFile(fileName);
    if (verbose) {
        Rcpp::Rcout << "- bb: " << mgd.metaGraph.region.area() << std::endl;
    }

    Rcpp::GenericVector shapeMaps;
    Rcpp::GenericVector shapeGraphs;
    Rcpp::GenericVector pointMaps;

    for (auto &drawingFile : mgd.drawingFiles) {
        if (verbose) {
            Rcpp::Rcout << " - drawingName: " << drawingFile.first.name << std::endl;
        }
        for (auto it = drawingFile.second.begin(); it != drawingFile.second.end(); ++it) {
            if (verbose) {
                Rcpp::Rcout << " - shapeMap name: " << it->getName() << std::endl;
            }
            shapeMaps.push_back(Rcpp::List::create(
                Rcpp::Named("group") = "shape_" + drawingFile.first.name,
                Rcpp::Named("ptr") = Rcpp::XPtr<ShapeMap>(new ShapeMap(std::move(*it)), true)));
        }
    }
    for (auto it = mgd.dataMaps.begin(); it != mgd.dataMaps.end(); ++it) {
        if (verbose) {
            Rcpp::Rcout << " - dataMap name: " << it->getName() << std::endl;
        }
        shapeMaps.push_back(Rcpp::List::create(
            Rcpp::Named("group") = "data",
            Rcpp::Named("ptr") = Rcpp::XPtr<ShapeMap>(new ShapeMap(std::move(*it)), true)));
    }
    for (auto it = mgd.shapeGraphs.begin(); it != mgd.shapeGraphs.end(); ++it) {
        if (verbose) {
            Rcpp::Rcout << " - dataMap name: " << it->getName() << std::endl;
        }
        std::string mapType = "none";
        switch (it->getMapType()) {
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
        shapeGraphs.push_back(Rcpp::List::create(
            Rcpp::Named("type") = mapType,
            Rcpp::Named("ptr") = Rcpp::XPtr<ShapeGraph>(new ShapeGraph(std::move(*it)), true)));
    }
    for (auto it = mgd.pointMaps.begin(); it != mgd.pointMaps.end(); ++it) {
        if (verbose) {
            Rcpp::Rcout << " - dataMap name: " << it->getName() << std::endl;
        }
        pointMaps.push_back(Rcpp::List::create(
            Rcpp::Named("ptr") = Rcpp::XPtr<PointMap>(new PointMap(std::move(*it)), true)));
    }

    return Rcpp::List::create(Rcpp::Named("shapeMaps") = shapeMaps,
                              Rcpp::Named("shapeGraphs") = shapeGraphs,
                              Rcpp::Named("pointMaps") = pointMaps);
}
