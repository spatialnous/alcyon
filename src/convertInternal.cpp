// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "ShapeMap.h"

#include "salalib/shapemap.h"
#include "salalib/shapegraph.h"
#include "salalib/mapconverter.h"

#include <Rcpp.h>

// [[Rcpp::export("Rcpp_toAxialShapeGraph")]]
Rcpp::List toAxialShapeGraph(
        Rcpp::XPtr<ShapeMap> shapeMap,
        Rcpp::Nullable<std::string> nameNV = R_NilValue,
        Rcpp::Nullable<bool> copydataNV = R_NilValue) {

    std::string name = "ax_map";
    if (nameNV.isNotNull()) {
        name = Rcpp::as<std::string>(nameNV);
    }
    bool copydata = true;
    if (copydataNV.isNotNull()) {
        copydata = Rcpp::as<bool>(copydataNV);
    }

    std::unique_ptr<ShapeGraph> axMap(MapConverter::convertDataToAxial(
            nullptr, name, *(shapeMap.get()), copydata));

    auto shapeMapNames = getShapeMapAttributeNames(shapeMap.get());
    auto newNames = getShapeMapAttributeNames(axMap.get());

    for(const auto &name: shapeMapNames) {
        auto axIt = std::find(newNames.begin(), newNames.end(), name);
        if (axIt != newNames.end()) {
          newNames.erase(axIt);
        }
    }

    Rcpp::List result = Rcpp::List::create(
        Rcpp::Named("completed") = true,
        Rcpp::Named("newAttributes") = newNames,
        // release the unique_ptr so that it's not deleted on scope close
        Rcpp::Named("mapPtr") = Rcpp::XPtr<ShapeGraph>(axMap.release())
    );

    return result;
}


// [[Rcpp::export("Rcpp_axialToSegment")]]
Rcpp::XPtr<ShapeGraph> axialToSegment(
        Rcpp::XPtr<ShapeGraph> shapeGraph,
        Rcpp::Nullable<std::string> nameNV = R_NilValue,
        Rcpp::Nullable<bool> copydataNV = R_NilValue,
        Rcpp::Nullable<double> stubremovalNV = R_NilValue) {

    std::string name = "seg_map";
    if (nameNV.isNotNull()) {
        name = Rcpp::as<std::string>(nameNV);
    }

    bool copydata = true;
    if (copydataNV.isNotNull()) {
        copydata = Rcpp::as<bool>(copydataNV);
    }

    double stubremoval = 0.0;
    if (stubremovalNV.isNotNull()) {
        stubremoval = Rcpp::as<bool>(stubremovalNV);
    }

    // keepOriginal - will try to remove it from shapeGraphs, but since
    // this is a plain conversion it's not necessary
    std::unique_ptr<ShapeGraph> segMap(MapConverter::convertAxialToSegment(
            nullptr,
            *(shapeGraph.get()),
            name,
            true, // keepOriginal
            copydata,
            stubremoval));

    return Rcpp::XPtr<ShapeGraph>(segMap.release());
}


// [[Rcpp::export("Rcpp_shapeMapToSegment")]]
Rcpp::List shapeMapToSegment(
        Rcpp::XPtr<ShapeMap> shapeMap,
        Rcpp::Nullable<std::string> nameNV = R_NilValue,
        Rcpp::Nullable<bool> keeporiginalNV = R_NilValue,
        Rcpp::Nullable<bool> copydataNV = R_NilValue,
        Rcpp::Nullable<double> stubremovalNV = R_NilValue) {

    std::string name = "seg_map";
    if (nameNV.isNotNull()) {
        name = Rcpp::as<std::string>(nameNV);
    }

    bool keeporiginal = true;
    if (keeporiginalNV.isNotNull()) {
        keeporiginal = Rcpp::as<bool>(keeporiginalNV);
    }

    bool copydata = true;
    if (copydataNV.isNotNull()) {
        copydata = Rcpp::as<bool>(copydataNV);
    }

    bool converted = true;

    auto segMap =
        MapConverter::convertDataToSegment(
            nullptr,
            name,
            *(shapeMap.get()),
            copydata
        );

    auto shapeMapNames = getShapeMapAttributeNames(shapeMap.get());
    auto newNames = getShapeMapAttributeNames(segMap.get());
    for(const auto &name: shapeMapNames) {
      auto axIt = std::find(newNames.begin(), newNames.end(), name);
      if (axIt != newNames.end()) {
        newNames.erase(axIt);
      }
    }

    Rcpp::List result = Rcpp::List::create(
        Rcpp::Named("completed") = true,
        Rcpp::Named("newAttributes") = newNames,
        // release the unique_ptr so that it's not deleted on scope close
        Rcpp::Named("mapPtr") = Rcpp::XPtr<ShapeGraph>(segMap.release())
    );

    return result;
}
