// SPDX-FileCopyrightText: 2024-2025 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "rcpp_ShapeMap.hpp"

#include "salalib/mapconverter.hpp"
#include "salalib/shapegraph.hpp"
#include "salalib/shapemap.hpp"

#include "helper_nullablevalue.hpp"

#include "communicator.hpp"

#include <Rcpp.h>

// [[Rcpp::export("Rcpp_toAxialShapeGraph")]]
Rcpp::List toAxialShapeGraph(Rcpp::XPtr<ShapeMap> shapeMap,
                             const Rcpp::Nullable<std::string> nameNV = R_NilValue,
                             const Rcpp::Nullable<bool> copydataNV = R_NilValue,
                             const Rcpp::Nullable<bool> progressNV = R_NilValue) {

    auto name = NullableValue::get(nameNV, std::string("ax_map"));
    auto copydata = NullableValue::get(copydataNV, true);
    auto progress = NullableValue::get(progressNV, false);

    std::unique_ptr<ShapeGraph> axMap(MapConverter::convertDataToAxial(
        getCommunicator(progress).get(), name, *(shapeMap.get()), copydata));

    auto shapeMapNames = getShapeMapAttributeNames(shapeMap.get());
    auto newNames = getShapeMapAttributeNames(axMap.get());

    for (const auto &name : shapeMapNames) {
        auto axIt = std::find(newNames.begin(), newNames.end(), name);
        if (axIt != newNames.end()) {
            newNames.erase(axIt);
        }
    }

    Rcpp::List result =
        Rcpp::List::create(Rcpp::Named("completed") = true, Rcpp::Named("newAttributes") = newNames,
                           // release the unique_ptr so that it's not deleted on scope close
                           Rcpp::Named("mapPtr") = Rcpp::XPtr<ShapeGraph>(axMap.release()));

    return result;
}

// [[Rcpp::export("Rcpp_axialToSegment")]]
Rcpp::XPtr<ShapeGraph> axialToSegment(Rcpp::XPtr<ShapeGraph> shapeGraph,
                                      const Rcpp::Nullable<std::string> nameNV = R_NilValue,
                                      const Rcpp::Nullable<bool> copydataNV = R_NilValue,
                                      const Rcpp::Nullable<double> stubremovalNV = R_NilValue,
                                      const Rcpp::Nullable<bool> progressNV = R_NilValue) {

    auto name = NullableValue::get(nameNV, std::string("seg_map"));
    auto copydata = NullableValue::get(copydataNV, true);
    auto stubremoval = NullableValue::get(stubremovalNV, 0.0);
    auto progress = NullableValue::get(progressNV, false);

    // keepOriginal - will try to remove it from shapeGraphs, but since
    // this is a plain conversion it's not necessary
    std::unique_ptr<ShapeGraph> segMap(MapConverter::convertAxialToSegment(
        getCommunicator(progress).get(), *(shapeGraph.get()), name,
        true, // keepOriginal
        copydata, stubremoval));

    return Rcpp::XPtr<ShapeGraph>(segMap.release());
}

// [[Rcpp::export("Rcpp_shapeMapToSegment")]]
Rcpp::List shapeMapToSegment(Rcpp::XPtr<ShapeMap> shapeMap,
                             const Rcpp::Nullable<std::string> nameNV = R_NilValue,
                             const Rcpp::Nullable<bool> copydataNV = R_NilValue,
                             const Rcpp::Nullable<bool> progressNV = R_NilValue) {

    auto name = NullableValue::get(nameNV, std::string("seg_map"));
    auto copydata = NullableValue::get(copydataNV, true);
    auto progress = NullableValue::get(progressNV, false);

    auto segMap = MapConverter::convertDataToSegment(getCommunicator(progress).get(), name,
                                                     *(shapeMap.get()), copydata);

    auto shapeMapNames = getShapeMapAttributeNames(shapeMap.get());
    auto newNames = getShapeMapAttributeNames(segMap.get());
    for (const auto &name : shapeMapNames) {
        auto axIt = std::find(newNames.begin(), newNames.end(), name);
        if (axIt != newNames.end()) {
            newNames.erase(axIt);
        }
    }

    Rcpp::List result =
        Rcpp::List::create(Rcpp::Named("completed") = true, Rcpp::Named("newAttributes") = newNames,
                           // release the unique_ptr so that it's not deleted on scope close
                           Rcpp::Named("mapPtr") = Rcpp::XPtr<ShapeGraph>(segMap.release()));

    return result;
}
