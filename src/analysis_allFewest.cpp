// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "salalib/alllinemap.h"
#include "salalib/shapemap.h"

#include "helper_nullablevalue.h"

#include "communicator.h"

#include <Rcpp.h>
#include <memory>

// [[Rcpp::export("Rcpp_makeAllLineMap")]]
Rcpp::List makeAllLineMap(Rcpp::XPtr<ShapeMap> boundsMap, double seedX, double seedY,
                          const Rcpp::Nullable<bool> progressNV = R_NilValue) {
    auto progress = NullableValue::get(progressNV, false);

    Rcpp::XPtr<ShapeGraph> map(new ShapeGraph(AllLine::createAllLineMap()));
    std::vector<Line> lines;

    for (const auto &line : boundsMap->getAllShapesAsLines()) {
        lines.push_back(Line(line.start(), line.end()));
    }

    QtRegion region(boundsMap->getRegion());

    auto mapData = Rcpp::XPtr<AllLine::MapData>(new AllLine::MapData(AllLine::generate(
        getCommunicator(progress).get(), *map.get(), lines, region, Point2f(seedX, seedY))));

    return Rcpp::List::create(Rcpp::Named("allLineMap") = map, Rcpp::Named("mapData") = mapData);
}

// [[Rcpp::export("Rcpp_extractFewestLineMaps")]]
Rcpp::List extractFewestLineMaps(Rcpp::XPtr<ShapeGraph> allLineMap,
                                 Rcpp::XPtr<AllLine::MapData> mapData,
                                 const Rcpp::Nullable<bool> progressNV = R_NilValue) {
    auto progress = NullableValue::get(progressNV, false);

    auto [fewestlinemap_subsets, fewestlinemap_minimal] =
        AllLine::extractFewestLineMaps(getCommunicator(progress).get(), *allLineMap, *mapData);

    return Rcpp::List::create(Rcpp::Named("Fewest-Line Map (Subsets)") = Rcpp::XPtr<ShapeGraph>(
                                  new ShapeGraph(std::move(fewestlinemap_subsets)), true),
                              Rcpp::Named("Fewest-Line Map (Minimal)") = Rcpp::XPtr<ShapeGraph>(
                                  new ShapeGraph(std::move(fewestlinemap_minimal)), true));
}
