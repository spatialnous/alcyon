// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "salalib/shapemap.h"
#include "salalib/alllinemap.h"

#include "communicator.h"

#include <Rcpp.h>
#include <memory>

// [[Rcpp::export("Rcpp_makeAllLineMap")]]
Rcpp::XPtr<AllLineMap> makeAllLineMap(Rcpp::XPtr<ShapeMap> boundsMap,
                                      double seedX,
                                      double seedY) {

    Rcpp::XPtr<AllLineMap> map(new AllLineMap);
    std::vector<Line> lines;

    for (const auto &line : boundsMap->getAllShapesAsLines()) {
        lines.push_back(Line(line.start(), line.end()));
    }

    QtRegion region(boundsMap->getRegion());

    map->generate(getCommunicator(true).get(),
                  lines,
                  region,
                  Point2f(seedX, seedY));
    return map;
}

// [[Rcpp::export("Rcpp_extractFewestLineMaps")]]
Rcpp::List extractFewestLineMaps(Rcpp::XPtr<AllLineMap> allLineMap) {
    auto [fewestlinemap_subsets, fewestlinemap_minimal] =
        allLineMap->extractFewestLineMaps(getCommunicator(true).get());

    return Rcpp::List::create(
        Rcpp::Named("Fewest-Line Map (Subsets)") =
            Rcpp::XPtr<ShapeGraph>(
                fewestlinemap_subsets.release(), true),
        Rcpp::Named("Fewest-Line Map (Minimal)") =
            Rcpp::XPtr<ShapeGraph>(
                fewestlinemap_minimal.release(), true)
    );
}
