// Copyright 2024 Petros Koutsolampros
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program. If not, see <https://www.gnu.org/licenses/>.

#include "salalib/shapemap.h"
#include "salalib/shapegraph.h"
#include "salalib/mapconverter.h"

#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::XPtr<ShapeGraph> toAxialShapeGraph(Rcpp::XPtr<ShapeMap> shapeMap) {
    std::unique_ptr<ShapeGraph> axMap(MapConverter::convertDataToAxial(
            nullptr, "ax_map", *(shapeMap.get()), true));

    // release the unique_ptr so that it's not deleted on scope close
    ShapeGraph *shpgp = axMap.release();

    return Rcpp::XPtr<ShapeGraph>(shpgp);
}
