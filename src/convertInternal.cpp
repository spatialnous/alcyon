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
Rcpp::XPtr<ShapeGraph> toAxialShapeGraph(
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

    // release the unique_ptr so that it's not deleted on scope close
    ShapeGraph *shpgp = axMap.release();

    return Rcpp::XPtr<ShapeGraph>(shpgp);
}


// [[Rcpp::export]]
Rcpp::XPtr<ShapeGraph> axialToSegment(
        Rcpp::XPtr<ShapeGraph> shapeGraph,
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

    double stubremoval = 0.0;
    if (stubremovalNV.isNotNull()) {
        stubremoval = Rcpp::as<bool>(stubremovalNV);
    }

    std::unique_ptr<ShapeGraph> segMap(MapConverter::convertAxialToSegment(
            nullptr,
            *(shapeGraph.get()),
            name,
            keeporiginal,
            copydata,
            stubremoval));

    // release the unique_ptr so that it's not deleted on scope close
    ShapeGraph *shpgp = segMap.release();

    return Rcpp::XPtr<ShapeGraph>(shpgp);
}
