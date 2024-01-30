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

#include "attrHelper.h"

#include <Rcpp.h>

namespace { // anonymous

std::string getSFShapeMapExpectedColName(
        int rColIdx,
        std::string colName) {
    return "df_" + std::to_string(rColIdx) + "_" + colName;

}
}
// [[Rcpp::export("RCPP_getSFShapeMapExpectedColName")]]
std::string getSFShapeMapExpectedColName(
        Rcpp::DataFrame &df,
        int rColIdx) {

    auto dfcn = Rcpp::as<const Rcpp::StringVector>(df.attr("names"));
    const int colIdx = rColIdx - 1;
    const std::string &colName = Rcpp::as<std::string>(dfcn.at(colIdx));
    return getSFShapeMapExpectedColName(rColIdx, colName);
}

// [[Rcpp::export("Rcpp_toShapeMap")]]
Rcpp::XPtr<ShapeMap> toShapeMap(
        Rcpp::DataFrame &df,
        Rcpp::Nullable<std::vector<int>> keepColumnIdxsNV = R_NilValue) {

    if (!AttrHelper::hasClass(df, "sf")) {
        Rcpp::stop("Not an sf dataframe");
    }

    auto dfrn = df.attr("row.names");
    if (TYPEOF(dfrn) != INTSXP &&
        TYPEOF(dfrn) != REALSXP) {
        Rcpp::stop("Non-numeric row.names are not supported");
    }

    auto dfcn = Rcpp::as<const Rcpp::StringVector>(df.attr("names"));

    std::vector<int> keepColumnIdxs;
    if (keepColumnIdxsNV.isNotNull()) {
        keepColumnIdxs = Rcpp::as<std::vector<int>>(keepColumnIdxsNV);
    }

    auto geometryColumnIdx = AttrHelper::getGeometryColumnIndex(df);
    const auto &lines = Rcpp::as<Rcpp::GenericVector>(df.at(geometryColumnIdx));

    Rcpp::XPtr<ShapeMap> shp(new ShapeMap("tmp_df_shp"));

    std::vector<std::pair<const int, Rcpp::IntegerVector::const_iterator>> iIts;
    std::vector<std::pair<const int, Rcpp::NumericVector::const_iterator>> rIts;

    { // create the row-names column in the shapemap
        const int rowNameColIdx = shp->addAttribute("df_row_name");
        if (rowNameColIdx == -1) {
            // error adding column (e.g., duplicate column names)
            Rcpp::stop("Error creating df row column");
        }

        switch( TYPEOF(dfrn) ) {
        case INTSXP: {
            const auto &rowNames = Rcpp::as<const Rcpp::IntegerVector>(dfrn);
            iIts.emplace_back(std::make_pair(rowNameColIdx, rowNames.begin()));
            break;
        }
        case REALSXP: {
            const auto &rowNames = Rcpp::as<const Rcpp::NumericVector>(dfrn);
            rIts.emplace_back(std::make_pair(rowNameColIdx, rowNames.begin()));
            break;
        }
        }
    }

    // for any other columns it has been requested, create in ShapeMa
    for (const int rColIdx: keepColumnIdxs) {
        // R indexes start from 1
        const int colIdx = rColIdx - 1;
        const auto &col = df.at(colIdx);
        const std::string &colName = Rcpp::as<std::string>(dfcn.at(colIdx));
        switch( TYPEOF(col) ) {
        case INTSXP: {
            if (Rf_isFactor(col))
                Rcpp::stop("Non-numeric columns are not supported (" +
                    std::to_string(colIdx) + ")");
            int newColIdx = shp->addAttribute(
                getSFShapeMapExpectedColName(rColIdx, colName));

            if (newColIdx == -1) {
                // error adding column (e.g., duplicate column names)
                Rcpp::stop("Error creating df  column");
            }
            const auto &tmp = Rcpp::as<const Rcpp::IntegerVector>(col);
            iIts.emplace_back(std::make_pair(newColIdx, tmp.begin()));
            break;
        }
        case REALSXP: {
            int newColIdx = shp->addAttribute(
                getSFShapeMapExpectedColName(rColIdx, colName));

            if (newColIdx == -1) {
                // error adding column (e.g., duplicate column names)
                Rcpp::stop("Error creating df  column");
            }
            const auto &tmp = Rcpp::as<const Rcpp::NumericVector>(col);
            rIts.emplace_back(std::make_pair(newColIdx, tmp.begin()));
            break;
        }
        case STRSXP: {
            Rcpp::stop("String columns are not supported (" +
                std::to_string(colIdx) + ")");
            break;
        }
        case VECSXP: {
            Rcpp::stop("Non-numeric columns are not supported (" +
                std::to_string(colIdx) + ")");

            break;
        }
        default: {
            Rcpp::stop("incompatible SEXP encountered; only accepts lists"
                           " with REALSXPs, STRSXPs, VECSXPs and INTSXPs");
        }
        }
    }


    for (auto lit = lines.begin(); lit != lines.end(); ++lit) {
        auto coords = Rcpp::as<Rcpp::NumericVector>(*lit);

        if (coords.size() == 4) {
            // 2D line x1,y1,x2,y2
            // TODO: Make this a vector of pairs
            std::map<int, float> extraAttributes;

            for (auto &idxiit: iIts) {
                extraAttributes.emplace(idxiit.first, *idxiit.second);
            }
            for (auto &idxrit: rIts) {
                extraAttributes.emplace(idxrit.first, *idxrit.second);
            }

            Line line(Point2f(coords[0], coords[1]),
                      Point2f(coords[2], coords[3]));
            shp->makeLineShape(
                    line,
                    false /* through_ui */,
                    false /* tempshape */,
                    extraAttributes);
        }
        for (auto &idxiit: iIts) {
            ++idxiit.second;
        }
        for (auto &idxrit: rIts) {
            ++idxrit.second;
        }
    }


    return shp;
}
