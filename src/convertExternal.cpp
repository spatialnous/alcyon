// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "salalib/shapemap.h"
#include "salalib/shapegraph.h"

#include "attrHelper.h"

#include <Rcpp.h>

namespace { // anonymous

std::string getSfShapeMapExpectedColName(
        int rColIdx,
        std::string colName) {
    return "df_" + std::to_string(rColIdx) + "_" + colName;
}

} // anonymous

// [[Rcpp::export("Rcpp_getSfShapeMapExpectedColName")]]
std::string getSfShapeMapExpectedColName(
        Rcpp::DataFrame &df,
        int rColIdx) {

    auto dfcn = Rcpp::as<const Rcpp::StringVector>(df.attr("names"));
    const int colIdx = rColIdx - 1;
    const std::string &colName = Rcpp::as<std::string>(dfcn.at(colIdx));
    return getSfShapeMapExpectedColName(rColIdx, colName);
}

// [[Rcpp::export("Rcpp_getAxialToSegmentExpectedColName")]]
std::string getAxialToSegmentExpectedColName(
        std::string &colName) {
    return "Axial " + colName;
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
    const auto &geom = Rcpp::as<Rcpp::GenericVector>(df.at(geometryColumnIdx));

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
                Rcpp::stop("Non-numeric columns are not supported (%d: %s)",
                           colIdx, colName);
            int newColIdx = shp->addAttribute(
                getSfShapeMapExpectedColName(rColIdx, colName));

            if (newColIdx == -1) {
                // error adding column (e.g., duplicate column names)
                Rcpp::stop("Error creating df column (%d: %s)",
                           colIdx, colName);
            }
            const auto &tmp = Rcpp::as<const Rcpp::IntegerVector>(col);
            iIts.emplace_back(std::make_pair(newColIdx, tmp.begin()));
            break;
        }
        case REALSXP: {
            int newColIdx = shp->addAttribute(
                getSfShapeMapExpectedColName(rColIdx, colName));

            if (newColIdx == -1) {
                // error adding column (e.g., duplicate column names)
                Rcpp::stop("Error creating df column (%d: %s)",
                           colIdx, colName);
            }
            const auto &tmp = Rcpp::as<const Rcpp::NumericVector>(col);
            rIts.emplace_back(std::make_pair(newColIdx, tmp.begin()));
            break;
        }
        case STRSXP: {
            Rcpp::stop("String columns are not supported (%d: %s)",
                       colIdx, colName);
            break;
        }
        case VECSXP: {
            Rcpp::stop("Non-numeric columns are not supported (%d: %s)",
                       colIdx, colName);

            break;
        }
        default: {
            Rcpp::stop("incompatible SEXP encountered; only accepts lists"
                           " with REALSXPs, STRSXPs, VECSXPs and INTSXPs");
        }
        }
    }

    for (auto git = geom.begin(); git != geom.end(); ++git) {
        Rcpp::NumericMatrix coords;
        if(TYPEOF(*git) == VECSXP) {
            // multi-object item
            auto multiObject = Rcpp::as<Rcpp::GenericVector>(*git);
            // for the moment only get the first
            coords = Rcpp::as<Rcpp::NumericMatrix>(multiObject[0]);
        } else {
            coords = Rcpp::as<Rcpp::NumericMatrix>(*git);
        }

        // TODO: Make this a vector of pairs
        std::map<int, float> extraAttributes;

        for (auto &idxiit: iIts) {
            extraAttributes.emplace(idxiit.first, *idxiit.second);
        }
        for (auto &idxrit: rIts) {
            extraAttributes.emplace(idxrit.first, *idxrit.second);
        }

        if (coords.rows() == 1) {
            // 2D point x1,y1
            Point2f point(coords[0], coords[1]);
            shp->makePointShape(
                    point,
                    false /* tempshape */,
                    extraAttributes);
        } else if (coords.rows() == 2) {
            // 2D line x1,x2,y1,y2

            Line line(Point2f(coords[0], coords[2]),
                      Point2f(coords[1], coords[3]));
            shp->makeLineShape(
                    line,
                    false /* through_ui */,
                    false /* tempshape */,
                    extraAttributes);
        } else if (coords.rows() > 2) {
            // 2D polygon x1,x2,y1,y2
            // TODO: Make this a vector of pairs

            std::vector<Point2f> points;
            for (int rowIdx = 0; rowIdx < coords.rows(); ++rowIdx) {
                const Rcpp::NumericMatrix::Row &row = coords(rowIdx, Rcpp::_);
                points.push_back(Point2f(row[0], row[1]));
            }

            shp->makePolyShape(
                    points,
                    false /* open */,
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
