// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#pragma once

#include <Rcpp.h>

namespace AttrHelper {

bool hasClass(Rcpp::DataFrame &df, std::string cl) {
    if (df.hasAttribute("class") &&
        TYPEOF(df.attr("class")) == STRSXP) {
        // has a class attribute which is a string vector
        auto classData = Rcpp::as<Rcpp::StringVector>(df.attr("class"));
        return std::find(
            classData.begin(), classData.end(),
            cl) != classData.end();
    }
    return false;
}

Rcpp::StringVector getStringVectorAttr(Rcpp::DataFrame &df, std::string cl) {

    if (!df.hasAttribute(cl)) {
        Rcpp::stop("Dataframe does not have the attribute %s", cl);
    }
    return Rcpp::as<Rcpp::StringVector>(df.attr(cl));
}


int getGeometryColumnIndex(Rcpp::DataFrame &df) {
    return df.findName(Rcpp::as<std::string>(
            *getStringVectorAttr(df, "sf_column").begin()));
}
}
