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
        Rcpp::stop("Dataframe does not have the attribute \"" + cl + "\"");
    }
    return Rcpp::as<Rcpp::StringVector>(df.attr(cl));
}


int getGeometryColumnIndex(Rcpp::DataFrame &df) {
    return df.findName(Rcpp::as<std::string>(
            *getStringVectorAttr(df, "sf_column").begin()));
}
}
