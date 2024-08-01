// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include <Rcpp.h>
#include <typeinfo>

// debian 12 install:
// sudo apt install libudunits2-dev libproj-dev libgdal-dev

// [[Rcpp::plugins(cpp17)]]

// sample function showing how to extract data from a dataframe
void exploreDF(Rcpp::DataFrame &df) {

    for (auto it = df.begin(); it != df.end(); it++) {
        // https://gallery.rcpp.org/articles/rcpp-wrap-and-recurse/
        // #define INTSXP      13    /* integer vectors */
        // #define REALSXP     14    /* real variables */
        // #define CPLXSXP     15    /* complex variables */
        // #define STRSXP      16    /* string vectors */
        // #define DOTSXP      17    /* dot-dot-dot object */
        // #define ANYSXP      18    /* make "any" args work. */
        // #define VECSXP      19    /* generic vectors */
        switch (TYPEOF(*it)) {
        case REALSXP: {
            auto tmp = Rcpp::as<Rcpp::NumericVector>(*it);

            // std::cout << "REALSXP" << std::endl;
            // for (auto attrName : tmp.attributeNames()) {
            //   std::cout << attrName << ": " << std::endl;
            //   //Rcpp::print(tmp.attr(attrName));
            // }
            break;
        }
        case STRSXP: {
            auto tmp = Rcpp::as<Rcpp::StringVector>(*it);

            // std::cout << "REALSXP" << std::endl;
            // for (auto attrName : tmp.attributeNames()) {
            //   std::cout << attrName << ": " << std::endl;
            //   //Rcpp::print(tmp.attr(attrName));
            // }
            break;
        }
        case INTSXP: {
            if (Rf_isFactor(*it))
                break; // factors have internal type INTSXP
            auto tmp = Rcpp::as<Rcpp::IntegerVector>(*it);

            // std::cout << "INTSXP" << std::endl;
            // print(tmp);
            // for (auto attrName : tmp.attributeNames()) {
            //   std::cout << attrName << ": " << std::endl;
            //   //Rcpp::print(tmp.attr(attrName));
            // }
            break;
        }
        case VECSXP: {
            // generic vector
            auto gv = Rcpp::as<Rcpp::GenericVector>(*it);
            if (gv.hasAttribute("class") && TYPEOF(gv.attr("class")) == STRSXP) {
                // has a class attribute which is a string vector
                auto classData = Rcpp::as<Rcpp::StringVector>(gv.attr("class"));
                bool isLineString = std::find(classData.begin(), classData.end(),
                                              "sfc_LINESTRING") != classData.end();

                if (isLineString) {
                    // do stuff with line vector
                }
            }
            break;
        }
        default: {
            Rcpp::stop("incompatible SEXP encountered; only accepts lists"
                       " with REALSXPs, STRSXPs, VECSXPs and INTSXPs");
        }
        }
    }
}
