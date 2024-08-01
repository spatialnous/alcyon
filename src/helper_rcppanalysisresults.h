// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#pragma once

#include "salalib/analysisresult.h"

#include <Rcpp.h>

template <class T> struct RcppAnalysisResults {
    Rcpp::List result;

    RcppAnalysisResults(T mapPtr) {
        result =
            Rcpp::List::create(Rcpp::Named("completed") = false, Rcpp::Named("cancelled") = false,
                               Rcpp::Named("mapPtr") = mapPtr);
    }

    void setCompleted(bool completed) { result["completed"] = completed; }

    void setAttributes(const std::vector<std::string> &attributes) {
        result["newAttributes"] = attributes;
    }

    void setFromResult(AnalysisResult &&analysisResult) {
        result["completed"] = analysisResult.completed;
        result["newAttributes"] = analysisResult.getAttributes();
    }

    void cancel() {
        result["completed"] = false;
        result["cancelled"] = true;
    }

    Rcpp::List &getData() { return result; }
};
