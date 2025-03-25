// SPDX-FileCopyrightText: 2024-2025 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#pragma once

#include "salalib/pointmap.hpp"

#include "helper_rcppanalysisresults.hpp"

#include "communicator.hpp"

namespace RcppRunner {

    template <class T> Rcpp::XPtr<T> copyMapWithRegion(Rcpp::XPtr<T> mapPtr, bool copyMap) {
        if (copyMap) {
            const auto &prevRegion = mapPtr->getRegion();
            auto newMapPtr = Rcpp::XPtr(new T(prevRegion));
            newMapPtr->copy(*mapPtr, true, true);
            return newMapPtr;
        }
        return mapPtr;
    }

    template <class T> Rcpp::XPtr<T> copyMap(Rcpp::XPtr<T> mapPtr, bool copyMap) {
        if (copyMap) {
            auto newMapPtr = Rcpp::XPtr(new T());
            newMapPtr->copy(*mapPtr, ShapeMap::COPY_ALL, true);
            return newMapPtr;
        }
        return mapPtr;
    }

    template <class T>
    Rcpp::List runAnalysis(Rcpp::XPtr<T> &mapPtr, bool progress,
                           std::function<AnalysisResult(Communicator *, Rcpp::XPtr<T> &)> func) {

        RcppAnalysisResults result(mapPtr);

        try {
            result.setFromResult(func(getCommunicator(progress).get(), mapPtr));
        } catch (Communicator::CancelledException &) {
            result.cancel();
        }
        return result.getData();
    }

} // namespace RcppRunner
