// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#pragma once

#include <Rcpp.h>

#include <optional>

namespace NullableValue {
    template <class T> T get(const Rcpp::Nullable<T> nv, T defaultValue) {
        T value = defaultValue;
        if (nv.isNotNull()) {
            value = Rcpp::as<T>(nv);
        }
        return value;
    }

    template <class T> std::optional<T> getOptional(const Rcpp::Nullable<T> nv) {
        std::optional<T> value = std::nullopt;
        if (nv.isNotNull()) {
            value = Rcpp::as<T>(nv);
        }
        return value;
    }

    template <class T> T castIntGet(const Rcpp::Nullable<int> nv, T defaultValue) {
        T value = defaultValue;
        if (nv.isNotNull()) {
            value = static_cast<T>(Rcpp::as<int>(nv));
        }
        return value;
    }
} // namespace NullableValue
