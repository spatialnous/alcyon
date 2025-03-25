// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

template <class T> T getAsValidEnum(int value, bool allowNone = false) {
    if (value < static_cast<int>(T::min) || value > static_cast<int>(T::max)) {
        Rcpp::stop("Value " + std::to_string(value) + " out of range (" +
                   std::to_string(static_cast<int>(T::min)) + ", " +
                   std::to_string(static_cast<int>(T::max)) + ")");
    }
    if (!allowNone && value == static_cast<int>(T::None)) {
        Rcpp::stop("None value " + std::to_string(value) + " not allowed");
    }
    return static_cast<T>(value);
}
