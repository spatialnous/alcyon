// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0

// The values here should be kept the same as the ones in TraversalType.R

#pragma once

#include <Rcpp.h>

enum class TraversalType {
  None = 0,
  Angular = 1,
  Topological = 2,
  Metric = 3
};
