// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-or-later

// The values here should be kept the same as the ones in enum.R

#pragma once

#include <Rcpp.h>

enum class Traversal {
  None = 0,
  Angular = 1,
  Tulip = 2,
  Topological = 3,
  Metric = 4
};
