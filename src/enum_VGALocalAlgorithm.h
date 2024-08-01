// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

// The values here should be kept the same as the ones in vgaLocal.R

#pragma once

#include <Rcpp.h>

enum class VGALocalAlgorithm {
    None = 0,
    Standard = 1,
    AdjacencyMatrix = 2,
    // remember to change maximum if adding values here
    min = None,
    max = AdjacencyMatrix
};
