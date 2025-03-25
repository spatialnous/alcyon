// SPDX-FileCopyrightText: 2024-2025 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#pragma once

#include "salalib/shapemap.hpp"

#include <Rcpp.h>

std::vector<std::string> getShapeMapAttributeNames(ShapeMap *shapeMap);
