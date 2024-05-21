# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

#' ShapeGraph
#'
#' A representation of sala's ShapeGraph in R. Holds onto a sala ShapeGraph
#' pointer and operates on that
#' @importFrom methods setClass
setClass("ShapeGraph", contains = "ShapeMap")
