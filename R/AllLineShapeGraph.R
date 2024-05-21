# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

#' All-line Axial ShapeGraph
#'
#' A representation of sala's All-line ShapeGraph in R. Holds onto a sala
#' All-line ShapeGraph pointer and operates on that
#' @importFrom methods setClass
setClass("AllLineShapeGraph", contains = "AxialShapeGraph")
