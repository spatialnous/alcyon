# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

# A representation of sala's PointMap in R. Holds onto a sala PointMap
# pointer and operates on that

setClass("PointMap", slots = c(
  ptr = "externalptr"
))

# lintr seems unable to understand this as a constructor
# thus we have to exclude it from the particular linter
PointMap <- function(name, gridSize) { # nolint: object_name_linter
  new("PointMap", ptr = Rcpp_PointMap_createFromGrid(name, gridSize))
}

setGeneric("name", function(x) standardGeneric("name"))

setMethod("name", "PointMap", function(x) {
  Rcpp_PointMap_getName(x@ptr)
})
