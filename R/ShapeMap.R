# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

# A representation of sala's ShapeMap in R. Holds onto a sala Shapemap
# pointer and operates on that

setClass("ShapeMap", slots = c(
  ptr = "externalptr"
))

# lintr seems unable to understand this as a contstructor
# thus we have to exclude it from the particular linter
ShapeMap <- function(name) { # nolint: object_name_linter
  mod <- Rcpp::Module("alcyon_module", "alcyon")
  new("ShapeMap", ptr = mod$makeShapeMap(name))
}

setGeneric("name", function(x) standardGeneric("name"))

setMethod("name", "ShapeMap", function(x) {
  mod <- Rcpp::Module("alcyon_module", "alcyon")
  mod$getName(x@ptr)
})
