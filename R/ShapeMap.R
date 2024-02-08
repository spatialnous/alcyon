# Copyright 2024 Petros Koutsolampros
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <https://www.gnu.org/licenses/>.

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
