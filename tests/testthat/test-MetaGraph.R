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

context("MetaGraph tests")

test_that("Load data from MetaGraph", {
  fileName <- system.file(
    "extdata", "testdata", "barnsbury", "barnsburySmall.graph",
    package = "alcyon"
  )
  metaGraphData <- Rcpp_MetaGraph_read(fileName)
  expect_length(metaGraphData$shapeMaps, 1L)
  expect_length(metaGraphData$shapeGraphs, 1L)
  expect_length(metaGraphData$pointMaps, 0L)
})
