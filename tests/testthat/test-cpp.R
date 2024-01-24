# Copyright 2023 Petros Koutsolampros
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


context("C++ tests")


test_that("proper formatForCLI output", {
  # aedon::testPrintFromCpp()
  #library(aedon)

  MetaGraph <- Rcpp::Module("metagraph_module", "aedon")$MetaGraph
  b <- new(MetaGraph, "inst/extdata/testdata/gallery/gallery_connected.graph")
  print(b$getName())

  library(aedon)
  mod <- Rcpp::Module("aedon_module", "aedon")
  fileName = "inst/extdata/testdata/barnsbury/barnsburySmall.graph"
  b = mod$getMetaGraph(fileName)
  b
  b[[1]]$getName()

  {
    library(aedon)
    mod = Rcpp::Module("aedon_module", "aedon")
    lineStringMap = st_read("inst/extdata/testdata/barnsbury/barnsbury_small_axial.mif",
                             geometry_column = 1L, quiet = TRUE)

    mod = Rcpp::Module("aedon_module", "aedon")
    shapeMap = mod$toShapeMap(lineStringMap)
    shapeGraph = mod$toAxialShapeGraph(shapeMap);
    attrNames = mod$getAttributeNames(shapeGraph);
    attrNames
    mod$getAttributeData(shapeGraph, attrNames[1]);
    aedon:::runAxialAnalysis(shapeGraph, c(-1));
    mod$getAttributeNames(shapeGraph);
  }


  shp@mod$storage$getName

  shp = aedon::ShapeMap("lala")
  aedon::name(shp)
})

