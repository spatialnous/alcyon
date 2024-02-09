# SPDX-FileCopyrightText: 2019 Fani Kostourou
# SPDX-FileCopyrightText: 2019 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

export <- function(graphFileIn,
                   fileOut,
                   exportType,
                   cliPath = getDefaultCLILocation(),
                   verbose = FALSE) {
  depthmapXcli(c(
    "-f", formatForCLI(graphFileIn),
    "-o", formatForCLI(fileOut),
    "-m", "EXPORT",
    "-em", exportType
  ), cliPath, verbose)
}

getPointmapData <- function(graphFileIn,
                            scale = 1.0,
                            cliPath = getDefaultCLILocation(),
                            verbose = FALSE) {
  mapFile <- tempfile(fileext = ".csv")
  alcyon::export(graphFileIn, mapFile, "pointmap-data-csv", cliPath, verbose)
  dpm <- processPointMap(mapFile, scale, ",")
  file.remove(mapFile)
  return(dpm)
}

getPointmapLinks <- function(graphFileIn,
                             cliPath = getDefaultCLILocation(),
                             verbose = FALSE) {
  csvFile <- tempfile(fileext = ".csv")
  alcyon::export(graphFileIn, csvFile, "pointmap-links-csv", cliPath, verbose)
  links <- read.csv(csvFile)
  file.remove(csvFile)
  return(links)
}


getPointmapDataAndLinks <- function(graphFileIn,
                                    scale = 1.0,
                                    cliPath = getDefaultCLILocation(),
                                    verbose = FALSE) {
  mapFile <- tempfile(fileext = ".csv")
  alcyon::export(graphFileIn, mapFile, "pointmap-data-csv")
  linkFile <- tempfile(fileext = ".csv")
  alcyon::export(graphFileIn, linkFile, "pointmap-links-csv")
  dpm <- processPointMapAndLinks(mapFile, linkFile, scale, ",")
  file.remove(mapFile)
  file.remove(linkFile)
  return(dpm)
}

processPointMap <- function(mapPath,
                            scale = 1.0,
                            sep = "\t") {
  pointMapData <- read.csv(mapPath, sep = sep)
  pointMapData$x <- pointMapData$x * scale
  pointMapData$y <- pointMapData$y * scale
  pointMapData <- cbind(pointMapData, refIDtoIndex(pointMapData$Ref))
  xycols <- c("x", "y")
  pointMapPoints <- pointMapData[, xycols]
  pointMapData <- pointMapData[, !(names(pointMapData) %in% xycols)]
  dpm <- SpatialPointsDataFrame(pointMapPoints, data = pointMapData)
  suppressWarnings({
    gridded(dpm) <- TRUE
  })
  return(list(map = dpm, importScale = scale))
}

processPointMapAndLinks <- function(mapPath,
                                    linkPath = NA,
                                    scale = 1.0,
                                    sep = "\t") {
  pointMap <- processPointMap(mapPath, scale, sep)
  pointMap$links <- NA
  if (!is.na(linkPath)) {
    pointMap$links <- read.csv(linkPath, sep = sep)
  }
  return(pointMap)
}

getShapeGraph <- function(graphFileIn,
                          cliPath = getDefaultCLILocation(),
                          verbose = FALSE) {
  mapFile <- tempfile(fileext = ".mif")
  alcyon::export(graphFileIn, mapFile, "shapegraph-map-mif", cliPath, verbose)
  ogr <- sf::st_read(mapFile, geometry_column = 1L, quiet = !verbose)
  file.remove(mapFile)
  return(ogr)
}

getShapeGraphConnections <- function(graphFileIn,
                                     cliPath = getDefaultCLILocation(),
                                     verbose = FALSE) {
  connectionsFile <- tempfile(fileext = ".csv")
  alcyon::export(
    graphFileIn, connectionsFile, "shapegraph-connections-csv",
    cliPath, verbose
  )
  csv <- read.table(connectionsFile, header = TRUE, sep = ",")
  file.remove(connectionsFile)
  return(csv)
}

getShapeGraphLinksUnlinks <- function(graphFileIn,
                                      cliPath = getDefaultCLILocation(),
                                      verbose = FALSE) {
  linksunlinksFile <- tempfile(fileext = ".csv")
  alcyon::export(
    graphFileIn, linksunlinksFile, "shapegraph-links-unlinks-csv",
    cliPath, verbose
  )
  csv <- read.table(linksunlinksFile, header = TRUE, sep = ",")
  file.remove(linksunlinksFile)
  return(csv)
}
