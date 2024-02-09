# SPDX-FileCopyrightText: 2019 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

isovist <- function(graphFileIn,
                    graphFileOut,
                    x,
                    y,
                    angle = NA,
                    viewangle = NA,
                    cliPath = getDefaultCLILocation(),
                    verbose = FALSE) {
  if (is.na(graphFileOut)) graphFileOut <- graphFileIn
  tmpPtz <- tempfile(fileext = ".csv")
  dt <- data.frame(x = x, y = y)
  targetedIsovist <- FALSE
  suppressWarnings({
    targetedIsovist <- !is.na(angle)
  })
  if (targetedIsovist) {
    dt$angle <- angle
    dt$viewangle <- viewangle
  }
  write.table(dt, tmpPtz, row.names = FALSE, quote = FALSE, sep = ",")

  depthmapXcli(c(
    "-f", formatForCLI(graphFileIn),
    "-o", formatForCLI(graphFileOut),
    "-m", "ISOVIST",
    "-if", formatForCLI(tmpPtz)
  ), cliPath, verbose)
  invisible(file.remove(tmpPtz))
}

isovist2pts <- function(graphFileIn,
                        graphFileOut,
                        x,
                        y,
                        toX,
                        toY,
                        viewangle,
                        cliPath = getDefaultCLILocation(),
                        verbose = FALSE) {
  angles <- 180.0 * atan2(toY - y, toX - x) / pi
  angles <- ifelse(angles < 0.0, 360.0 + angles, angles)
  isovist(graphFileIn, graphFileOut, x, y, angles, viewangle, cliPath, verbose)
}

makeIsovists <- function(graphFilePath,
                         originX,
                         originY,
                         scale = 1.0,
                         cliPath = getDefaultCLILocation(),
                         verbose = FALSE) {
  tmpGraph <- tempfile(fileext = ".graph")
  alcyon::isovist(graphFilePath, tmpGraph, originX, originY)
  alcyon::convertMap(tmpGraph, tmpGraph, "convex")

  tmpMap <- tempfile(fileext = ".mif")
  alcyon::export(tmpGraph, tmpMap, "shapegraph-map-mif")
  isovists <- sf::st_read(tmpMap, geometry_column = 1L, quiet = !verbose)

  file.remove(tmpGraph)
  file.remove(tmpMap)

  return(elide(isovists,
               bb = matrix(c(0.0, 0.0, 1.0, 1.0),
                           ncol = 2L),
               scale = scale))
}
