# SPDX-FileCopyrightText: 2019 Fani Kostourou
# SPDX-FileCopyrightText: 2019 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

linkMapCoords <- function(graphFileIn,
                          graphFileOut,
                          linkFromX,
                          linkFromY,
                          linkToX,
                          linkToY,
                          unlink = FALSE,
                          mapTypeToLink = "pointmaps",
                          cliPath = getDefaultCLILocation(),
                          verbose = FALSE) {
  if (is.na(graphFileOut)) graphFileOut <- graphFileIn
  if (!(mapTypeToLink %in% c("pointmaps", "shapegraphs"))) {
    stop("Unknown map type: ", mapTypeToLink)
  }

  tmpPtz <- tempfile(fileext = ".tsv")
  dt <- data.frame(x1 = linkFromX, y1 = linkFromY, x2 = linkToX, y2 = linkToY)
  write.table(dt, tmpPtz, row.names = FALSE, quote = FALSE, sep = "\t")


  params <- c(
    "-f", formatForCLI(graphFileIn),
    "-o", formatForCLI(graphFileOut),
    "-m", "LINK",
    "-lmt", mapTypeToLink,
    "-lm", ifelse(unlink, "unlink", "link"),
    "-lt", "coords",
    "-lf", tmpPtz
  )


  depthmapXcli(params, cliPath, verbose)
  file.remove(tmpPtz)
}

linkMapRefs <- function(graphFileIn,
                        graphFileOut,
                        linkFrom,
                        linkTo,
                        mapTypeToLink = "pointmaps",
                        unlink = FALSE,
                        cliPath = getDefaultCLILocation(),
                        verbose = FALSE) {
  if (is.na(graphFileOut)) graphFileOut <- graphFileIn
  if (!(mapTypeToLink %in% c("pointmaps", "shapegraphs"))) {
    stop("Unknown map type: ", mapTypeToLink)
  }

  tmpPtz <- tempfile(fileext = ".tsv")
  dt <- data.frame(reffrom = linkFrom, refto = linkTo)
  write.table(dt, tmpPtz, row.names = FALSE, quote = FALSE, sep = "\t")


  params <- c(
    "-f", formatForCLI(graphFileIn),
    "-o", formatForCLI(graphFileOut),
    "-m", "LINK",
    "-lmt", mapTypeToLink,
    "-lm", ifelse(unlink, "unlink", "link"),
    "-lt", "refs",
    "-lf", tmpPtz
  )

  depthmapXcli(params, cliPath, verbose)
  file.remove(tmpPtz)
}
