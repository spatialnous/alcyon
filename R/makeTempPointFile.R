# SPDX-FileCopyrightText: 2019 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

makeTempPointFile <- function(pointsX,
                              pointsY,
                              sep = "\t") {
  tmpPtz <- tempfile(fileext = ".csv")
  dt <- data.frame(x = pointsX, y = pointsY)
  write.table(dt, tmpPtz, row.names = FALSE, quote = FALSE, sep = sep)
  tmpPtz
}
