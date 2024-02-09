# SPDX-FileCopyrightText: 2019 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

refIDtoIndex <- function(refID) {
  i <- bitwShiftR(refID, 16L)
  j <- bitwAnd(refID, 0x0000FFFF)
  return(data.frame(i, j))
}

indexToRefID <- function(i,
                         j) {
  return(bitwShiftL(i, 16L) + j)
}
