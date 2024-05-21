# SPDX-FileCopyrightText: 2019-2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

#' Ref ID to index and vice-versa
#'
#' Converts a depthmapX "Ref" ID to the indices (x, y) of a cell, or the reverse
#'
#' @name refIdToIndexAndBack
#' @param refID The Ref ID
#' @param i The x-axis index of the cell
#' @param j The y-axis index of the cell
#' @returns A pair of indices (x, y) or a Ref ID
#' @examples
#' idx <- refIDtoIndex(852645)
#' # outputs:
#' #    i   j
#' # 1 13 677
#'
#' idx <- indexToRefID(13, 667)
#' # outputs:
#' # 852645
#' @rdname refIdToIndexAndBack
#' @export
refIDtoIndex <- function(refID) {
  i <- bitwShiftR(refID, 16L)
  j <- bitwAnd(refID, 0x0000FFFF)
  return(data.frame(i, j))
}

#' @rdname refIdToIndexAndBack
#' @export
indexToRefID <- function(i,
                         j) {
  return(bitwShiftL(i, 16L) + j)
}
