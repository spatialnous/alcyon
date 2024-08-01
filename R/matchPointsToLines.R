# SPDX-FileCopyrightText: 2019 Kimon Krenz
# SPDX-FileCopyrightText: 2019 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

getMatches <- function(d) {
    # the process needs to be done for the fewer objects
    # i.e. if we have 1000 lines, but 10 points then the
    # process only needs to run 10 times
    mindim <- min(ncol(d), nrow(d))
    # create a row of matches to store the row/column names
    matches <- matrix(ncol = 2L, nrow = mindim)
    # first run is special as it's on the raw distance matrix
    i <- 1L
    matches[i, ] <- arrayInd(which.min(d), dim(d))
    # for all other runs:
    for (i in 2L:mindim) {
        # filter the matrix based on the previous matches
        m <- d[
            !(rownames(d) %in% matches[1L:(i - 1L), 1L]),
            !(colnames(d) %in% matches[1L:(i - 1L), 2L]),
            drop = FALSE
        ]
        # find the index of the smallest value (closest point to line)
        idcs <- arrayInd(which.min(m), dim(m))
        # add to matches, making sure to take row/column names
        matches[i, ] <- as.integer(c(
            rownames(m)[idcs[, 1L]],
            colnames(m)[idcs[, 2L]]
        ))
    }
    return(matches)
}


#' Match points to lines
#'
#' Match points to their closest line. Matches (spatial-join) points to lines.
#' Finds the point closest to a line. One point is attached to one line, thus if
#' fewer points than lines are given then some lines will have no point
#' attached.
#'
#' @param points Points to attach.
#' @param lines Lines to attach to.
#' @param getIndex Get the index returned and not the data.
#' @returns If getIndex is TRUE then the index of the points as they relate to
#' the matching lines are given. If not, then the data from the points dataframe
#' is returned.
#' @examples
#' segmentsMif <- system.file(
#'     "extdata", "testdata", "barnsbury",
#'     "barnsbury_small_segment_original.mif",
#'     package = "alcyon"
#' )
#' segmentsSf <- st_read(
#'     segmentsMif,
#'     geometry_column = 1L, quiet = TRUE
#' )
#' gateCountsMif <- system.file(
#'     "extdata", "testdata", "barnsbury",
#'     "barnsbury_ped_gatecounts.mif",
#'     package = "alcyon"
#' )
#' gateCountsSf <- st_read(
#'     gateCountsMif,
#'     geometry_column = 1L, quiet = TRUE
#' )
#' matchPointsToLines(gateCountsSf, segmentsSf)
#' @export
matchPointsToLines <- function(points,
                               lines,
                               getIndex = FALSE) {
    d <- sf::st_distance(lines, points)

    # this is done to retain the name of the
    # row/column through filtering
    rownames(d) <- seq_len(nrow(d))
    colnames(d) <- seq_len(ncol(d))

    matches <- getMatches(d)

    rowidx <- rep(NA, nrow(lines))
    rowidx[matches[, 1L]] <- matches[, 2L]
    if (getIndex) {
        return(rowidx)
    } else {
        return(points[rowidx, ])
    }
}
