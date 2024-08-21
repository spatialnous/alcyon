# SPDX-FileCopyrightText: 2019 Fani Kostourou
# SPDX-FileCopyrightText: 2019-2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

#' Conversion of shapegraph to graph data
#'
#' Creates data to be construct a graph, based on the connections and the x,y
#' coordinates of the centroids of shapes in a shapegraph (axial, segment,
#' convex). Specify weightColumn to assign weight to graph edges.
#'
#' If weightColumn is provided, edge connections weight is calculated by taking
#' the average of the variable of the connected nodes.
#'
#' @importFrom sf st_coordinates st_geometry st_drop_geometry st_centroid
#' @param shapeGraph A ShapeGraph
#' @param weightColumn Optional. The variable used to assign weight to graph edges
#' @returns Returns a list with edges and vertices for constructing a graph.
#' @eval c("@examples",
#' rxLoadSmallAxialLines(),
#' "shapegraphToGraphData(shapeGraph)")
#' @export
shapegraphToGraphData <- function(shapeGraph,
                                  weightColumn = NA) {
    ogr <- shapeGraph
    linksunlinks <- links(shapeGraph)
    connections <- connections(shapeGraph)
    if (nrow(linksunlinks) == 0L && length(connections$from) == 0L) {
        stop("The shapeGraph provided has no connections and no links")
    }

    edges <- matrix(ncol = 3L, nrow = 0L)
    colnames(edges) <- c("from", "to", "type")

    if (length(connections$from) != 0L) {
        connMat <- do.call(cbind, connections)
        connMat <- t(apply(connMat, 1L, function(row) {
            return(c(min(row["from"], row["to"]),
                     max(row["from"], row["to"])))
        }))
        colnames(connMat) <- c("from", "to")
        edges <- rbind(edges, cbind(unique(connMat), "connection"))
    }
    if (nrow(linksunlinks) != 0L) {
        mapLinks <- linksunlinks[linksunlinks[, "isunlink"] == 0L, c("from", "to")]
        if (nrow(mapLinks) != 0L) {
            mapLinks <- t(apply(mapLinks, 1L, function(row) {
                return(c(min(row["from"], row["to"]),
                         max(row["from"], row["to"])))
            }))
            colnames(mapLinks) <- c("from", "to")
            edges <- rbind(edges, cbind(mapLinks, "link"))
        }

        mapUnlinks <- linksunlinks[linksunlinks[, "isunlink"] == 1L, c("from", "to")]
        if (nrow(mapUnlinks) != 0L) {
            mapUnlinks <- t(apply(mapUnlinks, 1L, function(row) {
                return(c(min(row["from"], row["to"]),
                         max(row["from"], row["to"])))
            }))
            colnames(mapUnlinks) <- c("from", "to")
            edges[match(paste0(mapUnlinks[, "from"], "_", mapUnlinks[, "to"]),
                        paste0(edges[, "from"], "_", edges[, "to"])), "type"] <- "unlink"
        }
    }

    edges[, c("from", "to")] <- t(apply(edges, 1L, function(row) {
        return(c(min(row["from"], row["to"]),
                 max(row["from"], row["to"])))
    }))
    colnames(edges) <- c("from", "to", "type")
    edges <- edges[!duplicated(edges), ]

    ogr$x <- st_coordinates(st_centroid(st_geometry(ogr)))[, "X"]
    ogr$y <- st_coordinates(st_centroid(st_geometry(ogr)))[, "Y"]

    refA <- edges[, "from"]
    refB <- edges[, "to"]
    depthRef <- ogr$Depthmap_Ref
    ogr <- ogr[, c("Depthmap_Ref", names(ogr)[names(ogr) != "Depthmap_Ref"])]
    if (!is.na(weightColumn)) {
        edges$weight <- ((ogr[[match(refA, depthRef), weightColumn]]) +
                             (ogr[[match(refB, depthRef), weightColumn]])) / 2.0
        graph <- list(
            edges,
            directed = FALSE,
            vertices = st_drop_geometry(ogr)
        )
    } else {
        graph <- list(
            edges = edges,
            directed = FALSE,
            vertices = st_drop_geometry(ogr)
        )
    }
    return(graph)
}
