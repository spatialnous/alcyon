# SPDX-FileCopyrightText: 2026 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

#' Coercion between sf and alcyon map classes
#'
#' Direct conversions between \code{sf} objects and the map classes provided by
#' this package. For ShapeMap -> Axial -> Segment see
#' \link{axialToSegmentShapeGraph}.
#'
#' @name alcyon-coercion
#'
#' @param from the object to coerce.
#' @param to the target class. Supplied by \code{\link[methods]{as}} and not
#'   used by the method bodies.
#' @param strict logical, part of the \code{coerce} generic signature. Not used
#'   by the method bodies.
#'
#' @aliases coerce,sf,ShapeMap-method
#' @aliases coerce,ShapeMap,sf-method
#' @aliases coerce,ShapeMap,AxialShapeGraph-method
#' @aliases coerce,sf,AxialShapeGraph-method
#' @aliases coerce,ShapeMap,SegmentShapeGraph-method
#' @aliases coerce,sf,SegmentShapeGraph-method
NULL
