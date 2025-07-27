# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

context("VGA tests")

defaultLatticeMapColumns <- c(
    "x",
    "y",
    "filled",
    "blocked",
    "contextfilled",
    "edge",
    "Ref",
    "Connectivity",
    "Point First Moment",
    "Point Second Moment"
)

runAnalysisR <- function(func, newExpectedCols) {
    startData <- loadSimpleLinesAsLatticeMap(vector())
    lineStringMap <- startData$sf
    latticeMap <- startData$latticeMap

    expectedCols <- defaultLatticeMapColumns

    latticeMap <- func(latticeMap, lineStringMap)

    coords <- Rcpp_LatticeMap_getFilledPoints(
        latticeMapPtr = attr(latticeMap, "sala_map")
    )
    expect_identical(dim(coords), c(90L, 10L + length(newExpectedCols)))
    expectedCols <- c(
        expectedCols,
        newExpectedCols
    )
    expect_identical(colnames(coords), expectedCols)
}

test_that("VGA in R, Through vision", {
    runAnalysisR(
        function(latticeMap, ...) {
            return(vgaThroughVision(latticeMap))
        },
        "Through vision"
    )
})

test_that("VGA in R, Angular all-to-all", {
    runAnalysisR(
        function(latticeMap, ...) {
            return(allToAllTraverse(
                latticeMap,
                traversalType = TraversalType$Angular,
                radii = -1L,
                radiusTraversalType = TraversalType$None
            ))
        },
        newExpectedCols = c(
            "Angular Mean Depth",
            "Angular Total Depth",
            "Angular Node Count"
        )
    )
})

test_that("VGA in R, Metric all-to-all", {
    runAnalysisR(
        function(latticeMap, ...) {
            return(allToAllTraverse(
                latticeMap,
                traversalType = TraversalType$Metric,
                radii = -1L,
                radiusTraversalType = TraversalType$None
            ))
        },
        newExpectedCols = c(
            "Metric Mean Shortest-Path Angle",
            "Metric Mean Shortest-Path Distance",
            "Metric Mean Straight-Line Distance",
            "Metric Node Count"
        )
    )
})

test_that("VGA in R, Topological (Visual global) all-to-all", {
    runAnalysisR(
        function(latticeMap, ...) {
            return(allToAllTraverse(
                latticeMap,
                traversalType = TraversalType$Topological,
                radii = -1L,
                radiusTraversalType = TraversalType$None
            ))
        },
        newExpectedCols = c(
            "Visual Entropy",
            "Visual Integration [HH]",
            "Visual Integration [P-value]",
            "Visual Integration [Tekl]",
            "Visual Mean Depth",
            "Visual Node Count",
            "Visual Relativised Entropy"
        )
    )
})

test_that("VGA in R, Visual local all-to-all", {
    runAnalysisR(
        function(latticeMap, ...) {
            return(vgaVisualLocal(latticeMap, FALSE))
        },
        newExpectedCols = c(
            "Visual Clustering Coefficient",
            "Visual Control",
            "Visual Controllability"
        )
    )
})

test_that("VGA in R, Isovist all-to-all", {
    runAnalysisR(
        function(latticeMap, lineStringMap) {
            boundaryMap <- as(lineStringMap, "ShapeMap")
            return(vgaIsovist(latticeMap, boundaryMap))
        },
        newExpectedCols = c(
            "Isovist Area",
            "Isovist Compactness",
            "Isovist Drift Angle",
            "Isovist Drift Magnitude",
            "Isovist Min Radial",
            "Isovist Max Radial",
            "Isovist Occlusivity",
            "Isovist Perimeter"
        )
    )
})

test_that("VGA in R, Angular one-to-all", {
    runAnalysisR(
        function(latticeMap, ...) {
            return(oneToAllTraverse(
                latticeMap,
                traversalType = TraversalType$Angular,
                fromX = 7.52,
                fromY = 6.02
            ))
        },
        newExpectedCols = "Angular Step Depth"
    )
})

test_that("VGA in R, Metric one-to-all", {
    runAnalysisR(
        function(latticeMap, ...) {
            return(oneToAllTraverse(
                latticeMap,
                traversalType = TraversalType$Metric,
                fromX = 7.52,
                fromY = 6.02
            ))
        },
        newExpectedCols = c(
            "Metric Step Shortest-Path Angle",
            "Metric Step Shortest-Path Length",
            "Metric Straight-Line Distance"
        )
    )
})

test_that("VGA in R, Visual one-to-all", {
    runAnalysisR(
        function(latticeMap, ...) {
            return(oneToAllTraverse(
                latticeMap,
                traversalType = TraversalType$Topological,
                fromX = 7.52,
                fromY = 6.02
            ))
        },
        newExpectedCols = "Visual Step Depth"
    )
})

test_that("VGA in R, Angular one-to-one", {
    runAnalysisR(
        function(latticeMap, ...) {
            return(oneToOneTraverse(
                latticeMap,
                traversalType = TraversalType$Angular,
                fromX = 7.52,
                fromY = 6.02,
                toX = 5.78,
                toY = 2.96
            ))
        },
        newExpectedCols = c(
            "Angular Shortest Path",
            "Angular Shortest Path Linked",
            "Angular Shortest Path Order",
            "Angular Shortest Path Visual Zone",
            "Angular Shortest Path Metric Zone",
            "Angular Shortest Path Inv Metric Zone"
        )
    )
})

test_that("VGA in R, Metric one-to-one", {
    runAnalysisR(
        function(latticeMap, ...) {
            return(oneToOneTraverse(
                latticeMap,
                traversalType = TraversalType$Metric,
                fromX = 7.52,
                fromY = 6.02,
                toX = 5.78,
                toY = 2.96
            ))
        },
        newExpectedCols = c(
            "Metric Shortest Path",
            "Metric Shortest Path Distance",
            "Metric Shortest Path Linked",
            "Metric Shortest Path Order",
            "Metric Shortest Path Visual Zone",
            "Metric Shortest Path Metric Zone",
            "Metric Shortest Path Inv Metric Zone"
        )
    )
})

test_that("VGA in R, Visual one-to-one", {
    runAnalysisR(
        function(latticeMap, ...) {
            return(oneToOneTraverse(
                latticeMap,
                traversalType = TraversalType$Topological,
                fromX = 7.52,
                fromY = 6.02,
                toX = 5.78,
                toY = 2.96
            ))
        },
        newExpectedCols = c(
            "Visual Shortest Path",
            "Visual Shortest Path Linked",
            "Visual Shortest Path Order",
            "Visual Shortest Path Visual Zone",
            "Visual Shortest Path Metric Zone",
            "Visual Shortest Path Inv Metric Zone"
        )
    )
})
