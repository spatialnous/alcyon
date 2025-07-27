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

runAnalysisCPP <- function(func, newExpectedCols) {
    startData <- loadSimpleLinesAsLatticeMap(vector())
    lineStringMap <- startData$sf
    latticeMapPtr <- attr(startData$latticeMap, "sala_map")

    expectedCols <- defaultLatticeMapColumns

    vgaResult <- func(latticeMapPtr, lineStringMap)
    latticeMapPtr <- vgaResult$mapPtr

    expect_identical(vgaResult$newAttributes, newExpectedCols)

    coords <- Rcpp_LatticeMap_getFilledPoints(latticeMapPtr = latticeMapPtr)
    expect_identical(dim(coords), c(90L, 10L + length(newExpectedCols)))
    expectedCols <- c(
        expectedCols,
        newExpectedCols
    )
    expect_identical(colnames(coords), expectedCols)
}

test_that("VGA in C++, Through vision", {
    runAnalysisCPP(
        function(latticeMapPtr, ...) {
            return(Rcpp_VGA_throughVision(latticeMapPtr))
        },
        newExpectedCols = "Through vision"
    )
})

test_that("VGA in C++, Angular all-to-all", {
    runAnalysisCPP(
        function(latticeMapPtr, ...) {
            return(Rcpp_VGA_angular(latticeMapPtr, -1.0, FALSE))
        },
        newExpectedCols = c(
            "Angular Mean Depth",
            "Angular Total Depth",
            "Angular Node Count"
        )
    )
})

test_that("VGA in C++, Metric all-to-all", {
    runAnalysisCPP(
        function(latticeMapPtr, ...) {
            return(Rcpp_VGA_metric(latticeMapPtr, -1.0, FALSE))
        },
        newExpectedCols = c(
            "Metric Mean Shortest-Path Angle",
            "Metric Mean Shortest-Path Distance",
            "Metric Mean Straight-Line Distance",
            "Metric Node Count"
        )
    )
})

test_that("VGA in C++, Visual global all-to-all", {
    runAnalysisCPP(
        function(latticeMapPtr, ...) {
            return(Rcpp_VGA_visualGlobal(latticeMapPtr, -1L, FALSE))
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

test_that("VGA in C++, Visual local all-to-all", {
    runAnalysisCPP(
        function(latticeMapPtr, ...) {
            return(Rcpp_VGA_visualLocal(latticeMapPtr, FALSE))
        },
        newExpectedCols = c(
            "Visual Clustering Coefficient",
            "Visual Control",
            "Visual Controllability"
        )
    )
})

test_that("VGA in C++, Isovist all-to-all", {
    runAnalysisCPP(
        function(latticeMapPtr, lineStringMap) {
            boundaryMap <- as(lineStringMap, "ShapeMap")
            return(Rcpp_VGA_isovist(latticeMapPtr, attr(boundaryMap, "sala_map")))
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

test_that("VGA in C++, Angular one-to-all", {
    runAnalysisCPP(
        function(latticeMapPtr, ...) {
            return(Rcpp_VGA_angularDepth(latticeMapPtr, cbind(7.52, 6.02)))
        },
        newExpectedCols = "Angular Step Depth"
    )
})

test_that("VGA in C++, Metric one-to-all", {
    runAnalysisCPP(
        function(latticeMapPtr, ...) {
            return(Rcpp_VGA_metricDepth(latticeMapPtr, cbind(7.52, 6.02)))
        },
        newExpectedCols = c(
            "Metric Step Shortest-Path Angle",
            "Metric Step Shortest-Path Length",
            "Metric Straight-Line Distance"
        )
    )
})

test_that("VGA in C++, Visual one-to-all", {
    runAnalysisCPP(
        function(latticeMapPtr, ...) {
            return(Rcpp_VGA_visualDepth(latticeMapPtr, cbind(7.52, 6.02)))
        },
        newExpectedCols = "Visual Step Depth"
    )
})

test_that("VGA in C++, Angular one-to-one", {
    runAnalysisCPP(
        function(latticeMapPtr, ...) {
            return(Rcpp_VGA_angularShortestPath(
                latticeMapPtr,
                cbind(7.52, 6.02),
                cbind(5.78, 2.96)
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

test_that("VGA in C++, Metric one-to-one", {
    runAnalysisCPP(
        function(latticeMapPtr, ...) {
            return(Rcpp_VGA_metricShortestPath(
                latticeMapPtr,
                cbind(7.52, 6.02),
                cbind(5.78, 2.96)
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

test_that("VGA in C++, Visual one-to-one", {
    runAnalysisCPP(
        function(latticeMapPtr, ...) {
            return(Rcpp_VGA_visualShortestPath(
                latticeMapPtr,
                cbind(7.52, 6.02),
                cbind(5.78, 2.96)
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
