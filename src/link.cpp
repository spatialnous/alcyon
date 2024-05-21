// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "salalib/shapegraph.h"
#include "salalib/pointdata.h"

#include "genlib/p2dpoly.h"

#include "communicator.h"

#include <Rcpp.h>

// [[Rcpp::export("Rcpp_ShapeGraph_linkCoords")]]
void shapeGraphLinkCoords(Rcpp::XPtr<ShapeGraph> shapeGraphPtr,
                          Rcpp::NumericMatrix coords) {
  if (coords.cols() != 4) {
    Rcpp::stop("The coords matrix needs to have 4 columns: x1, y1, x2, y2");
  }
  for (int i = 0; i < coords.rows(); ++i) {
    const Rcpp::NumericMatrix::Row &row = coords( i , Rcpp::_ );
    QtRegion region(Point2f(row[0], row[1]),
                    Point2f(row[0], row[1]));
    shapeGraphPtr->setCurSel(region);
    shapeGraphPtr->linkShapes(Point2f(row[2], row[3]));
  }
}

// [[Rcpp::export("Rcpp_ShapeGraph_linkRefs")]]
void shapeGraphLinkRefs(Rcpp::XPtr<ShapeGraph> shapeGraphPtr,
                        Rcpp::IntegerMatrix refs) {
  if (refs.cols() != 2) {
    Rcpp::stop("The refs matrix needs to have 2 columns: fromRef, toRef");
  }
  for (int i = 0; i < refs.rows(); ++i) {
    const Rcpp::IntegerMatrix::Row &row = refs( i , Rcpp::_ );
    shapeGraphPtr->linkShapesFromRefs(row[0], row[1]);
  }
}

// [[Rcpp::export("Rcpp_PointMap_linkCoords")]]
void pointMapLinkCoords(Rcpp::XPtr<PointMap> pointMapPtr,
                        Rcpp::NumericMatrix coords) {
  if (coords.cols() != 4) {
    Rcpp::stop("The coords matrix needs to have 4 columns: x1, y1, x2, y2");
  }
  for (int i = 0; i < coords.rows(); ++i) {
    const Rcpp::NumericMatrix::Row &row = coords( i , Rcpp::_ );
    const PixelRef &a = pointMapPtr->pixelate(Point2f(row[0], row[1]), false);
    const PixelRef &b = pointMapPtr->pixelate(Point2f(row[2], row[3]), false);

    if (!pointMapPtr->includes(a) || !pointMapPtr->getPoint(a).filled()) {
      Rcpp::stop("Point on line %d (%f, %f) not on filled analysis space",
                 i, row[0], row[1]);
    }

    if (!pointMapPtr->includes(b) || !pointMapPtr->getPoint(b).filled()) {
      Rcpp::stop("Point on line %d (%f, %f) not on filled analysis space",
                 i, row[2], row[3]);
    }

    if (pointMapPtr->isPixelMerged(a)) {
      Rcpp::stop("Point on line %d (%f, %f) is already part of a link",
                 i, row[0], row[1]);
    }

    if (pointMapPtr->isPixelMerged(b)) {
      Rcpp::stop("Point on line %d (%f, %f) is already part of a link",
                 i, row[2], row[3]);
    }

    pointMapPtr->mergePixels(a, b);
  }
}

// [[Rcpp::export("Rcpp_PointMap_linkRefs")]]
void pointMapLinkRefs(Rcpp::XPtr<PointMap> pointMapPtr,
                      Rcpp::IntegerMatrix refs) {
  if (refs.cols() != 2) {
    Rcpp::stop("The refs matrix needs to have 2 columns: fromRef, toRef");
  }
  for (int i = 0; i < refs.rows(); ++i) {
    const Rcpp::IntegerMatrix::Row &row = refs( i , Rcpp::_ );

    const PixelRef a(row[0]);
    const PixelRef b(row[1]);

    if (!pointMapPtr->includes(a) || !pointMapPtr->getPoint(a).filled()) {
      Rcpp::stop("Point on line %d (%d) not on filled analysis space",
                 i, row[0]);
    }

    if (!pointMapPtr->includes(b) || !pointMapPtr->getPoint(b).filled()) {
      Rcpp::stop("Point on line %d (%d) not on filled analysis space",
                 i, row[1]);
    }

    if (pointMapPtr->isPixelMerged(a)) {
      Rcpp::stop("Point on line %d (%d) is already part of a link",
                 i, row[0]);
    }

    if (pointMapPtr->isPixelMerged(b)) {
      Rcpp::stop("Point on line %d (%d) is already part of a link",
                 i, row[1]);
    }

    pointMapPtr->mergePixels(row[0], row[1]);
  }
}

// [[Rcpp::export("Rcpp_ShapeGraph_unlinkCoords")]]
void shapeMapUnlinkCoords(Rcpp::XPtr<ShapeGraph> shapeGraphPtr,
                          Rcpp::NumericMatrix coords) {
  if (coords.cols() != 4) {
    Rcpp::stop("The coords matrix needs to have 4 columns: x1, y1, x2, y2");
  }
  for (int i = 0; i < coords.rows(); ++i) {
    const Rcpp::NumericMatrix::Row &row = coords( i , Rcpp::_ );
    auto ref1 = shapeGraphPtr->getClosestLine(Point2f(row[0], row[1]));
    auto ref2 = shapeGraphPtr->getClosestLine(Point2f(row[2], row[3]));
    shapeGraphPtr->unlinkShapesFromRefs(ref1, ref2);
  }
}

// [[Rcpp::export("Rcpp_ShapeGraph_unlinkAtCrossPoint")]]
void shapeGraphUnlinkAtCrossPoint(Rcpp::XPtr<ShapeGraph> shapeGraphPtr,
                                  Rcpp::NumericMatrix coords) {
  if (coords.cols() != 2) {
    Rcpp::stop("The coords matrix needs to have 2 columns: x, y");
  }
  for (int i = 0; i < coords.rows(); ++i) {
    const Rcpp::NumericMatrix::Row &row = coords( i , Rcpp::_ );
    shapeGraphPtr->unlinkAtPoint(Point2f(row[0], row[1]));
  }
}

// [[Rcpp::export("Rcpp_ShapeGraph_unlinkRefs")]]
void shapeMapUnlinkRefs(Rcpp::XPtr<ShapeGraph> shapeGraphPtr,
                        Rcpp::IntegerMatrix refs) {
  if (refs.cols() != 2) {
    Rcpp::stop("The refs matrix needs to have 2 columns: fromRef, toRef");
  }
  for (int i = 0; i < refs.rows(); ++i) {
    const Rcpp::IntegerMatrix::Row &row = refs( i , Rcpp::_ );
    shapeGraphPtr->unlinkShapesFromRefs(row[0], row[1]);
  }
}

// [[Rcpp::export("Rcpp_PointMap_unlinkCoords")]]
void pointMapUnlinkCoords(Rcpp::XPtr<PointMap> pointMapPtr,
                          Rcpp::NumericMatrix coords) {
  if (coords.cols() != 4) {
    Rcpp::stop("The coords matrix needs to have 4 columns: x1, y1, x2, y2");
  }
  for (int i = 0; i < coords.rows(); ++i) {
    const Rcpp::NumericMatrix::Row &row = coords( i , Rcpp::_ );
    const PixelRef &a = pointMapPtr->pixelate(Point2f(row[0], row[1]), false);
    const PixelRef &b = pointMapPtr->pixelate(Point2f(row[2], row[3]), false);

    if (!pointMapPtr->includes(a) || !pointMapPtr->getPoint(a).filled()) {
      Rcpp::stop("Point on line %d (%f, %f) not on filled analysis space",
                 i, row[0], row[1]);
    }

    if (!pointMapPtr->includes(b) || !pointMapPtr->getPoint(b).filled()) {
      Rcpp::stop("Point on line %d (%f, %f) not on filled analysis space",
                 i, row[2], row[3]);
    }

    if (!pointMapPtr->isPixelMerged(a)) {
      Rcpp::stop("Point on line %d (%f, %f) is not part of a link",
                 i, row[0], row[1]);
    }

    if (!pointMapPtr->isPixelMerged(b)) {
      Rcpp::stop("Point on line %d (%f, %f) is not part of a link",
                 i, row[2], row[3]);
    }

    pointMapPtr->unmergePixel(a);
  }
}

// [[Rcpp::export("Rcpp_PointMap_unlinkRefs")]]
void pointMapUnlinkRefs(Rcpp::XPtr<PointMap> pointMapPtr,
                        Rcpp::IntegerMatrix refs) {
  if (refs.cols() != 2) {
    Rcpp::stop("The refs matrix needs to have 2 columns: fromRef, toRef");
  }
  for (int i = 0; i < refs.rows(); ++i) {
    const Rcpp::IntegerMatrix::Row &row = refs( i , Rcpp::_ );

    const PixelRef a(row[0]);
    const PixelRef b(row[1]);

    if (!pointMapPtr->includes(a) || !pointMapPtr->getPoint(a).filled()) {
      Rcpp::stop("Point on line %d (%d) not on filled analysis space",
                 i, row[0]);
    }

    if (!pointMapPtr->includes(b) || !pointMapPtr->getPoint(b).filled()) {
      Rcpp::stop("Point on line %d (%d) not on filled analysis space",
                 i, row[1]);
    }

    if (!pointMapPtr->isPixelMerged(a)) {
      Rcpp::stop("Point on line %d (%f, %f) is not part of a link",
                 i, row[0], row[1]);
    }

    if (!pointMapPtr->isPixelMerged(b)) {
      Rcpp::stop("Point on line %d (%f, %f) is not part of a link",
                 i, row[2], row[3]);
    }

    pointMapPtr->unmergePixel(a);
  }
}
