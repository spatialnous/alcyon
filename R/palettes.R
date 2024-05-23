# SPDX-FileCopyrightText: 2019 Fani Kostourou
# SPDX-FileCopyrightText: 2019 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

#' Colour Palettes from depthmapX
#'
#' Create n contiguous colours taken from depthmapX.
#'
#' @name palettes
#' @param n Number of colours to generate
#' @param rangeMin The min value of the range
#' @param rangeMax The max value of the range
#' @returns Returns a vector of colours.
#' @examples
#' depthmap.classic.colour(100, 0, 1)
#' depthmap.axmanesque.colour(100, 0, 1)
#' depthmap.purpleorange.colour(100, 0, 1)
#' depthmap.bluered.colour(100, 0, 1)
#' depthmap.grayscale.colour(100, 0, 1)
#' depthmap.nicehsb.colour(100, 0, 1)
#' @rdname palettes
#' @export
depthmap.classic.colour <- function(n,
                                    rangeMin = 0.0,
                                    rangeMax = 1.0) {
  unlist(lapply(0L:(n - 1L), function(i) {
    makeDepthmapClassicColour(i / n, rangeMin, rangeMax)
  }))
}

#' @rdname palettes
#' @export
depthmap.axmanesque.colour <- function(n,
                                       rangeMin = 0.0,
                                       rangeMax = 1.0) {
  unlist(lapply(0L:(n - 1L), function(i) {
    makeAxmanesqueColour(i / n, rangeMin, rangeMax)
  }))
}

#' @rdname palettes
#' @export
depthmap.purpleorange.colour <- function(n,
                                         rangeMin = 0.0,
                                         rangeMax = 1.0) {
  unlist(lapply(0L:(n - 1L), function(i) {
    makePurpleOrangeColour(i / n, rangeMin, rangeMax)
  }))
}

#' @rdname palettes
#' @export
depthmap.bluered.colour <- function(n,
                                    rangeMin = 0.0,
                                    rangeMax = 1.0) {
  unlist(lapply(0L:(n - 1L), function(i) {
    makeBlueRedColour(i / n, rangeMin, rangeMax)
  }))
}

#' @rdname palettes
#' @export
depthmap.grayscale.colour <- function(n,
                                      rangeMin = 0.0,
                                      rangeMax = 1.0) {
  unlist(lapply(0L:(n - 1L), function(i) {
    makeGreyScaleColour(i / n, rangeMin, rangeMax)
  }))
}

#' @rdname palettes
#' @export
depthmap.nicehsb.colour <- function(n,
                                    rangeMin = 0.0,
                                    rangeMax = 1.0) {
  unlist(lapply(0L:(n - 1L), function(i) {
    makeNiceHSBColour(i / n, rangeMin, rangeMax)
  }))
}


#' Single Colour from depthmapX's Palettes
#'
#' Create a single colour from depthmapX's palettes.
#'
#' @name makeColour
#' @param value Value within the min/max range to take
#' @param rangeMin The min value of the range
#' @param rangeMax The max value of the range
#' @returns Returns a single colour.
#' @examples
#' makeDepthmapClassicColour(0.2, 0, 1)
#' makeAxmanesqueColour(0.2, 0, 1)
#' makePurpleOrangeColour(0.2, 0, 1)
#' makeBlueRedColour(0.2, 0, 1)
#' makeGreyScaleColour(0.2, 0, 1)
#' makeNiceHSBColour(0.2, 0, 1)
#' @export
#' @rdname makeColour
makeDepthmapClassicColour <- function(value,
                                      rangeMin = 0.0,
                                      rangeMax = 1.0) {
  if (rangeMin > rangeMax) {
    value <- 1.0 - value
    rangeMin <- 1.0 - rangeMin
    rangeMax <- 1.0 - rangeMax
  }
  if (value < 0.0) {
    return(c(0.0, 0.0, 0.0))
  }

  green <- rangeMin + (rangeMax - rangeMin) / 10.0
  r <- g <- b <- 0.0
  offset <- 0.0333

  htmlByte <- function(colorByte) {
    return(floor((colorByte + offset) * 15.0) * 17.0)
  }

  ## | --- 0
  ## |
  ## | --- rangeMin
  ## |
  ## | --- (green + rangeMin) / 2 <- avg between green and rangeMin
  ## |
  ## | --- green
  ## |
  ## | --- (green + rangeMax) / 2 <- avg between green and rangeMax
  ## |
  ## | --- rangeMax
  ## |
  ## | --- higher than rangeMax

  # NB previously included colour muting: the 1.0 was originally 0.9 to
  # mute the colours slightly
  if (value < rangeMin) {
    r <- htmlByte(0.5 * (rangeMin - value) / rangeMin)
    b <- 0xFF
  } else if (value < (green + rangeMin) / 2.0) {
    b <- 0xFF
    g <- htmlByte(2.0 * (value - rangeMin) / (green - rangeMin))
  } else if (value < green) {
    b <- htmlByte(2.0 * (green - value) / (green - rangeMin))
    g <- 0xFF
  } else if (value < (green + rangeMax) / 2.0) {
    g <- 0xFF
    r <- htmlByte(2.0 * (value - green) / (rangeMax - green))
  } else if (value < rangeMax) {
    g <- htmlByte(2.0 * (rangeMax - value) / (rangeMax - green))
    r <- 0xFF
  } else {
    r <- 0xFF
    b <- htmlByte(0.5 * (value - rangeMax) / (1.0 - rangeMax))
  }
  return(grDevices::rgb(r / 255.0, g / 255.0, b / 255.0))
}

#' @rdname makeColour
#' @export
makeAxmanesqueColour <- function(value,
                                 rangeMin = 0.0,
                                 rangeMax = 1.0) {
  gNicecolor <- c(
    "#3333DD", # 0 blue
    "#3388DD", # 1
    "#22CCDD", # 2
    "#22CCBB", # 3
    "#22DD88", # 4
    "#88DD22", # 5
    "#BBCC22", # 6
    "#DDCC22", # 7
    "#DD8833", # 8
    "#DD3333" # 9 red
  )
  f <- (value - rangeMin) / (rangeMax - rangeMin)
  if (rangeMin > rangeMax) {
    f <- 1.0 - f
  }
  return(gNicecolor[1L + as.integer((f - 1e-9) * length(gNicecolor))])
}

#' @rdname makeColour
#' @export
makePurpleOrangeColour <- function(value,
                                   rangeMin = 0.0,
                                   rangeMax = 1.0) {
  gPurpleOrange <- c(
    "#542788", # 0 purple
    "#998EC3", # 1
    "#D8DAEB", # 2
    "#F7F7F7", # 3
    "#FEE0B6", # 4
    "#F1A340", # 5
    "#B35806" # 6 orange
  )
  f <- (value - rangeMin) / (rangeMax - rangeMin)
  if (rangeMin > rangeMax) {
    f <- 1.0 - f
  }
  return(gPurpleOrange[1L + as.integer((f - 1e-9) * length(gPurpleOrange))])
}

#' @rdname makeColour
#' @export
makeBlueRedColour <- function(value,
                              rangeMin = 0.0,
                              rangeMax = 1.0) {
  gBlueRed <- c(
    "#4575B4", # 0 blue
    "#91BFDB", # 1
    "#E0F3F8", # 2
    "#FFFFBF", # 3
    "#FEE090", # 4
    "#FC8D59", # 5
    "#D73027" # 6 red
  )
  f <- (value - rangeMin) / (rangeMax - rangeMin)
  if (rangeMin > rangeMax) {
    f <- 1.0 - f
  }
  return(gBlueRed[1L + as.integer((f - 1e-9) * length(gBlueRed))])
}

#' @rdname makeColour
#' @export
makeGreyScaleColour <- function(value,
                                rangeMin = 0.0,
                                rangeMax = 1.0) {
  gGreyscale <- c(
    "#000000", # 0 black
    "#444444", # 1
    "#777777", # 2
    "#AAAAAA", # 3
    "#CCCCCC", # 4
    "#EEEEEE", # 5
    "#FFFFFF" # 6 white
  )
  f <- (value - rangeMin) / (rangeMax - rangeMin)
  if (rangeMin > rangeMax) {
    f <- 1.0 - f
  }
  return(gGreyscale[1L + as.integer((f - 1e-9) * length(gGreyscale))])
}

#' @rdname makeColour
#' @export
makeNiceHSBColour <- function(value,
                              rangeMin = 0.0,
                              rangeMax = 1.0) {
  gNicecolorhsb <- c(
    "#3333DD", # 0 blue
    "#3377DD", # 1
    "#33BBDD", # 2
    "#33DDBB", # 3
    "#33DD55", # 4
    "#55DD33", # 5
    "#BBDD33", # 6
    "#DDBB33", # 7
    "#DD7733", # 8
    "#DD3333" # 9 red
  )
  f <- (value - rangeMin) / (rangeMax - rangeMin)
  if (rangeMin > rangeMax) {
    f <- 1.0 - f
  }
  return(gNicecolorhsb[1L + as.integer((f - 1e-9) * length(gNicecolorhsb))])
}
