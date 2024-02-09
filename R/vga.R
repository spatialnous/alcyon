# SPDX-FileCopyrightText: 2019 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

vga <- function(graphFileIn,
                graphFileOut,
                vgaMode,
                radii,
                cliPath = getDefaultCLILocation(),
                verbose = FALSE) {
  if (is.na(graphFileOut)) graphFileOut <- graphFileIn
  if (!(vgaMode %in% c(
    "isovist", "visibility-global", "visibility-local",
    "metric", "angular", "thruvision"
  ))) {
    stop("Unknown VGA mode: ", vgaMode)
  }
  params <- c(
    "-f", formatForCLI(graphFileIn),
    "-o", formatForCLI(graphFileOut),
    "-m", "VGA",
    "-vr", paste0(radii, collapse = ",")
  )
  if (vgaMode %in% c("isovist", "metric", "angular", "thruvision")) {
    params <- c(params, "-vm", vgaMode)
  } else if (vgaMode == "visibility-global") {
    params <- c(params, "-vm", "visibility")
    params <- c(params, "-vg")
  } else if (vgaMode == "visibility-local") {
    params <- c(params, "-vm", "visibility")
    params <- c(params, "-local")
  }
  depthmapXcli(params, cliPath, verbose)
}
