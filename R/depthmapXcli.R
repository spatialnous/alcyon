# SPDX-FileCopyrightText: 2019 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

getDefaultCLILocation <- function() {
  depthmapXcli <- NA
  switch(Sys.info()[["sysname"]],
    Windows = {
      depthmapXcli <- "depthmapXcli_win64.exe"
    },
    Linux = {
      depthmapXcli <- "depthmapXcli_linux64"
    },
    Darwin = {
      depthmapXcli <- "depthmapXcli_macos"
    }
  )
  if (is.na(depthmapXcli)) {
    stop("Unknown operating system")
  }
  system.file("exec", depthmapXcli, package = "rdepthmap")
}

depthmapXcli <- function(params,
                         cliPath = getDefaultCLILocation(),
                         verbose = FALSE) {
  suppressWarnings({
    cmdData <- system2(cliPath, params, stdout = TRUE)
  })
  hasStatus <- "status" %in% names(attributes(cmdData))
  if (hasStatus && attr(cmdData, "status") != 0L) {
    # errored
    for (d in cmdData) {
      if (startsWith(d, "Usage")) {
        break
      }
      errTxt <- d
    }
    stop(errTxt)
  }
  if (verbose) cat(cmdData, sep = "\n")
}

formatForCLI <- function(filePath) {
  if (startsWith(filePath, "\"")) {
    return(filePath)
  }
  return(paste0("\"", filePath, "\""))
}
