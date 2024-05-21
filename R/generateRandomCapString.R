# SPDX-FileCopyrightText: 2019 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0

generateRandomCapString <- function(n = 10L) {
  paste(sample(LETTERS, n, TRUE), collapse = "")
}
