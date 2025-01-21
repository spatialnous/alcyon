# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

# CRAN OMP THREAD LIMIT
withr::with_envvar(
    new = c(OMP_THREAD_LIMIT = 2L),
    testthat::test_check("alcyon")
)
