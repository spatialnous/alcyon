# SPDX-FileCopyrightText: Petros Koutsolampros
# SPDX-License-Identifier: GPL-3.0-only

#!/bin/bash

# install container
podman build -t alcyon-debug \
-<< EOF
FROM docker.io/wch1/r-debug:latest
RUN apt-get update && apt-get upgrade -y && apt-get install -y libudunits2-dev libgdal-dev cmake libomp-15-dev
RUN RDsan -e 'install.packages(c("sf", "stars", "RcppProgress", "testthat", "devtools"))'
RUN RDcsan -e 'install.packages(c("sf", "stars", "RcppProgress", "testthat", "devtools"))'
EOF

podman build -t alcyon-debug-rocker \
-<< EOF
FROM docker.io/rocker/r-devel-ubsan-clang:latest
RUN apt-get update && apt-get upgrade -y && apt-get install -t unstable -y libudunits2-dev libgdal-dev cmake libomp-15-dev
RUN ASAN_OPTIONS=detect_leaks=0 Rdevel -e 'install.packages(c("sf", "stars", "RcppProgress", "testthat", "devtools"))'
EOF

