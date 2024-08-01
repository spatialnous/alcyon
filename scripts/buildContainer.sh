# SPDX-FileCopyrightText: Petros Koutsolampros
# SPDX-License-Identifier: GPL-3.0-only

#!/bin/bash

# install container
podman build -t alcyon-debug-builder \
-<< EOF
FROM docker.io/wch1/r-debug:latest
RUN apt-get update && apt-get upgrade -y && apt-get install -y libudunits2-dev libgdal-dev cmake
RUN RDsan -e 'install.packages(c("sp", "sf", "stars", "RcppProgress", "testthat", "devtools"))'
RUN RDsan -e 'install.packages(c("igraph"))'
RUN RDcsan -e 'install.packages(c("sp", "sf", "stars", "RcppProgress", "testthat", "devtools"))'
RUN RDcsan -e 'install.packages(c("igraph"))'
EOF

