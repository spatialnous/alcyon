# SPDX-FileCopyrightText: Petros Koutsolampros
# SPDX-License-Identifier: GPL-3.0-only

#!/bin/bash

if [ $# -lt 2 ]; then
    echo "Requires at least 2 arguments, the username and password for docker.io"
    exit 1
fi

podman login -u $1 -p $2  docker.io/spatialnous/alcyon-debug
podman push alcyon-debug docker://docker.io/spatialnous/alcyon-debug:latest
