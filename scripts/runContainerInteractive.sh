# SPDX-FileCopyrightText: Petros Koutsolampros
# SPDX-License-Identifier: GPL-3.0-only

#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd $SCRIPT_DIR

podman run -it --volume=../:/alcyon/ \
    --workdir=/alcyon \
     alcyon-debug:latest \
     /bin/bash

# for the rocker container
# podman run -it --volume=../:/alcyon/ \
#     --workdir=/alcyon \
#      alcyon-debug-rocker:latest \
#      /bin/bash


# To test inside the container:

# RDsan -e 'install.packages("/alcyon", repos=NULL)'
# RDsan -e 'testthat::test_file("/alcyon/tests/testthat/test-isovist.R", package="alcyon")'
# RDsan -e 'testthat::test_file("/alcyon/tests/testthat/test-agentanalysis.R", package="alcyon")'
# RDsan -e 'testthat::test_file("/alcyon/tests/testthat/test-MetaGraph.R", package="alcyon")'
# RDsan -e 'testthat::test_file("/alcyon/tests/testthat/test-prepareVGA.R", package="alcyon")'
# RDsan -e 'devtools::test("/alcyon")'
# RDsan -e 'devtools::run_examples()'

# To test with clang

# RDcsan -e 'install.packages("/alcyon", repos=NULL)'
# RDcsan -e 'testthat::test_file("/alcyon/tests/testthat/test-isovist.R", package="alcyon")'
# RDcsan -e 'testthat::test_file("/alcyon/tests/testthat/test-agentanalysis.R", package="alcyon")'
# RDcsan -e 'testthat::test_file("/alcyon/tests/testthat/test-MetaGraph.R", package="alcyon")'
# RDcsan -e 'testthat::test_file("/alcyon/tests/testthat/test-prepareVGA.R", package="alcyon")'
# RDcsan -e 'devtools::test("/alcyon")'
# RDcsan -e 'devtools::run_examples()'

# To test inside the container (for the rocker container):

# Rdevel -e 'install.packages("/alcyon", repos=NULL)'
# Rdevel -e 'testthat::test_file("/alcyon/tests/testthat/test-isovist.R", package="alcyon")'
# Rdevel -e 'testthat::test_file("/alcyon/tests/testthat/test-agentanalysis.R", package="alcyon")'
# Rdevel -e 'testthat::test_file("/alcyon/tests/testthat/test-MetaGraph.R", package="alcyon")'
# Rdevel -e 'testthat::test_file("/alcyon/tests/testthat/test-prepareVGA.R", package="alcyon")'
# Rdevel -e 'devtools::test("/alcyon")'
# Rdevel -e 'devtools::run_examples()'


# Or, inside RDsan inside the container:

# install.packages("/alcyon", repos=NULL)
# testthat::test_file("/alcyon/tests/testthat/test-isovist.R", package="alcyon")
# testthat::test_file("/alcyon/tests/testthat/test-agentanalysis.R", package="alcyon")
# testthat::test_file("/alcyon/tests/testthat/test-MetaGraph.R", package="alcyon")
# testthat::test_dir("/alcyon/tests")
