# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

AC_INIT([alcyon], 0.5.0) dnl package name, version
AC_CONFIG_SRCDIR([src/communicator.h])

if test -z "$CMAKE"; then CMAKE="`which cmake`"; fi
if test -z "$CMAKE"; then CMAKE=/Applications/CMake.app/Contents/bin/cmake; fi
if test ! -f "$CMAKE"; then echo "no ‘cmake’ command found"; exit 1; fi

: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
else
  echo "R_HOME set at: ${R_HOME}"
fi
CC=`"${R_HOME}/bin/R" CMD config CC`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
LDFLAGS=`"${R_HOME}/bin/R" CMD config LDFLAGS`

### Force enable openmp using --enable-force-openmp
AC_ARG_ENABLE([force-openmp],
    AS_HELP_STRING([--enable-force-openmp],
    [Try to force openmp even if R has not been built with it]))

EXTRA_LIBS=""
AS_IF([test "x$enable_force_openmp" = "xyes"], [
  EXTRA_LIBS="-fopenmp"
])

dnl substitute EXTRA_LIBS
AC_SUBST(EXTRA_LIBS)
dnl and do substitution in the src/Makevars.in
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT

stringContain() { case $2 in *$1* ) return 0;; *) return 1;; esac ;}

# This one looks into {R_HOME}/etc/Makeconf to see if R has been built with
# openmp support. If it hasn't it will set $R_HAS_OMP = FALSE, and that
# value will then be provided to CMake to disable OpenMP when building
R_HAS_OMP=`"${R_HOME}/bin/Rscript" -e 'mkcnf <- readLines(paste0(R.home(), "/etc/Makeconf"));
ompopt = sub("SHLIB_OPENMP_CXXFLAGS = (.*?)$", "\\\\1", mkcnf[[grep("SHLIB_OPENMP_CXXFLAGS", mkcnf)]]);
ompopt = trimws(ompopt); cat(ompopt == "-fopenmp")'`

DISABLE_OPENMP=OFF

if test "$R_HAS_OMP" = "FALSE" ; then
  if stringContain "-fopenmp" "$EXTRA_LIBS" -a ; then
    echo "R has NOT been built with OpenMP support but enabled manually"
    DISABLE_OPENMP=OFF
  else
    echo "R has NOT been built with OpenMP support, disabling"
    DISABLE_OPENMP=ON
  fi
else
  echo "R has been built with OpenMP support, enabling"
  DISABLE_OPENMP=OFF
fi

"${R_HOME}/bin/Rscript" -e 'library(Rcpp); compileAttributes(".")'

cd src
mkdir -p build-Release && cd build-Release
$CMAKE ../libs \
  -DCMAKE_VERBOSE_MAKEFILE=ON \
  -DCMAKE_BUILD_TYPE=Release \
  -DBUILD_SHARED_LIBS:bool=OFF \
  -DCMAKE_POSITION_INDEPENDENT_CODE:bool=ON \
  -DDISABLE_OPENMP=${DISABLE_OPENMP}

# remove .NOTPARALLEL to stop CMD from complaining
unamestr=$(uname)
if test "$unamestr" = Darwin; then
  find . -name Makefile -exec sed -i '' -e '/^.NOTPARALLEL:$/s/^/#/' {} \;
else
  find . -name Makefile -exec sed -i '/^.NOTPARALLEL:$/s/^/#/' {} \;
fi

${MAKE}

