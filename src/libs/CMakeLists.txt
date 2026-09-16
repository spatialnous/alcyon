# SPDX-FileCopyrightText: 2024-2025 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

cmake_minimum_required(VERSION 3.13.0..4.0)
project(alcyon_gensala)

set(CMAKE_COMPILE_WARNING_AS_ERROR OFF)

set (FORCE_GLOBAL_COMPILE_WARNINGS OFF)

if (CMAKE_CXX_COMPILER_ID MATCHES "^(GNU|Clang|AppleClang)$")
    set(COMPILE_WARNINGS -Wall -Wextra)
elseif (CMAKE_CXX_COMPILER_ID MATCHES "^MSVC$")
    set(COMPILE_WARNINGS /W4 /EHsc)
endif()

# policy for target sources - we don't expect any old CMakes
cmake_policy(SET CMP0076 NEW)

include_directories(".")

add_subdirectory(salalib EXCLUDE_FROM_ALL)

# get all the sources from salalib
get_target_property(salalib_SRCS salalib SOURCES)

# fix paths if they are not absolute
set(real_salalib_SRCS "")
foreach (source ${salalib_SRCS})
  get_filename_component(newSource ${source}
                         REALPATH BASE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/salalib")
  list(APPEND real_salalib_SRCS ${newSource})
endforeach ()

add_library(alcyon_gensala STATIC ${real_salalib_SRCS})

target_compile_options(alcyon_gensala PRIVATE ${COMPILE_WARNINGS})

# Set C++17 as the minimum
target_compile_features(alcyon_gensala PUBLIC cxx_std_17)

if (NOT DISABLE_OPENMP)
    find_package(OpenMP)
    if(OpenMP_CXX_FOUND)
        target_link_libraries(alcyon_gensala PUBLIC OpenMP::OpenMP_CXX)
    endif()
endif()



