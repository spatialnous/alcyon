# SPDX-FileCopyrightText: 2024-2025 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-only

cmake_minimum_required(VERSION 3.13.0..4.0)
project(alcyon_sala)

set(CMAKE_COMPILE_WARNING_AS_ERROR OFF)

set (FORCE_GLOBAL_COMPILE_WARNINGS ON)

if (CMAKE_CXX_COMPILER_ID MATCHES "^(GNU|Clang|AppleClang)$")
    set(COMPILE_WARNINGS -Wall -Wextra)
elseif (CMAKE_CXX_COMPILER_ID MATCHES "^MSVC$")
    set(COMPILE_WARNINGS /W4 /EHsc)
endif()

# policy for target sources - we don't expect any old CMakes
cmake_policy(SET CMP0076 NEW)

include_directories(".")

add_subdirectory(salalib)

target_compile_features(salalib PUBLIC cxx_std_17)

# R's Makevars links $(BLDDIR)/libsalalib.a
set_target_properties(salalib PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})



