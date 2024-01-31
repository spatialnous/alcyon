set(CMAKE_OSX_DEPLOYMENT_TARGET "10.9" CACHE STRING "Minimum macOS deployment version")
project(alcyon_gensala)
cmake_minimum_required(VERSION 3.13.0)
set(CMAKE_CXX_STANDARD 17)

set(CMAKE_COMPILE_WARNING_AS_ERROR ON)

if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU" OR
    "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
    set(warnings "-Wall -Wextra -Wpedantic -Weverything")
elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "MSVC")
    set(warnings "/W4 /EHsc")
    execute_process(COMMAND make_version_header.bat WORKING_DIRECTORY depthmapX)
endif()

# policy for target sources - we don't expect any old CMakes
cmake_policy(SET CMP0076 NEW)

include_directories(".")


add_subdirectory(genlib)
add_subdirectory(salalib)

# get all the sources from genlib
get_target_property(genlib_SRCS genlib SOURCES)

# fix paths if they are not absolute
set(real_genlib_SRCS "")
foreach (source ${genlib_SRCS})
  get_filename_component(newSource ${source}
                         REALPATH BASE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/genlib")
  list(APPEND real_genlib_SRCS ${newSource})
endforeach ()

# get all the sources from salalib
get_target_property(salalib_SRCS salalib SOURCES)

# fix paths if they are not absolute
set(real_salalib_SRCS "")
foreach (source ${salalib_SRCS})
  get_filename_component(newSource ${source}
                         REALPATH BASE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/salalib")
  list(APPEND real_salalib_SRCS ${newSource})
endforeach ()

add_library(alcyon_gensala STATIC ${real_genlib_SRCS} ${real_salalib_SRCS})


