# SPDX-FileCopyrightText: 2024 Petros Koutsolampros
#
# SPDX-License-Identifier: GPL-3.0-or-later

# A representation of sala's ShapeGraph in R. Holds onto a sala ShapeGraph
# pointer and operates on that

setClass("ShapeGraph", contains = "ShapeMap")
