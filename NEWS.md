# alcyon 0.7.0

* Sala: Increase calculation accuracy to achieve parity between x86 and arm64 results
* Force isovist polygons to be closed

# alcyon 0.6.0

* Use ALL_CXXFLAGS instead of ALL_OBJCXXFLAGS to build
* Update to latest sala which now contains genlib
* Add pkgdown and fix descriptions
* Fix agent analysis and unlinking sanitizer errors
* Add point-to-point unlinking

# alcyon 0.5.0

* Add multi-threaded analysis for Global VGA (Metric, Topological, Tulip), and for VGA Local analysis (generating the metrics Control, Controllability and Clustering Coefficient), as well as a new algorithm for Local analysis using an adjacency matrix.

# alcyon 0.4.0

* Fix isovist 2-point generation
* Allow one-to-one traversal for pointmaps and segment maps

# alcyon 0.3.0

* Fix CMake issues w.r.t macOS (CMake is under /Applications)
* Overhaul the package to use and extend sf and stars S3 classes
* Add agent-analysis vignette

# alcyon 0.2.0

* Initial CRAN submission.
