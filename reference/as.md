# as("sf", "ShapeMap")

This is a direct conversion, for ShapeMap -\> Axial -\> Segment see
[axialToSegmentShapeGraph](https://spatialnous.github.io/alcyon/reference/axialToSegmentShapeGraph.md)

This is a direct conversion, for ShapeMap -\> Axial -\> Segment see
[axialToSegmentShapeGraph](https://spatialnous.github.io/alcyon/reference/axialToSegmentShapeGraph.md)

## Usage

``` r
# S4 method for class 'sf,ShapeMap'
coerce(from, to = "ShapeMap", strict = TRUE)

# S4 method for class 'ShapeMap,sf'
coerce(from, to = "sf", strict = TRUE)

# S4 method for class 'ShapeMap,AxialShapeGraph'
coerce(from, to = "AxialShapeGraph", strict = TRUE)

# S4 method for class 'sf,AxialShapeGraph'
coerce(from, to = "AxialShapeGraph", strict = TRUE)

# S4 method for class 'ShapeMap,SegmentShapeGraph'
coerce(from, to = "SegmentShapeGraph", strict = TRUE)

# S4 method for class 'sf,SegmentShapeGraph'
coerce(from, to = "SegmentShapeGraph", strict = TRUE)
```

## See also

Other ShapeMap:
[`ShapeMap-class`](https://spatialnous.github.io/alcyon/reference/ShapeMap-class.md)

Other AxialShapeGraph:
[`AxialShapeGraph-class`](https://spatialnous.github.io/alcyon/reference/AxialShapeGraph-class.md)

Other SegmentShapeGraph:
[`SegmentShapeGraph-class`](https://spatialnous.github.io/alcyon/reference/SegmentShapeGraph-class.md)
