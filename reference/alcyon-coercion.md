# Coercion between sf and alcyon map classes

Direct conversions between `sf` objects and the map classes provided by
this package. For ShapeMap -\> Axial -\> Segment see
[axialToSegmentShapeGraph](https://spatialnous.github.io/alcyon/reference/axialToSegmentShapeGraph.md).

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

## Arguments

- from:

  the object to coerce.

- to:

  the target class. Supplied by
  [`as`](https://rdrr.io/r/methods/as.html) and not used by the method
  bodies.

- strict:

  logical, part of the `coerce` generic signature. Not used by the
  method bodies.

## See also

Other ShapeMap:
[`ShapeMap-class`](https://spatialnous.github.io/alcyon/reference/ShapeMap-class.md)

Other AxialShapeGraph:
[`AxialShapeGraph-class`](https://spatialnous.github.io/alcyon/reference/AxialShapeGraph-class.md)

Other SegmentShapeGraph:
[`SegmentShapeGraph-class`](https://spatialnous.github.io/alcyon/reference/SegmentShapeGraph-class.md)
