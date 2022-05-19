SetUsage @ "
ExtendedGraph[args$$] acts like Graph but accepts additional options and overrides how graphs are \
displayed.
* The following options and additional options are supported:
| %GraphLayout | None | the overall layout method to use for vertices and edges |
| %LayoutDimension | Automatic | number of dimensions of the graph layout |
| %VertexLabels | None | how to label vertices |
| %ImageSize | Automatic | size to plot the graph |
| %GraphRegionHighlight | None | regions of the graph to highlight |
| %GraphLegend | Automatic | legend to attach to the entire graph |
| %ArrowheadSize | Automatic | size of arrowheads |
| %ArrowheadStyle | Automatic | style of arrowheads |
| %ArrowheadPosition | Automatic | position of arrowheads along edges |
| %ArrowheadShape | Automatic | shape of arrowheads |
| %EdgeSetback | Automatic | how far to set back edges from vertices |
| %EdgeThickness | Automatic | thickness of edges |
| %LabelCardinals | False | whether to attach labels to arrowheads |
| %VertexShapeFunction | Automatic | how to draw vertices |
| %EdgeShapeFunction | Automatic | how to draw edges |
| %VertexColorFunction | None | function to obtain colors for vertices |
| %EdgeColorFunction | None | function to obtain colors for edges |
| %VertexColorRules | None | color vertices by rules |
| %EdgeColorRules | None | color edges by rules |
| %RegionColorRules | None | color vertices and edges by region |
| %VertexAnnotations | None | association of additional per-vertex data |
| %VertexCoordinateRules | None | list of rules for per-vertex coordinates |
| %VertexCoordinateFunction | None | function for computing per-vertex coordinates |
| %EdgeAnnotations | None | association of additional per-edge data |
| %GraphMetric | Automatic | metric to calculate graph distances |
| %CardinalColors | Automatic | association of cardinal colors |
| %VisibleCardinals | All | which cardinals to draw |
| %ViewOptions | Automatic | how to project 3D coordinates |
| %AdditionalImagePadding | None | additional padding to include unconditionally |
| %ViewRegion | All | region of graph to plot |
| %AspectRatioClipping | True | whether to clip aspect ratio |
| %CoordinateTransformFunction | None | function to remap coordinates before plotting |
| %Frame | False | whether to draw a frame |
| %FrameStyle | Automatic | color of frame |

## Arrowheads

* Any of the specifications below can also be given in the form <|card$1 -> spec$1, $$|>.

* %ArrowheadShape accepts these settings:
| 'Arrow' | solid kinded arrowhead (default) |
| 'Line' | partial triangle formed by two lines |
| 'DoubleLine' | two closely spaced partial triangles |
| 'Triangle' | triangle formed by three lines |
| 'HalfTriangle' | half triangle above the edge |
| 'Disk' | circular disk |
| 'Square' | square |
| 'Diamond' | diamond (rotated square) |
| 'Cone' | thin cone (3D) |
| 'Sphere' | sphere (3D) |
| 'Cardinal' | no arrowhead, use cardinal label |
| 'CrossLine' | a horizontal line |
| 'CrossBar' | a thick horizontal line |
| 'Tube' | a tube |
| None | no arrowheads |

* In addition, %ArrowheadShape supports suboptions via {'shape$', subopts$$}:
| %InversionStyle | 'Flip' | how to plot inverted cardinals in %CardinalSet[$$] |
| %TwoWayStyle | 'In' | how to plot inverted pairs in %CardinalSet[$$] |
| %PairedDistance | 0 | how far away to plot inverted pairs |
| %EdgeThickness | 1 | thickness of line-based arrowheads |

* %TwoWayStyle -> spec$ determines how to plot a cardinal and its inversion together:
| 'Out' | arrowheads facing away from each other |
| 'OutClose' | facing out with backs touching |
| 'In' | arrowheads facing towards each other |
| 'InClose' | facing in with tips touching |
| 'spec$' | one of the regular shapes |

* %InversionStyle -> spec$ determines how inverted cardinals are drawn:
| 'OverBar' | draw a inversion bar above arrowhead |
| 'UnderBar' | drwa a inversion bar below arrowhead |

* %PairedDistance -> size$ determines the separation of paired cardinals, in points.

* %ArrowheadSize accepts these settings:
| Automatic | use a safe arrowhead size, depending on layout |
| size$ | size$ in points in the final plot |
| Small, Medium, $$ | symbolic size, with Medium being equivalent to 20 |
| %AbsolutePointSize[size$] | equivalent to size$ |
| %PointSize[f$] | a fraction f$ of the width of the final plot |
| %Scaled[r$] | scale the default safe size by r$ |
| %Max[$$], %Min[$$] | max or min of several specifications |

* %ArrowheadStyle can be set to a color or list of directives.

* %CardinalColors -> <|card$1 -> col$1, $$|> determines the colors for arrowheads.

* %ArrowheadPosition -> r$ sets the position of the arrowhead to the fraction r$ along the \
length of the edge.

* %LabelCardinals -> True will add a label to each arrowhead indicating its cardinal.

## Edges

* %EdgeShapeFunction controls how edges are drawn, indepedently of arrowheads, and accepts:
| Automatic | use %Line or %Arrow as appropriate |
| None | do not draw edges |
| f$ | call f$ to obtain graphical primitives |

If a function f$ is given, it is provided with an association containing the following keys:
| 'Coordinates' | the list of {x$, y$} or {x$, y$, z$} coordinates |
| 'Source' | the source vertex |
| 'Target' | the target vertex |
| 'EdgeIndex' | the index of the edge |
| 'Counter' | an integer counter incremented on access |
| 'Shape' | the symbol %Line or %Arrow |
| 'Cardinal' | the cardinal(s) on the edge |
| 'Arrowheads' | the %Arrowheads[$$] expression (or None) |
| 'LabelStyle' | setting of %EdgeLabelStyle |

* The result can contain the expression %UniqueLabel[$$] inside a %Text[$$] primitive.
This will be renumbered so that all labels are ordered according to x$, y$ screen position.

* %EdgeColorFunction accepts these settings:
| None | color via %EdgeStyle (default) |
| 'Cardinal' | color by cardinal present on edge |
| {e$1, e$2, $$} | use values e$i in same order as %VertexList |
| <|e$1 -> val$1, $$, All -> val$|> | assign values to specific edges |
| {region$1 -> val$1, $$, All -> val$} | assign values to edges within specific regions |
| %Paletted[spec$, colors$] | use a given named or explicit color palette |
* If a spec produces non-color values, edges will colored based on the type of data.
* If %GraphLegend -> Automatic, a color legend will be shown.

* %EdgeSetback controls how far an edge should be set back from its endpoints.

* %EdgeThickness controls the thickness of rendered edges, and is given in points.

## Vertices

* %VertexShapeFunction controls how vertices are drawn and accepts these settings:
| Automatic | choose a method automatically |
| 'Point' | use %Point[$$] |
| 'Disk' | use %Disk[$$] |
| 'Sphere' | use %Sphere[$$] |
| 'Ball' | use 'Disk' for 2D and 'Sphere' for 3D |
| 'Square' | square (designed for 'Square' lattice) |
| 'Hexagon' | hexagon (designed for 'Triangular' lattice) |
| None | do not draw vertices |

* %VertexSize accepts these settings:
| Automatic | use a safe default size, depending on layout |
| size$ | size$ in points in the final plot |
| Small, Medium, $$ | symbolic size, with Medium being equivalent to 5 |
| %AbsolutePointSize[size$] | equivalent to size$ |
| %PointSize[f$] | a fraction f$ of the width of the final plot |
| %Scaled[r$] | fraction r$ of the quantiles of inter-vertex distance |
| {v$1 -> s$1, $$, %%All -> s$} | use specific sizes for specific vertices |
| %Max[$$],% Min[$$] | max or min of several specifications |

* %VertexColorFunction accepts these settings:
| None | color via %VertexStyle (default) |
| 'key$' | color with values from setting of %VertexAnnotations |
| {val$1, val$2, $$} | use values val$i in same order as %VertexList |
| <|v$1 -> val$1, $$, All -> val$|> | assign values to specific vertices |
| {region$1 -> val$1, $$, All -> val$} | assign values to vertices within specific regions |
| f$ | apply f$ to vertices to obtain values |
| spec$ -> f$ | apply f$ to result of spec$ |
| %Paletted[spec$, colors$] | use a given named or explicit color palette |
* If a spec produces non-color values, vertices will colored based on the type of data.
* If %GraphLegend -> Automatic, a color legend will be shown.

## Metrics

* %GraphMetric affects the behavior of %MetricDistance, %MetricDistanceMatrix, and %MetricFindShortestPath.
* The following settings are accepted:
| Automatic | the default graph distance |
| 'Euclidean' | root total square of per-cardinal distances |
| 'Chessboard' | maximum of the per-cardinal distances |
| %QuadraticForm[$$] | use a quadratic form |
| {s$1, s$2, $$} | use a particular signature |
| n$ | use a homogenous form of degree n$ |
| f$ | apply f$ to association of per-cardinal distances |

## Labeling

* %VertexLabels determines how to label vertices, and accepts these settings:
| None | do not label vertices (default) |
| Automatic | label with vertex names |
| 'Index' | label with vertex indices |
| 'key$' | label with values from VertexAnnotation |
| %Tooltip[spec$] | label vertices via a tooltip |

* %EdgeLabels determines how to label edges, and accepts these settings:
| None | do not label edges |
| 'Index' | label with edge index |
| Automatic | label edges with their cardinals |

* %VertexLabelStyle and %EdgeLabelStyle accept these settings:
| Automatic | default |
| styles$ | a list, directive, or individual style |
| {styles$, opt$ -> val$, $$} | provide suboptions |
Supported suboptions are:
| %ItemSize | a symbolic, numeric, or %Scaled[$$] value |
| %Background | additional background to distinguish labels |
| %BaseStyle | extra options to control font, etc. |
| %LabelPosition | one of Above, Below, Left, Right, Center, or Automatic |
| %Spacings | size of offset from the labelled element |
* %VertexLabelStyle -> {%LabelPosition -> Automatic} will maximize the distance to adjacent edges.

## Annotations

* %VertexAnnotations can be set to an association between named properties and lists of values.
* The values should be in the same order and length as given by %VertexList.
* These values are accessible via %VertexColorFunction and %VertexLabels.

* %EdgeAnnotations can be set to an asssocation between named properties and associations of values.
* These associations should have keys that are edge indices.
* Tthese values are accessible via %EdgeLabels.

## Highlights and colors

* %GraphRegionHighlight takes a list of regions to highlight, see %GraphRegion.

* %RegionColorRules can be a list of rules of the following forms:
| region$ -> color$ | set color of vertices and edges within region, see %GraphRegion |
| vertex$ -> color$ | set color of a specific vertex |
| edge$ -> color$ | set color of a specific edge |
| {spec$1, $$} -> color$ | set color of several elements at once |

## Legends

* %GraphLegend accepts these settings:
| None | no legend |
| Automatic | attach legends for cardinals, colors, highlights, etc |
| expr$ | use a custom legend given by expr$ |

## Misc

* %CoordinateTransformFunction can be a function, which will be applied to each coordinates, or one of:
| {'Rotate', n$} | rotate by n$ degrees |
| 'Rotate0' | don't rotate |
| 'Rotate90' | rotate 90\[Degree] |
| 'Rotate180' | rotate 180\[Degree] |
| 'Rotate270' | rotate 270\[Degree] |
| 'ReflectHorizontal' | reflect horizontally |
| 'ReflectVertical' | reflect vertically |
| 'BendVertical' | bend vertical edges for layered digraphs |
| {'Snap', n$} | snap vertices to n$ \[Times] n$ grid |
| 'PolarProjection' | spherical polar projection |

* %Padding, whether in %ImagePadding or %AdditionImagePadding, can be specified in these forms:
| None | no padding |
| n$ | pad by n$ on all sides |
| {h$, v$} | pad by h$ horizontally and v$ vertically |
| {{l$, r$}, {b$, t$}} | explicit padding |
| {Left -> l$, $$} | per-side padding |

* %Frame -> True will draw a frame that encompasses the vertices and edges, plus any additional
padding included by %ImagePadding. The effects of %AdditionalImagePadding will not be included.

* The special expression %GraphicsValue[$$] can be used in %Prolog, %Epilog, or by shape functions.
It will be replaced with computed values after plotting is complete. See %GraphicsValue for more information.
"