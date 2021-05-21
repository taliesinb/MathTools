Package["GraphTools`"]

PackageImport["GeneralUtilities`"]


(**************************************************************************************************)

PackageExport["HighlightGraphRegion"]

SetUsage @ "
HighlightGraphRegion[graph$, highlights$] highlights regions of graph$ according to highlights$.
<*$GraphRegionHighlightUsage*>
* HighlightGraphRegion returns a Graph in which the option GraphRegionHighlight has been set to \
highlghts$. Any pre-existing highlights are preserved.
"

DeclareArgumentCount[HighlightGraphRegion, 2];

declareSyntaxInfo[HighlightGraphRegion, {_, _, OptionsPattern[]}];

HighlightGraphRegion[graph_, highlights_] := Scope[
  oldHighlights = AnnotationValue[graph, GraphRegionHighlight];
  oldHighlights = If[FailureQ[oldHighlights], {}, Developer`ToList @ oldHighlights];
  Annotate[graph, GraphRegionHighlight -> Join[oldHighlights, Developer`ToList @ highlights]]
];

(**************************************************************************************************)

PackageExport["GraphRegionHighlightGraphics"]

GraphRegionHighlightGraphics[graph_, regionSpec_] := Scope[

  graph = CoerceToGraph[1];

  GraphPlotScope[graph,
    {graphics, padding} = resolveGraphRegionHighlightGraphics @ regionSpec;
    graphics = Part[graphics, All, 2];
    plotRange = $GraphPlotRange
  ];

  If[FailureQ[graphics], ReturnFailed[]];

  Graphics[graphics, PlotRange -> plotRange, Framed -> False]
];

(**************************************************************************************************)

(********************************************)
(** highlight processing code              **)
(********************************************)

PackageScope["resolveGraphRegionHighlightGraphics"]

resolveGraphRegionHighlightGraphics[None | {} | <||>] :=
  {{}, None, 0};

resolveGraphRegionHighlightGraphics[spec_] := Scope[

  $highlightStyle = $GraphHighlightStyle;

  (* GraphHighlightStyle -> Opacity[o] will still use the default color palette but control it's opacity *)
  $defaultOpacity = If[!MatchQ[$highlightStyle, $opacityPattern], 0.5,
    $highlightStyle = Automatic; ExtractFirstOpacity[$highlightStyle]];

  (* toMultiDirective will interpret palette specs passed to GraphHighlightStyle, which will
  turn into lists. it will also map over lists and associations to control lists or associations of regions *)
  $highlightStyle = toMultiDirective @ Replace[$highlightStyle, Automatic -> Offset["Dark", 2]];

  (* if no opacity was specified, use the default opacity *)
  $highlightOpacity = ExtractFirstOpacity[$highlightStyle];
  SetNone[$highlightOpacity,
    $highlightStyle = SetColorOpacity[$highlightStyle, $defaultOpacity]; $defaultOpacity];

  $plotWidth = First[$GraphPlotSize];
  $highlightRadius = $GraphMaxSafeVertexSize;
  $pointSize = $highlightRadius / $plotWidth;
  $radiusScaling = 0.5;

  $zorder = 1;
  $requiredPadding = 0;
  $pathStyle = "Line";
  $arrowheadPosition = 1.0;
  $arrowheadSize = 1;
  $edgeSetback = 1.5;
  $pathWinding = True;

  $colorPalette = ToColorPalette[Automatic];
  CollectTo[{$highlightsBag, $legendsBag}, processOuterSpec[spec]];
  legend = If[$legendsBag === {}, None, DiscreteColorLegend @ Association @ $legendsBag];

  $highlightsBag = Sort[$highlightsBag];
  {Part[$highlightsBag, All, {1, 3}], legend, $requiredPadding}
];

requirePadding[p_] := $requiredPadding = Max[$requiredPadding, p];
requirePaddingPointSize[ps_] := requirePadding[ps / First[$GraphPlotSize] * $GraphPlotImageWidth];

sowHighlight[g_] := Internal`StuffBag[$highlightsBag, {$zorder, Internal`BagLength[$highlightsBag], g}];

sowLegend[name_, color_] := Internal`StuffBag[$legendsBag, name -> color];

(**************************************************************************************************)

processOuterSpec = MatchValues[
  spec_ ? Developer`ListOrAssociationQ :=
    If[!Developer`ListOrAssociationQ[spec],
      Scan[processGeneralSpec, spec],
      $i = 1; MapIndexed[processIndexedStyleSpec, spec]
    ];
  (* this applies styles before iterating over a list or associatino *)
  s_Style :=
    processStyleSpec[s, processOuterSpec];
  other_ :=
    If[Developer`ListOrAssociationQ[$highlightStyle],
      Block[
        {$highlightStyle = First @ $highlightStyle},
        processGeneralSpec[other];
      ],
      processGeneralSpec[other];
    ];
];

GraphRegionHighlight::missspecstyle = "Cannot find a style to use for spec part ``.";

processIndexedStyleSpec[spec_, {part_}] := Block[
  {i = If[ListQ[$highlightStyle], $i++, part]},
  Block[{$highlightStyle = Quiet @ Check[Part[$highlightStyle, i], $Failed]},
    If[FailureQ[$highlightStyle],
      Message[GraphRegionHighlight::missspecstyle, part],
      processGeneralSpec @ If[Head[part] === Key, Legended[spec, First @ part], spec];
    ];
  ];
];

(**************************************************************************************************)

PackageExport["ZOrder"]

SetUsage @ "
ZOrder is an option that controls how graphical elements are sorted from back to front.
"

(**************************************************************************************************)

PackageExport["PathStyle"]

SetUsage @ "
PathStyle is an option that controls how paths are rendered in highlights.
"

(**************************************************************************************************)

GraphRegionHighlight::badelem = "Unknown highlight element ``.";

PackageExport["HighlightRadius"]

SetUsage @ "
HighlightRadius is an option that controls the radius of highlighting.
"

PackageExport["EdgeSetback"]

SetUsage @ "
EdgeSetback is an option that controls how far an edge should be set back from final vertex.
"


processGeneralSpec = MatchValues[
  Legended[Style[spec_, opts__], label_] :=
    % @ Style[Legended[spec, label], opts];
  Legended[spec_, label_] := (
    % @ spec;
    sowLegend[label, $highlightStyle];
  );
  Axes -> spec_ :=
    processAxesSpec @ spec;
  spec_Style :=
    processStyleSpec[spec, %];
  Labeled[spec_, _] :=
    % @ spec;
  region_ ? GraphRegionElementQ :=
    resolveHighlightSpec @ region;
  Arrow[spec_List] /; VectorQ[spec, validVertexQ] :=
    resolveHighlightSpec @ Arrow @ Line @ spec;
  Arrow[spec_] := (
    $pathStyle = "Arrow"; % @ spec;
  );
  Null := Null;
  list_List :=
    Scan[processGeneralSpec, list];
  other_ :=
    Message[GraphRegionHighlight::badelem, Shallow[other]];
];

(**************************************************************************************************)

processAxesSpec[(All|_Integer|{__Integer})] := Scope[
  colors = LookupCardinalColors[$Graph];
  KeyValueMap[axisHighlight, Part[colors, If[IntegerQ[spec], {spec}, spec]]]
];

processAxesSpec[Axes -> spec_] := Scope[
  colors = LookupCardinalColors[$Graph];
  words = Map[parseCardinalWord, Developer`ToList @ spec];
  colors = Association @ Map[#1 -> blendColors[Sort @ Lookup[colors, StripNegated /@ #1]]&, words];
  KeyValueMap[axisHighlight, colors]
];

axisHighlight[word_, color_] := Scope[
  path = First @ resolveHighlightSpec @ InfiniteLine[GraphOrigin, word];
  If[Length @ DeleteDuplicates @ First @ path <= 2, Return[]];
  processHighlightSpec @ Style[path, color];
];

blendColors[{$Blue, $Red}] := $Pink;
blendColors[{$Blue, $Green}] := $Teal;
blendColors[{$Green, $Red}] := $Orange;
blendColors[colors_] := OklabBlend[colors];

(**************************************************************************************************)

GraphRegionHighlight::badstylespec = "Unknown style specification ``.";

processStyleSpec[spec_, f_] := Scope[
  $innerFunc =f ;
  iProcessStyleSpec @ spec
];


Style;
SyntaxInformation[Style];
Options[Style];

$additionalStyleOptions = {PerformanceGoal, PathStyle, ArrowheadPosition, ArrowheadSize, HighlightRadius, EdgeSetback};
Unprotect[Style];
SyntaxInformation[Style] = ReplaceOptions[
  SyntaxInformation[Style],
  "OptionNames" -> Union[Keys @ Options @ Style, SymbolName /@ $additionalStyleOptions]
];
Protect[Style];

iProcessStyleSpec = MatchValues[
  Style[most__, style:$ColorPattern] := Block[
    {$highlightStyle = SetColorOpacity[RemoveColorOpacity @ style, $highlightOpacity]},
    % @ Style @ most
  ];
  Style[most__, PerformanceGoal -> goal_] := Scope[
    $perfGoal = goal;
    % @ Style @ most
  ];
  Style[most__, PathStyle -> style_] := Scope[
    $pathStyle = style;
    % @ Style @ most
  ];
  Style[most__, ArrowheadPosition -> pos_] := Scope[
    $arrowheadPosition = N[pos];
    % @ Style @ most
  ];
  Style[most__, ArrowheadSize -> sz_] := Scope[
    $arrowheadSize = sz;
    % @ Style @ most
  ];
  Style[most__, HighlightRadius -> r_] := Scope[
    $radiusScaling = r;
    % @ Style @ most
  ];
  Style[most__, EdgeSetback -> r_] := Scope[
    $edgeSetback = r;
    % @ Style @ most
  ];
  Style[most__, "Background"] :=
    % @ Style[most, Opaque, ZOrder -> -10];
  Style[most__, "Foreground"] :=
    % @ Style[most, Opaque, ZOrder -> 10];
  Style[most__, ZOrder -> z_Integer] := Scope[
    $zorder = z;
    % @ Style @ most
  ];
  Style[most__, o:$opacityPattern] := Block[
    {$highlightOpacity = ExtractFirstOpacity[o]}, Block[
    {$highlightStyle = SetColorOpacity[RemoveColorOpacity @ $highlightStyle, $highlightOpacity]},
    % @ Style @ most
  ]];
  Style[elem_] :=
    $innerFunc[elem];
  Style[__, remaining_] :=
    Message[GraphRegionHighlight::badstylespec, remaining];
];

(**************************************************************************************************)

resolveHighlightSpec[region_] := Scope[
  results = Developer`ToList @ processRegionSpec @ region;
  Scan[highlightRegion, results]
];

GraphRegionHighlight::interror = "Internal error: couldn't highlight `` data.";

highlightRegion[other_] := (
  Message[GraphRegionHighlight::interror, other];
);

highlightRegion[GraphRegionData[vertices_, edges_]] := Scope[
  If[$perfGoal === "Speed",
    Return @ highlightRegion @ GraphRegionData[vertices, {}]];
  graphics = subgraphCoveringGraphics[
    $highlightRadius * $radiusScaling / 3, vertices, edges,
    $IndexGraphEdgeList, $GraphVertexCoordinates, $GraphEdgeCoordinateLists
  ];
  sowHighlight[{$highlightStyle, graphics}];
];

highlightRegion[GraphRegionData[vertices_, {}]] :=
  sowVertexPoints @ vertices;

sowVertexPoints[vertices_] := Scope[
  pointSize = $radiusScaling * $pointSize * 1;
  requirePaddingPointSize[pointSize];
  sowHighlight @ {
    $highlightStyle,
    PointSize[pointSize],
    Point @ Part[$GraphVertexCoordinates, DeleteDuplicates @ vertices]
  }
];

highlightRegion[GraphPathData[vertices_, {}, {}]] :=
  sowVertexPoints @ vertices;

$currentRegionAnnotations = <||>;
highlightRegion[GraphRegionAnnotation[data_, anno_]] := Scope[
  $currentRegionAnnotations = anno;
  highlightRegion @ data;
]

highlightRegion[GraphPathData[vertices_, edges_, negations_]] := Scope[
  segments = Part[$GraphEdgeCoordinateLists, edges];
  numSegments = Length @ segments;
  thickness = $radiusScaling * $pointSize / 2;
  thicknessRange = thickness * $plotWidth;
  If[negations =!= {}, segments //= MapAt[Reverse, List /@ negations]];
  shortenings = Lookup[$currentRegionAnnotations, PathOffset, {}];
  shortenings //= Map[processShortenings] /* Association;
  lastIsShortened = KeyExistsQ[shortenings, numSegments] && !StringQ[shortenings[numSegments]];
  segments = joinSegments[segments, shortenings];
  doArrow = MatchQ[$pathStyle, "Arrow" | "DiskArrow"];
  pathPrimitives = If[$GraphIs3D,
    Tube[segments, thickness / 5]
  ,
    curve = JoinedCurve[Line @ segments];
    If[doArrow,
      arrowheads = Arrowheads @ List @ List[
        thickness, $arrowheadPosition,
        makeHighlightArrowheadShape[
          Directive[Opacity @ $highlightOpacity, JoinForm @ "Miter", CapForm @ "Round"], 5
        ]
      ];
      disk = If[$pathStyle === "DiskArrow", Disk[Part[$GraphVertexCoordinates, First @ vertices], $highlightRadius/4], Nothing];
      capForm = CapForm @ If[$arrowheadPosition == 1, None, "Round"];
      arrow = If[!lastIsShortened, Arrow[curve, {0, $edgeSetback * thicknessRange}], Arrow[curve]];
      {disk, arrowheads, arrow}
    ,
      curve
    ]
  ];
  zrequirePaddingPointSize[2 * thickness * If[doArrow, 1.8, 1]];
  sowHighlight @ {
    If[$GraphIs3D && $highlightOpacity < 0.9, FaceForm[$highlightStyle, None], $highlightStyle],
    JoinForm @ "Round", CapForm @ "Round",
    Thickness @ thickness,
    pathPrimitives
  };
];

joinSegments[segments_, shortenings_] := Scope[
  numSegments = Length @ segments;
  $offsetVector = 0; isLast = False;
  line = Internal`Bag[
    applyOffsetToSegment[Part[segments, 1], Lookup[shortenings, 1, 0]]
  ];
  Do[
    isLast = i == numSegments;
    segment = PlusVector[$offsetVector] @ Part[segments, i];
    segment = Rest @ applyOffsetToSegment[segment, Lookup[shortenings, i, 0]];
    Internal`StuffBag[line, segment, 1],
    {i, 2, numSegments}
  ];
  Internal`BagPart[line, All]
];

GraphRegionHighlight::badshortening = "PathOffset spec `` is invalid.";

processShortenings = MatchValues[
  z_Integer := modLen[z] -> -1;
  z_Integer -> m_Integer := modLen[z] -> m;
  z_Integer -> Scaled[n_] := modLen[z] -> Scaled[n];
  z_Integer -> "Bend" := Splice[{z -> "BendOut", z+1 -> "BendIn"}];
  other_ := (Message[GraphRegionHighlight::badshortening, other]; {})
];

modLen[z_] := Mod[z, numSegments + 1, 1];

applyOffsetToSegment[segment_, 0] := segment;
applyOffsetToSegment[segment_, offset_] := Scope[
  {first, last} = FirstLast @ segment;
  translated = PlusVector[segment, -first];
  dist = EuclideanDistance[first, last];
  newSegment = Match[offset,
    Scaled[n_]:> scaleSegment[translated, n],
    "BendOut" :> bendOutSegment[translated],
    "BendIn" :> bendInSegment[translated],
    n_ ? Positive :> extendSegment[translated, n],
    n_ ? Negative :> truncateSegment[translated, n]
  ];
  newSegment //= PlusVector[first];
  If[!StringQ[offset], $offsetVector += Last[newSegment] - last];
  newSegment
];

bendOutSegment[coords_] := Scope[
  truncatedCoords = Most @ truncateSegment[coords, 2];
  $bendPoint ^= Last @ truncatedCoords;
  truncatedCoords
];

bendInSegment[coords_] := Scope[
  truncatedCoords = Rest @ Reverse @ truncateSegment[Reverse @ coords, 2];
  {a, b, c} = {$bendPoint, First @ coords, First @ truncatedCoords};
  mid1 = Mean[{a, a, b, b, c}];
  mid2 = Mean[{a, a, b, b, b, c, c}];
  mid3 = Mean[{a, b, b, c, c}];
  (* the first is a dummy that will be dropped by the Rest in joinSegments *)
  Join[{{0, 0}, mid1, mid2, mid3}, truncatedCoords]
];

scaleSegment[coords_, n_] := Scope[
  margin = n * thicknessRange * If[isLast, 2, 1.5];
  scaling = (dist + margin) / dist;
  offset * scaling
];

finalDelta[coords_, n_] := Normalize[Last @ coords] * thicknessRange * n;

extendSegment[coords_, n_] := Scope[
  delta = finalDelta[coords, n];
  MapAt[PlusOperator[delta], coords, -1]
];

truncateSegment[coords_, n_] := Scope[
  final = Last[coords] + finalDelta[coords, n];
  coords = DropWhile[Reverse @ coords, EuclideanDistance[#, final] < thicknessRange * n&];
  Append[Reverse @ coords, final]
];

(**************************************************************************************************)

subgraphCoveringGraphics[r_, vertices_, edgeIndices_, edgeList_, vertexCoords_, edgeCoordsLists_] := Scope[
  vertexPoints = Part[vertexCoords, vertices];
  center = Mean[vertexPoints];
  radius = Max[SquaredDistanceMatrix[vertexPoints, {center}]];
  externalCoords = Part[vertexCoords, Complement[Range @ Length @ vertexCoords, vertices]];
  If[(other = Min[SquaredDistanceMatrix[externalCoords, {center}]]) > radius,
    dr = (other - radius)/3;
    Return @ Disk[center, Sqrt[radius + dr]];
  ];
  edgePoints = DeleteDuplicates @ Flatten[edgeSpaced /@ Part[edgeCoordsLists, edgeIndices], 1];
  points = ToPacked @ Join[vertexPoints, edgePoints];
  primitives = PointDilationGraphics[points, r];
  If[ContainsQ[primitives, Polygon[_Rule]],
    primitives = primitives /. p:Polygon[_Rule] :> removeTrivialHoles[p, externalCoords];
  ];
  primitives
];

containsAnyPointsQ[coords_, points_] := Scope[
  bbox = CoordinateBounds[coords];
  points = Select[points, VectorBetween[bbox]];
  If[points === {}, False,
    memberFunc = RegionMember @ ConvexHullMesh @ coords;
    AnyTrue[points, memberFunc]
  ]
];

removeTrivialHoles[Polygon[coords_ -> holes_], ext2_] := Scope[
  bbox = CoordinateBounds[coords];
  ext = Select[ext2, VectorBetween[bbox]];
  makePolygon[coords, Select[holes, hole |-> containsAnyPointsQ[hole, ext]]]
];

makePolygon[coords_, {}] := Polygon[coords];
makePolygon[coords_, holes_] := Polygon[coords -> holes];

edgeSpaced[{a_, b_}] := Table[i * a + (1-i) * b, {i, .125, .875, .125}];
edgeSpaced[list_List] := Mean[list];
