PackageExport["HighlightGraphRegion"]

SetUsage @ "
HighlightGraphRegion[graph$, highlights$] highlights regions of graph$ according to highlights$.
<*$GraphRegionHighlightUsage*>
* HighlightGraphRegion returns a Graph in which the option GraphRegionHighlight has been set to \
highlghts$. Any pre-existing highlights are preserved.
"

DeclareArgumentCount[HighlightGraphRegion, 2];

Options[HighlightGraphRegion] = $simpleGraphOptionRules;

declareSyntaxInfo[HighlightGraphRegion, {_, _, OptionsPattern[]}];

HighlightGraphRegion[graph_, highlights_, opts:OptionsPattern[]] := Scope[
  oldHighlights = AnnotationValue[graph, GraphRegionHighlight];
  oldHighlights = If[FailureQ[oldHighlights], {}, ToList @ oldHighlights];
  ExtendedGraph[
    graph,
    GraphRegionHighlight -> Join[oldHighlights, ToList @ highlights],
    opts
  ]
];

(**************************************************************************************************)

PackageExport["GraphRegionGraphics"]

GraphRegionGraphics[graph_, regionSpec_] := Scope[

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

  $highlightRadius = $GraphMaxSafeVertexSize;
  $pathRadius = 3;
  $pointSize = $highlightRadius / First[$GraphPlotSize];
  $radiusScaling = 0.5;

  $zorder = 1;
  $requiredPadding = 0;
  $pathStyle = "Line";
  $arrowheadPosition = 1.0;
  $arrowheadSize = 1;
  $edgeSetback = 1.5;
  $outline = False;

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

debugGraphic[g_] := (Internal`StuffBag[$highlightsBag, {100, 100, {Black, g}}]; g);

echoGraphic[g_] := (Echo[Graphics[g, ImageSize -> {200, 200}]]; g);

findExistingArrowheadSpec[] := FirstCase[$GraphPlotGraphics, _Arrowheads, None, {0, Infinity}];

(**************************************************************************************************)

processOuterSpec = MatchValues[
  spec_ ? ListOrAssociationQ :=
    If[!ListOrAssociationQ[spec],
      Scan[processGeneralSpec, spec],
      $i = 1; MapIndexed[processIndexedStyleSpec, spec]
    ];
  (* this applies styles before iterating over a list or associatino *)
  s_Style :=
    processStyleSpec[s, processOuterSpec];
  other_ :=
    If[ListOrAssociationQ[$highlightStyle],
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

PackageExport["PathRadius"]

SetUsage @ "
PathRadius is an option that controls how paths are rendered in highlights.
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

processAxesSpec[spec:(All|_Integer|{__Integer})] := Scope[
  colors = LookupCardinalColors[$Graph];
  KeyValueMap[axisHighlight, Part[colors, If[IntegerQ[spec], {spec}, spec]]]
];

processAxesSpec[spec_] := Scope[
  colors = LookupCardinalColors[$Graph];
  words = Map[ParseCardinalWord, ToList @ spec];
  colors = Association @ Map[#1 -> blendColors[Sort @ Lookup[colors, StripNegated /@ #1]]&, words];
  KeyValueMap[axisHighlight, colors]
];

axisHighlight[word_, color_] := Scope[
  path = First @ processRegionSpec @ InfiniteLine[GraphOrigin, word];
  If[Length @ DeleteDuplicates @ First @ path <= 2, Return[]];
  processGeneralSpec @ Style[path, color, HighlightRadius -> 1];
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

PackageExport["PathOutline"]
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
  Style[most__, PathOutline -> out_] := Scope[
    $outline = out;
    % @ Style @ most
  ];
  Style[most__, EdgeSetback -> r_] := Scope[
    $edgeSetback = r;
    % @ Style @ most
  ];
  Style[most__, "Background"] := % @ Style[most, Opaque, ZOrder -> -10];
  Style[most__, "Foreground"] := % @ Style[most, Opaque, ZOrder -> 10];
  Style[most__, "Overlay"] := % @ Style[most, Opaque, ZOrder -> 10, PathStyle -> "Overlay"];
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
  results = ToList @ processRegionSpec @ region;
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
    $IndexGraphEdgeList, $VertexCoordinates, $EdgeCoordinateLists
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
    Point @ Part[$VertexCoordinates, DeleteDuplicates @ vertices]
  }
];

highlightRegion[GraphPathData[vertices_, {}, {}]] /; $pathStyle =!= "Overlay" :=
  sowVertexPoints @ vertices;

$currentRegionAnnotations = <||>;
highlightRegion[GraphRegionAnnotation[data_, anno_]] := Scope[
  $currentRegionAnnotations = anno;
  highlightRegion @ data;
]

highlightRegion[GraphPathData[vertices_, edges_, negations_]] := Scope[

  segments = Part[$EdgeCoordinateLists, edges];
  If[negations =!= {},
    segments //= MapAt[Reverse, List /@ negations]];
  numSegments = Length @ segments;

  thickness = $pathRadius / $GraphPlotImageWidth;
  thicknessRange = thickness * First[$GraphPlotSize] * 1.5;

  adjustments = parseAdjustments /@ Lookup[$currentRegionAnnotations, PathAdjustments, {}];

  lastIsModified = !MatchQ[Lookup[adjustments, numSegments], _Missing | _String];
  segments = joinSegments[segments, adjustments, $pathStyle =!= "Overlay"];

  doArrow = MatchQ[$pathStyle, "Arrow" | "DiskArrow"];

  darkerColor := OklabDarker[RemoveColorOpacity @ $highlightStyle, .1];
  darkerStyle = If[$highlightOpacity == 1, darkerColor, Nothing];
  setbackDistance = If[lastIsModified || !doArrow, 0, $edgeSetback * thicknessRange];

  pathPrimitives = If[$GraphIs3D,
    Tube[segments, thicknessRange / 3]
  ,
    Switch[$pathStyle,
     "Arrow" | "DiskArrow",
        arrowheads = Arrowheads @ List @ List[
          thickness, $arrowheadPosition,
          makeHighlightArrowheadShape[
            {Opacity @ $highlightOpacity, darkerStyle, JoinForm @ "Miter", CapForm @ "Round"}, 5
          ]
        ];
        disk = If[$pathStyle === "DiskArrow", Disk[Part[$VertexCoordinates, First @ vertices], $highlightRadius/4], Nothing];
        arrow = setbackArrow[segments, setbackDistance];
        initialDot = {darkerStyle, PointSize[thickness * 2], Point @ First @ segments};
        {disk, arrowheads, arrow, initialDot}
      ,
      "Overlay",
        newVertices = {};
        TransformGraphPlotPrimitives[removeHighlightedPathEdges, edges, "EdgePrimitives"];
        TransformGraphPlotPrimitives[removeHighlightedPathVertices, vertices, "VertexPrimitives"];
        newEdges = If[edges === {}, Nothing, Style[Arrow @ segments, arrowheads, CapForm @ "Round"]];
        pathPrimitives = {newVertices, newEdges};
        replaceWithColor[pathPrimitives, $highlightStyle]
      ,
      _,
        setbackLine[segments, setbackDistance]
    ]
  ];

  pathStyle = If[$GraphIs3D && $highlightOpacity < 0.9, FaceForm[$highlightStyle, None], $highlightStyle];

  pathOutline = If[$outline === False, Nothing,
    outlineColor = If[$outline === True, darkerColor, $outline];
    {outlineColor, Thickness[thickness * 1.5], Line[segments]}
  ];

  zrequirePaddingPointSize[2 * thickness * If[doArrow, 1.8, 1]];
  sowHighlight @ {
    JoinForm @ "Round", CapForm @ "Round",
    pathOutline,
    pathStyle, Thickness @ thickness, pathPrimitives
  };
];

replaceWithColor[g_, c_] :=
  ReplaceAll[g, $ColorPattern -> c];

(* we simply delete matching edges, because we will redraw them possibly with adjustments *)
removeHighlightedPathEdges[{old_, new_}] := (
  arrowheads = FirstCase[old, _Arrowheads, None, Infinity];
  old
);

(* we will delete matching vertices, but save them to be redrawn with the highlight color *)
removeHighlightedPathVertices[{old_, new_}] := (
  AppendTo[newVertices, new];
  old
);


(**************************************************************************************************)

setbackArrow[{}, _] := {};

setbackArrow[curve_, 0|0.] := Arrow @ curve;

setbackArrow[curve_, d_] := Scope[
  target = Last @ curve;
  curve = SetbackCoordinates[curve, {0, d}];
  If[curve === {}, Return @ {}];
  last = Last @ curve;
  Arrow @ Append[curve, last + Normalize[target - last] * 1*^-3]
];

setbackLine[curve_, 0|0.] := Line @ curve;

setbackLine[curve_, d_] := Line @ SetbackCoordinates[curve, {0, d}];

joinSegments[{}, _, _] := {};

joinSegments[segments_, adjustments_, shouldJoin_] := Scope[
  numSegments = Length @ segments;
  $offsetVector = 0; isLast = False;
  segments = segments;
  lineBag = Internal`Bag[];
  Replace[adjustments, {
    Rule[{z_, _}, {"Shrink", n_}] :> (Part[segments, z] //= shrinkSegment[n * thicknessRange]),
    Rule[{z_, _}, {"Short", n_}] :> (Part[segments, {z, z + 1}] //= shortSegment[n * thicknessRange])
  }, {1}];
  Do[
    isLast = i == numSegments;
    segment = PlusVector[$offsetVector] @ Part[segments, i];
    mod = Lookup[adjustments, i, 0];
    Switch[mod,
      0,
        Null,
      {"Bend", _} /; !isLast,
        nextSegment = Part[segments, i + 1];
        {segment, nextSegment} = applyBendBetween[segment, nextSegment, 1.5 * Last[mod]];
        Part[segments, i + 1] = nextSegment,
      _ ? Positive,
        {delta, segment} = extendSegment[segment, mod];
        $offsetVector += delta,
      _ ? Negative,
        {delta, segment} = truncateSegment[segment, Abs @ mod];
        If[doArrow && isLast,
          (* this makes sure the arrowhead points at the original target *)
          AppendTo[segment, PointAlongLine[Last[segment], Part[segments, -1, -1], 1*^-3]]];
        $offsetVector += delta
    ];
    If[shouldJoin,
      Internal`StuffBag[lineBag, If[i === 1, Identity, Rest] @ segment, 1],
      Internal`StuffBag[lineBag, segment]
    ];
  ,
    {i, 1, numSegments}
  ];
  Internal`BagPart[lineBag, All]
];

shrinkSegment[d_][segment_] := Scope[
  {a, b} = FirstLast @ segment;
  mid = Mean @ segment;
  a2 = PointAlongLine[a, mid, d];
  b2 = PointAlongLine[b, mid, d];
  scaling = EuclideanDistance[mid, a2] / EuclideanDistance[mid, a];
  translated = PlusVector[segment, -mid];
  segment = PlusVector[translated * scaling, mid];
  Join[{a, a2}, segment, {b2, b}]
];

shrinkSegment[{d1_, d2_}][segment_] := Scope[
  {a, b} = FirstLast @ segment;
  mid = Mean @ segment;
  a2 = PointAlongLine[a, mid, d1];
  b2 = PointAlongLine[b, mid, d2];
  scaling1 = EuclideanDistance[mid, a2] / EuclideanDistance[mid, a];
  scaling2 = EuclideanDistance[mid, b2] / EuclideanDistance[mid, b];
  translated = PlusVector[segment, -mid];
  segment = MapThread[{t, p} |-> t * p + mid, {
    translated,
    Interpolated[scaling1, scaling2, Length @ translated]
  }];
  Join[{a, a2}, segment, {b2, b}]
];

shortSegment[d_][{segmentA_, segmentB_}] := Scope[
  If[ListQ[d], {d1, d2} = d, d1 = d2 = d];
  segmentA = SetbackCoordinates[segmentA, {0, d1}];
  segmentB = SetbackCoordinates[segmentB, {d2, 0}];
  AppendTo[segmentA, First @ segmentB];
  {segmentA, segmentB}
];

GraphRegionHighlight::badpadj = "PathAdjustments element `` is invalid.";

parseAdjustments = MatchValues[
  z_Integer := modLen[z] -> -1;
  z_Integer -> spec:(_Integer | _Scaled | {"Bend", ___}) := modLen[z] -> spec;
  z:{__Integer} -> spec:{"Shrink" | "Short", ___} := modLen[z] -> spec;
  z_ -> {"Expand", n_} := %[z -> {"Shrink", -n}];
  z_ -> spec_String := %[z -> {spec, 1}];
  other_ := (Message[GraphRegionHighlight::badpadj, other]; {})
];

modLen[z_] := Mod[z, numSegments + 1, 1];

(**************************************************************************************************)

applyBendBetween[segment1_, segment2_, d_] := Scope[
  {delta, truncated1} = truncateSegment[segment1, d];
  {delta, truncated2} = truncateSegment[Reverse @ segment2, d];
  bendStart = Last @ truncated1;
  bendEnd = Last @ truncated2;
  circlePoints = circleAround[bendStart, bendEnd, Last @ segment1];
  {
    Join[truncated1, circlePoints],
    Join[Take[circlePoints, -2], Reverse @ truncated2]
  }
];

circleAround[p1_, p2_, q_] := Scope[
  d1 = p1 - q; d2 = p2 - q;
  r = Mean[{Norm @ d1, Norm @ d2}];
  a1 = ArcTan @@ d1; a2 = ArcTan @@ d2;
  an = p + a0; bn = p + b0;
  While[a2 < a1, a2 += Tau]; as1 = DeleteDuplicates @ Append[a2] @ Range[a1, a2, Tau / 16];
  While[a1 < a2, a1 += Tau]; as2 = Reverse @ DeleteDuplicates @ Append[a1] @ Range[a2, a1, Tau / 16];
  as = MinimumBy[{as1, as2}, Length];
  AngleVector[q, {r, #}]& /@ as
];

scaleSegment[coords_, n_] := Scope[
  {first, last} = FirstLast @ segment;
  translated = PlusVector[segment, -first];
  dist = EuclideanDistance[first, last];
  margin = n * thicknessRange * If[isLast, 2, 1.5];
  scaling = (dist + margin) / dist;
  PlusVector[translated * scaling, first]
];

finalDelta[coords_, n_] := Normalize[Part[coords, -1] - Part[coords, -2]] * thicknessRange * n;

extendSegment[coords_, n_] := Scope[
  delta = finalDelta[coords, n];
  {delta, MapAt[PlusOperator[delta], coords, -1]}
];

truncateSegment[coords_, n_] := Scope[
  delta = finalDelta[coords, -n];
  coords = SetbackCoordinates[coords, {0, thicknessRange * n}];
  {delta, coords}
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
