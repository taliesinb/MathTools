PackageExport["HighlightGraphRegion"]

SetUsage @ "
HighlightGraphRegion[graph$, highlights$] highlights regions of graph$ according to highlights$.
* HighlightGraphRegion returns a %Graph in which the option %GraphRegionHighlight has been set to \
highlights$. Any pre-existing highlights are preserved.
<*$GraphRegionHighlightUsage*>
"

DeclareArgumentCount[HighlightGraphRegion, {2, 3}];

Options[HighlightGraphRegion] = $simpleGraphOptionRules;

declareSyntaxInfo[HighlightGraphRegion, {_, _, OptionsPattern[]}];

HighlightGraphRegion[graph_, highlights_, style:(_List | _String | $ColorPattern), opts:OptionsPattern[]] :=
  HighlightGraphRegion[graph, highlights, GraphHighlightStyle -> style, opts];

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

  If[MatchQ[spec, {__Association}], spec = Join @@ spec];
  If[Head[$highlightStyle] === Directive, $highlightStyle = List @@ $highlightStyle];

  (* strip off style rules and apply them in the standard way *)
  Which[
    ListQ[$highlightStyle] && MemberQ[$highlightStyle, $highlightStylePattern],
      {styleRules, $highlightStyle} = SelectDiscard[$highlightStyle, MatchQ @ $highlightStylePattern];
      If[$highlightStyle === {}, $highlightStyle = Automatic];
      spec = Style[spec, Sequence @@ styleRules],
    MatchQ[$highlightStyle, $highlightStylePattern],
      spec = Style[spec, $highlightStyle];
      $highlightStyle = Automatic,
    True,
      Null
  ];

  (* GraphHighlightStyle -> Opacity[o] will still use the default color palette but control it's opacity *)
  $defaultOpacity = If[!MatchQ[$highlightStyle, $opacityPattern | Style[$opacityPattern, ___]], 0.5,
    $highlightStyle = Automatic; ExtractFirstOpacity[$highlightStyle]];

  (* toMultiDirective will interpret palette specs passed to GraphHighlightStyle, which will
  turn into lists. it will also map over lists and associations to control lists or associations of regions *)
  defaultPalette = If[FreeQ[spec, "Foreground" | "Background" | Opaque | "Replace"], "Dark", "Medium"];
  $highlightStyle = toMultiDirective @ Replace[$highlightStyle, Automatic -> Offset[defaultPalette, 2]];

  (* if no opacity was specified, use the default opacity *)
  $highlightOpacity = ExtractFirstOpacity[$highlightStyle];
  SetNone[$highlightOpacity,
    $highlightStyle = SetColorOpacity[$highlightStyle, $defaultOpacity]; $defaultOpacity];

  $highlightRadius = $GraphMaxSafeVertexSize;

  (* todo: use the same conversion functions as I use in GraphPlotting *)
  $pathRadius = Automatic;
  $graphPlotWidth = First @ $GraphPlotSize;
  $pointSize = $highlightRadius / $graphPlotWidth;
  $radiusScaling = 1;

  $zorder = 1;
  $requiredPadding = 0;
  $pathStyle = "Line";
  $arrowheadPosition = 1.0;
  $arrowheadSize = Automatic;
  $edgeSetback = 1;
  $outline = False;
  $simplifyRegions = True;

  $colorPalette = ToColorPalette[Automatic];
  CollectTo[{$highlightsBag, $legendsBag}, processOuterSpec[spec]];
  legend = If[$legendsBag === {}, None, DiscreteColorLegend @ Association @ $legendsBag];

  $highlightsBag = Sort[$highlightsBag];
  {Part[$highlightsBag, All, {1, 3}], legend, $requiredPadding}
];

requirePadding[p_] := $requiredPadding = Max[$requiredPadding, p];
requirePaddingPointSize[ps_] := requirePadding[ps / $graphPlotWidth * $GraphPlotImageWidth];

sowHighlight[g_] := Internal`StuffBag[$highlightsBag, {$zorder, Internal`BagLength[$highlightsBag], g}];

sowLegend[name_, color_] := Internal`StuffBag[$legendsBag, name -> color];

debugGraphic[g_] := (Internal`StuffBag[$highlightsBag, {100, 100, {Black, g}}]; g);

echoGraphic[g_] := (Echo[Graphics[g, ImageSize -> {200, 200}]]; g);

findExistingArrowheadSpec[] := FirstCase[$GraphPlotGraphics, _Arrowheads, None, {0, Infinity}];

(**************************************************************************************************)

processOuterSpec = MatchValues[
  spec_ ? ListOrAssociationQ := Block[
    {$i = 1, $highlightStyle = $highlightStyle},
    MapIndexed[processIndexedStyleSpec[#1, First @ #2, $i++]&, spec];
  ];
  s_Style :=
    (* this applies styles before iterating over a list or associations *)
    processStyleSpec[s, processOuterSpec];
  other_ := Block[
    {$highlightStyle = $highlightStyle},
    If[ListOrAssociationQ[$highlightStyle], $highlightStyle //= First];
    processGeneralSpec[other];
  ];
];

GraphRegionHighlight::missspecstyle = "Cannot find a style to use for spec part ``.";

processIndexedStyleSpec[spec_, key_, index_] := Block[
  {part},
  part = Which[
    AssociationQ @ $highlightStyle, key,
    ListQ @ $highlightStyle,        index,
    True,                           All
  ];
  If[IntegerQ @ part, part = Min[part, Length @ $highlightStyle]];
  Block[{$highlightStyle = Part[$highlightStyle, part]},
    If[MissingQ[$highlightStyle],
      Message[GraphRegionHighlight::missspecstyle, part],
      processGeneralSpec @ If[Head[key] === Key, Legended[spec, First @ key], spec];
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
HighlightRadius is an option that controls the radius of highlighting regions.
"

PackageExport["PathRadius"]

SetUsage @ "
PathRadius is an option that controls the radius of highlighting paths.
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
    % @ Arrow @ Line @ spec;
  Arrow[spec_] := Block[{$pathStyle = Replace[$pathStyle, "Line" -> "Arrow"]},
    % @ spec;
  ];
  Null := Null;
  list_List :=
    Scan[processGeneralSpec, list];
  other_ :=
    Message[GraphRegionHighlight::badelem, Shallow[other]];
];

validVertexQ[v_] := !FailureQ[findVertexIndex @ v];

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
  processGeneralSpec @ Style[path, color, HighlightRadius -> 2];
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

$additionalStyleOptions = {PerformanceGoal, PathStyle, ArrowheadPosition, ArrowheadSize, PointSize, HighlightRadius, PathRadius, EdgeSetback, SimplifyRegions, ZOrder};
Unprotect[Style];
SyntaxInformation[Style] = ReplaceOptions[
  SyntaxInformation[Style],
  "OptionNames" -> Union[Keys @ Options @ Style, SymbolName /@ $additionalStyleOptions]
];
Protect[Style];

PackageExport["PathOutline"]
PackageExport["SimplifyRegions"]

$namedStyles = "Background" | "Foreground" | "Replace" | "FadeGraph";
$highlightStylePattern = Rule[Alternatives @@ $additionalStyleOptions, _] | $namedStyles;

iProcessStyleSpec = MatchValues[
  Style[most__, style:$ColorPattern] := Block[
    {$highlightStyle = SetColorOpacity[RemoveColorOpacity @ style, $highlightOpacity]},
    % @ Style @ most
  ];
  Style[most__, SimplifyRegions -> boole_] := Scope[
    $simplifyRegions = boole;
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
  Style[most__, PointSize -> sz_] := Scope[
    $pointSize = sz / $GraphPlotImageWidth;
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
  Style[most__, PathRadius -> r_] := Scope[
    $pathRadius = r;
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
  Style[most__, "FadeGraph"] := (
    $GraphPlotGraphics ^= ReplaceAll[$GraphPlotGraphics, {t_Text :> t, _Opacity -> Opacity[1], $ColorPattern -> LightGray}];
    % @ Style[most];
  );
  Style[most__, "Background"] := % @ Style[most, Opaque, ZOrder -> -10];
  Style[most__, "Foreground"] := % @ Style[most, Opaque, ZOrder -> 10];
  Style[most__, "Replace"] := % @ Style[most, Opaque, ZOrder -> 10, PathStyle -> "Replace"];
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
    $highlightRadius * $radiusScaling, vertices, edges,
    $IndexGraphEdgeList, $VertexCoordinates, $EdgeCoordinateLists
  ];
  sowHighlight[{$highlightStyle, graphics}];
];

highlightRegion[GraphRegionData[vertices_, {}]] :=
  sowVertexPoints @ vertices;

sowVertexPoints[vertices_] := Scope[
  requirePaddingPointSize @ $pointSize;
  sowHighlight @ {
    $highlightStyle,
    PointSize @ $pointSize,
    Point @ Part[$VertexCoordinates, DeleteDuplicates @ vertices]
  }
];

highlightRegion[GraphPathData[vertices_, {}, {}]] /; $pathStyle =!= "Replace" && MatchQ[$pathStyle, "Line" | "Arrow"] := Scope[
  $pointSize = 2 * Replace[$pathRadius, Automatic -> 4] / $GraphPlotImageWidth;
  sowVertexPoints @ vertices;
];

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

  pathRadius = $pathRadius;
  SetAutomatic[pathRadius, Switch[$pathStyle,
    "Replace",                            1.0,
    s_ /; StringContainsQ[s, "Arrow"],    4.0,
    "Line",                               6.0
  ]];

  thickness = pathRadius / $GraphPlotImageWidth;
  thicknessRange = thickness * $graphPlotWidth * 1.5;
  bendRange = Max[thicknessRange/2, 5 / $GraphPlotImageWidth * $graphPlotWidth];

  adjustments = parseAdjustments /@ Lookup[$currentRegionAnnotations, PathAdjustments, {}];

  lastIsModified = !MatchQ[Lookup[adjustments, numSegments], _Missing | _String];
  segments = joinSegments[segments, adjustments, $pathStyle =!= "Replace"];

  doArrow = StringContainsQ[$pathStyle, "Arrow"];

  darkerColor := OklabDarker[RemoveColorOpacity @ $highlightStyle, .05];
  darkerStyle = If[$highlightOpacity == 1, darkerColor, Nothing];
  guessVertexSize = FirstCase[$GraphPlotGraphics, PointSize[sz_] :> 1.2 * sz * $graphPlotWidth, 0, Infinity];
  setbackDistance = If[lastIsModified || !doArrow || $arrowheadPosition != 1, 0, Max[$edgeSetback * thicknessRange/2, guessVertexSize]];
  pathPrimitives = If[$GraphIs3D,
    Tube[segments, thicknessRange / 3]
  ,
    Switch[$pathStyle,
     "Arrow" | "DiskArrow" | "DiskArrowDisk" | "ArrowDisk",
        arrowheadSize = $arrowheadSize;
        If[NumericQ[arrowheadSize],
          arrowheadSize = N[arrowheadSize] / $GraphPlotImageWidth];
        SetAutomatic[arrowheadSize, thickness];
        arrowheads = Arrowheads @ List @ List[
          arrowheadSize, $arrowheadPosition,
          makeHighlightArrowheadShape[
            {Opacity @ $highlightOpacity, darkerStyle, JoinForm @ "Miter", CapForm @ "Round"}, 5
          ]
        ];
        diskRadius = pathRadius * 2 / $GraphPlotImageWidth * $graphPlotWidth;
        disk1 = If[!StringStartsQ[$pathStyle, "Disk"], Nothing, Disk[Part[$VertexCoordinates, First @ vertices], diskRadius]];
        disk2 = If[!StringEndsQ[$pathStyle, "Disk"], Nothing, Disk[Part[$VertexCoordinates, Last @ vertices], diskRadius]];
        arrow = setbackArrow[segments, setbackDistance];
        {disk1, arrowheads, arrow, disk2}
      ,
      "Replace",
        newVertices = {}; newArrows = {};
        TransformGraphPlotPrimitives[removeHighlightedPathEdges, edges, "EdgePrimitives"];
        TransformGraphPlotPrimitives[removeHighlightedPathVertices, vertices, "VertexPrimitives"];
        newEdges = Which[
          edges === {}, Nothing,
          adjustments === {}, newArrows,
          True, Style[Arrow @ segments, arrowheads, CapForm @ "Round"]
        ];
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

  (* unfortunately CapForm doesn't do anything for Arrow *)
  requirePadding[If[doArrow, 1.2, 1] * thicknessRange / $graphPlotWidth * $GraphPlotImageWidth];
  sowHighlight @ {
    JoinForm @ "Round", CapForm @ "Round",
    pathOutline,
    pathStyle, Thickness @ thickness, pathPrimitives
  };
];

replaceWithColor[g_, c_] :=
  ReplaceAll[g, {t_Text :> t, $ColorPattern -> c}];

(* we simply delete matching edges, because we will redraw them possibly with adjustments *)
removeHighlightedPathEdges[{old_, new_}] := (
  arrowheads = FirstCase[old, _Arrowheads, None, Infinity];
  AppendTo[newArrows, new];
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
    Rule[{z_, _}, {"Shrink", n_}] :> (Part[segments, z] //= shrinkSegment[n * bendRange]),
    Rule[{z_, _}, {"Short", n_}] :> (Part[segments, {z, z + 1}] //= shortSegment[n * bendRange])
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
  margin = n * bendRange * If[isLast, 2, 1.5];
  scaling = (dist + margin) / dist;
  PlusVector[translated * scaling, first]
];

finalDelta[coords_, n_] := Normalize[Part[coords, -1] - Part[coords, -2]] * bendRange * n;

extendSegment[coords_, n_] := Scope[
  delta = finalDelta[coords, n];
  {delta, MapAt[PlusOperator[delta], coords, -1]}
];

truncateSegment[coords_, n_] := Scope[
  delta = finalDelta[coords, -n];
  coords = SetbackCoordinates[coords, {0, bendRange * n}];
  {delta, coords}
];

(**************************************************************************************************)

subgraphCoveringGraphics[r_, vertices_, edgeIndices_, edgeList_, vertexCoords_, edgeCoordsLists_] := Scope[
  vertexPoints = Part[vertexCoords, vertices];
  center = Mean[vertexPoints];
  radius = Max[SquaredDistanceMatrix[vertexPoints, {center}]];
  externalCoords = Part[vertexCoords, Complement[Range @ Length @ vertexCoords, vertices]];
  If[$simplifyRegions && externalCoords =!= {} && (other = Min[SquaredDistanceMatrix[externalCoords, {center}]]) > radius,
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
