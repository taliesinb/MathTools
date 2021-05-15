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
    plotRange = $GraphPlotRange
  ];

  If[FailureQ[graphics], ReturnFailed[]];

  Graphics[graphics, PlotRange -> plotRange, Framed -> False]
];

(**************************************************************************************************)

PackageScope["resolveGraphRegionHighlightGraphics"]

resolveGraphRegionHighlightGraphics[None | {}] :=
  {{}, 0};

resolveGraphRegionHighlightGraphics[elem_] :=
  resolveGraphRegionHighlightGraphics[{elem}];

resolveGraphRegionHighlightGraphics[list_List] := Scope[
  $baseHighlightStyle = toDirective @ $GraphHighlightStyle;
  $requiredPadding = 0;
  SetAutomatic[$baseHighlightStyle, $defaultBaseHighlightStyle];
  $highlightOpacity = ExtractFirstOpacity[$baseHighlightStyle];
  SetNone[$highlightOpacity, 0.5];
  $highlightRadius = $GraphMaxSafeVertexSize * 0.8;
  $pointSize = $highlightRadius / First[$GraphPlotSize];
  $roundingRadius = 1.0;
  CollectTo[{$highlightsBag}, Scan[processHighlightSpec, list]];
  {$highlightsBag, $requiredPadding}
];

(* what are the inherited options? *)

(* HighlightStyle gives the style of highlighting. *)
(* HighlightElements can be vertices or Edges *)
(* GraphMetric can override the graph's builtin metric *)

(********************************************)
(** highlight processing code              **)
(********************************************)

requirePadding[p_] := $requiredPadding = Max[$requiredPadding, p];
requirePaddingPointSize[ps_] := requirePadding[ps / First[$GraphPlotSize] * $GraphPlotImageWidth];

sowHighlight[g_] := Internal`StuffBag[$highlightsBag, g];

$highlightOpacity = 0.5;
$defaultBaseHighlightStyle := $defaultBaseHighlightStyle = Opacity[0.5, $DarkGreen];

$customHighlightedOptions = {
  Background -> Automatic,
  PerformanceGoal -> "Quality",
  RoundingRadius -> 1
};

Options[HighlightOptionsObject] = $customHighlightedOptions;

GraphRegionHighlight::badelem = "Unknown highlight element ``.";

processHighlightSpec[other_] := Message[GraphRegionHighlight::badelem, Shallow[other]];

processHighlightSpec[Framed] :=
  sowHighlight @ {EdgeForm[{Red, Dashed}], FaceForm[None], Rectangle @@ (Transpose @ $GraphPlotRange)}

processHighlightSpec[Axes -> spec:(All|_Integer|{__Integer})] := Scope[
  colors = LookupCardinalColors[$Graph];
  KeyValueMap[axisHighlight, Part[colors, If[IntegerQ[spec], {spec}, spec]]]
];

processHighlightSpec[Axes -> spec_] := Scope[
  colors = LookupCardinalColors[$Graph];
  words = Map[parseCardinalWord, Developer`ToList @ spec];
  colors = Association @ Map[#1 -> blendColors[Sort @ Lookup[colors, StripNegated /@ #1]]&, words];
  KeyValueMap[axisHighlight, colors]
];

axisHighlight[word_, color_] := Scope[
  path = First @ processRegionSpec @ InfiniteLine[GraphOrigin, word];
  If[Length @ DeleteDuplicates @ First @ path <= 2, Return[]];
  processHighlightSpec @ Highlighted[path, color];
];

blendColors[{$Blue, $Red}] := $Pink;
blendColors[{$Blue, $Green}] := $Teal;
blendColors[{$Green, $Red}] := $Orange;
blendColors[colors_] := OklabBlend[colors];

processHighlightSpec[expr_ ? GraphRegionElementQ] :=
  processHighlightSpec @ Highlighted @ expr;

processHighlightSpec[Highlighted[elems_, style:$ColorPattern:Automatic, opts:OptionsPattern[]]] := Block[
  {regions = processRegionSpec @ elems,
   $perfGoal = OptionValue[HighlightOptionsObject, {opts}, PerformanceGoal],
   $highlightStyle = toDirective @ style,
   $roundingRadius = Replace[
    OptionValue[HighlightOptionsObject, {opts}, RoundingRadius],
    Automatic -> $roundingRadius
  ]},
  SetAutomatic[$highlightStyle, $baseHighlightStyle];
  $highlightStyle = SetColorOpacity[$highlightStyle, $highlightOpacity];
  Scan[highlightIndividualRegion, regions];
];

highlightIndividualRegion[GraphRegionData[vertices_, edges_]] := Scope[
  If[$perfGoal === "Speed",
    Return @ highlightIndividualRegion @ GraphRegionData[vertices, {}]];
  graphics = subgraphCoveringGraphics[$roundingRadius * $highlightRadius, vertices, edges, $IndexGraphEdgeList, $GraphVertexCoordinates, $GraphEdgeCoordinateLists];
  sowHighlight[{$highlightStyle, graphics}];
];

highlightIndividualRegion[GraphRegionData[vertices_, {}]] :=
  sowVertexPoints @ vertices;

sowVertexPoints[vertices_] := Scope[
  pointSize = $roundingRadius * $pointSize * 0.5;
  requirePaddingPointSize[pointSize];
  sowHighlight @ {
    $highlightStyle,
    PointSize[pointSize],
    Point @ Part[$GraphVertexCoordinates, DeleteDuplicates @ vertices]
  }
];

highlightIndividualRegion[GraphPathData[vertices_, {}, {}]] :=
  sowVertexPoints @ vertices;

highlightIndividualRegion[GraphPathData[vertices_, edges_, negations_]] := Scope[
  edgeCoords = Part[$GraphEdgeCoordinateLists, edges];
  pathPrimitives = If[$GraphIs3D,
    Tube[makeTube @ edgeCoords, $highlightRadius * 0.15]
  ,
    If[negations =!= {}, edgeCoords //= MapAt[Reverse, List /@ negations]];
    JoinedCurve[Line /@ edgeCoords]
  ];
  thickness = $roundingRadius * 0.75 * $pointSize;
  requirePaddingPointSize[2 * thickness];
  sowHighlight @ {
    If[$GraphIs3D && $highlightOpacity < 0.9, FaceForm[$highlightStyle, None], $highlightStyle],
    JoinForm["Round"], CapForm["Round"],
    Thickness[thickness],
    pathPrimitives
  };
];

makeTube[list_] := Scope[
  n = Length[list];
  line = Part[list, 1];
  Do[
    segment = Part[list, i];
    lineLast = Last @ line; lineFirst = First @ line;
    segmentLast = Last @ segment; segmentFirst = First @ segment;
    Which[
      approxEqual[lineLast, segmentFirst],
        line = Join[line, Rest @ segment],
      approxEqual[lineLast, segmentLast],
        line = Join[line, Reverse @ Most @ segment],
      approxEqual[lineFirst, segmentLast],
        line = Join[Most @ segment, line],
      approxEqual[lineFirst, segmentFirst],
        line = Join[Reverse @ Rest @ segment, line],
      True,
        Print["OOPS: ", "Last: ", segmentLast, lineLast, " FIRST: ", segmentFirst, lineFirst];
    ],
    {i, 2, n}
  ];
  line
];

approxEqual[a_, b_] := EuclideanDistance[a, b] < 1*^-4;


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
