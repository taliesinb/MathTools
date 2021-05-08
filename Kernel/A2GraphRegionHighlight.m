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
    graphics = resolveGraphRegionHighlightGraphics[regionSpec];
    plotRange = $GraphPlotRange
  ];

  If[FailureQ[graphics], ReturnFailed[]];

  Graphics[graphics, PlotRange -> plotRange, Framed -> False]
];

(**************************************************************************************************)

PackageScope["resolveGraphRegionHighlightGraphics"]

resolveGraphRegionHighlightGraphics[None | {}] :=
  {};

resolveGraphRegionHighlightGraphics[elem_] :=
  resolveGraphRegionHighlightGraphics[{elem}];

resolveGraphRegionHighlightGraphics[list_List] := Scope[
  $baseHighlightStyle = $defaultBaseHighlightStyle;
  $highlightRadius = $GraphMaxSafeVertexSize * 0.8;
  $pointSize = $highlightRadius / First[$GraphPlotSize];
  $roundingRadius = 1.0;
  CollectTo[{$highlightsBag}, Scan[processHighlightSpec, list]];
  $highlightsBag
];

(* what are the inherited options? *)

(* HighlightStyle gives the style of highlighting. *)
(* HighlightElements can be vertices or Edges *)
(* GraphMetric can override the graph's builtin metric *)

(********************************************)
(** highlight processing code              **)
(********************************************)

sowHighlight[g_] := Internal`StuffBag[$highlightsBag, g];

$defaultBaseHighlightStyle := $defaultBaseHighlightStyle = Opacity[0.2, $Green];

$customHighlightedOptions = {
  Background -> Automatic,
  RoundingRadius -> 1
};

Options[HighlightOptionsObject] = $customHighlightedOptions;

GraphRegionHighlight::badelem = "Unknown highlight element ``.";

processHighlightSpec[other_] := Message[GraphRegionHighlight::badelem, Shallow[other]];

processHighlightSpec[Framed] :=
  sowHighlight @ {EdgeForm[{Red, Dashed}], FaceForm[None], Rectangle @@ (Transpose @ $GraphPlotRange)}

processHighlightSpec[expr_ ? GraphRegionElementQ] :=
  processHighlightSpec @ Highlighted @ expr;

processHighlightSpec[Highlighted[elems_, style:$ColorPattern:Automatic, opts:OptionsPattern[]]] := Block[
  {regions = processRegionSpec @ elems,
   $highlightStyle = style,
   $roundingRadius = Replace[
    OptionValue[HighlightOptionsObject, {opts}, RoundingRadius],
    Automatic -> $roundingRadius
  ]},
  SetAutomatic[$highlightStyle, $baseHighlightStyle];
  Scan[highlightIndividualRegion, regions];
];

highlightIndividualRegion[GraphRegionData[vertices_, edges_]] := Scope[
  graphics = subgraphCoveringGraphics[$roundingRadius * $highlightRadius, vertices, edges, $IndexGraphEdgeList, $GraphVertexCoordinates, $GraphEdgeCoordinateLists];
  sowHighlight[{$highlightStyle, graphics}];
];

highlightIndividualRegion[GraphPathData[vertices_, {}, {}]] :=
  {$highlightStyle, PointSize[$roundingRadius * $pointSize], Point @ Part[$GraphVertexCoordinates, vertices]};

highlightIndividualRegion[GraphPathData[vertices_, edges_, negations_]] := Scope[
  edgeCoords = Part[$GraphEdgeCoordinateLists, edges];
  If[negations =!= {}, edgeCoords //= MapAt[Reverse, List /@ negations]];
  pathPrimitives = JoinedCurve[Line /@ edgeCoords];
  sowHighlight @ {
    $highlightStyle, JoinForm["Round"], CapForm["Round"], Thickness[$roundingRadius * 0.75 * $pointSize],
    pathPrimitives
  };
];

(***)

RegionPolygon[region_] :=
  regionComponentPolygon /@ ConnectedMeshComponents[region];

regionComponentPolygon[region_] := Scope[
  polygons = region["BoundaryPolygons"];
  If[Length[polygons] === 1, Return @ First @ polygons];
  outerIndex = MinimumIndexBy[polygons, -Area[#]&];
  outer = Part[polygons, outerIndex];
  holes = Delete[polygons, outerIndex];
  Polygon[Part[outer, 1] -> Part[holes, All, 1]]
];

PointDilationRegion[points_, d_] := Scope[
  bounds = CoordinateBounds[points, 2 * d];
  Check[
    mesh = MeshRegion @ Point @ points;
    rd = RegionDistance[mesh];
    ir = ImplicitRegion[rd[{x,y}] <= d, {x, y}];
    quality = Length[points] < 1000;
    BoundaryDiscretizeRegion[ir, bounds,
      MaxCellMeasure -> If[quality, d/4, d],
      PerformanceGoal -> If[quality, "Quality", "Speed"]
    ]
  ,
    RegionUnion[Region[Disk[#, d]]& /@ points]
  ]
]

minimumSquaredDistance[points_] :=
  Min @ Clip[
    SquaredDistanceMatrix @ points,
    {$MachineEpsilon, $MaxMachineNumber}, {$MaxMachineNumber, $MaxMachineNumber}
  ];

PointDilationGraphics[points_, r_] := Scope[
  points = Developer`ToPackedArray[points];
  If[Length[points] < 1500 && minimumSquaredDistance[points] > (2r)^2,
    GraphicsComplex[points, Disk[#, r]& /@ Range[Length @ points]],
    RegionPolygon @ PointDilationRegion[points, r]
  ]
];

findShortestCycles[graph_] := Scope[
  vindex = VertexIndexAssociation[graph];
  Do[
    cycles = Select[InVertices /@ FindCycle[graph, {i}, All], DuplicateFreeQ];
    If[cycles =!= {}, Return[cycles, Block]];
  ,
    {i, 3, 5}
  ];
  {}
];

VectorBetween[x_, {l_, h_}] := And @@ MapThread[LessEqual, {l, x, h}];
VectorBetween[{x_, y_}, {{xl_, xh_}, {yl_, yh_}}] := xl <= x <= xh && yl <= y <= yh;
VectorBetween[lh_][x_] := VectorBetween[x, lh];

subgraphCoveringGraphics[r_, vertices_, edgeIndices_, edgeList_, vertexCoords_, edgeCoordsLists_] := Scope[
  vertexPoints = Part[vertexCoords, vertices];
  edgePoints = DeleteDuplicates @ Flatten[edgeSpaced /@ Part[edgeCoordsLists, edgeIndices], 1];
  points = Developer`ToPackedArray @ Join[vertexPoints, edgePoints];
  primitives = PointDilationGraphics[points, r];
  If[ContainsQ[primitives, Polygon[_Rule]],
    externalCoords = Part[vertexCoords, Complement[Range @ Length @ vertexCoords, vertices]];
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
