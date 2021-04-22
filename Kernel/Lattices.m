Package["GraphTools`"]


PackageImport["GeneralUtilities`"]


PackageExport["VertexCoordinatizationFunction"]
PackageExport["LatticeLayout"]
PackageExport["VertexNaming"]

SetUsage @ "VertexCoordinatizationFunction is an option to GenerateLatticeQuiver."
SetUsage @ "LatticeLayout is an option to GenerateLatticeQuiver."
SetUsage @ "VertexNaming is an option to LatticeGraph and LatticeQuiver."

$baseGenerateLatticeOptions = {
  VertexCoordinatizationFunction -> Automatic,
  ImageSize -> Automatic,
  LatticeLayout -> Automatic,
  ArrowheadSize -> Automatic,
  ArrowheadStyle -> Automatic,
  GraphLegend -> Automatic,
  MaxNorm -> Infinity,
  VertexNaming -> "SpiralIndex",
  NormFunction -> ChessboardDistance,
  VertexLabels -> None, EdgeLabels -> None,
  GraphRegionHighlight -> None
};

General::notquivrep = "First argument should be a QuiverRepresentationObject, or a quiver with canonically named cardinals.";
General::badvcoords = "VertexCoordinatizationFunction did not return vectors of consistent dimensions.";
General::badlatticename = "The specified name `` is not a known name for a lattice graph. Known names are: ``."
General::badlatticedepth = "The specified depth `` is not a positive integer."
General::badvertnaming = "Unknown setting `` for VertexNaming.";

Options[iGenerateLattice] = $baseGenerateLatticeOptions;

iGenerateLattice[head_, quiverRepresentation_, maxDepth_, directedEdges_, opts:OptionsPattern[]] := Scope[

  If[!IntegerQ[maxDepth] || !Positive[maxDepth], ReturnFailed[head::badlatticedepth, maxDepth]];

  If[StringQ[quiverRepresentation],
    quiverRepresentation = Lookup[$latticeQuiverRepresentations, quiverRepresentation, $Failed];
    If[FailureQ[qrep], ReturnFailed[head::badlatticename, name, commaString @ $latticeNames]];
  ];

  If[RuleQ[quiverRepresentation],
    quiverRepresentation = QuiverRepresentation @@ quiverRepresentation;
    If[FailureQ[quiverRepresentation], ReturnFailed[]];
  ];

  If[QuiverQ[quiverRepresentation],
    quiverRepresentation = Quiet @ QuiverRepresentation[quiverRepresentation];
    If[FailureQ[quiverRepresentation], ReturnFailed[head::notquivrep]];
  ];

  If[Head[quiverRepresentation] =!= QuiverRepresentationObject,
    ReturnFailed[head::notquivrep]];

  function = quiverStateToLatticeVertex @ quiverRepresentation["CayleyFunction", "Symmetric" -> True, "Labeled" -> True];
  baseRep = quiverRepresentation["Representation"];
  istate = List @ quiverStateToLatticeVertex @ quiverRepresentation["Identity"];

  quiver = quiverRepresentation["Quiver"];

  UnpackOptionsAs[head, opts,
    maxNorm, normFunction,
    vertexCoordinatizationFunction, latticeLayout,
    graphLegend, imageSize, vertexNaming, arrowheadStyle
  ];

  $quiverLabel := Quiver[quiver, ImageSize -> Tiny,
    ArrowheadStyle -> arrowheadStyle,
    GraphLegend -> Placed[Automatic, Left]];

  Switch[graphLegend,
    "Quiver",
      graphLegend = Placed[$quiverLabel, Right],
    "QuiverRepresentation",
      labeledGenerators = KeyValueMap[Labeled[#2, #1]&, quiverRepresentation["Generators"]];
      graphLegend = Placed[Row[{$quiverLabel, "  ", Row[labeledGenerators, " "]}], Right],
    _,
      None
  ];

  layoutDimension = Automatic; vertexLayout = Automatic;
  finalCoordFunc = None;
  Switch[vertexCoordinatizationFunction,
    Automatic,
      {coordTypes, coordFunc, isRedundant} = findCoordinatizationFunction[First /@ baseRep["Generators"], baseRep["Group"]];
      If[latticeLayout === Automatic && FreeQ[coordTypes, None],
        {is3D, finalCoordFunc} = chooseLatticeCoordinatization[coordTypes, isRedundant]];
      layoutDimension = If[is3D, 3, 2],
    None,
      coordFunc = Identity,
    _,
      coordFunc = Normal /* vertexCoordinatizationFunction;
      lattice
  ];

  (* do the exploration *)
  {vertexList, indexEdgeList, reason} = StateTransitionGraph[function, istate,
    {"VertexList", "IndexEdgeList", "TerminationReason"},
    MaxDepth -> maxDepth, IncludeFrontier -> False, DirectedEdges -> True
  ];

  (* rewrite the vertices via the coordinate function *)
  vertexList = MapAt[coordFunc, vertexList, {All, 1}];

  (* rewrite the indexed edges to use the explicit vertices *)
  edgeList = DeleteDuplicates[indexEdgeList] /.
    DirectedEdge[i_, j_, c_] :> DirectedEdge[Part[vertexList, i], Part[vertexList, j], c];

  If[directedEdges === False, edgeList = UndirectedEdge @@@ edgeList];

  (* the coordinitization might have collapsed some vertices *)
  vertexList //= DeleteDuplicates;

  (* remove any vertices that exceed the norm *)
  If[maxNorm =!= Infinity,
    initialCoord = MapAt[coordFunc, vertexList, 1];
    {vertexList, edgeList} = VertexEdgeList @ Subgraph[
      Graph[vertexList, edgeList],
      LatticeVertex[coords_ /; normFunction[coords, initialCoord] <= maxNorm, _]
    ];
  ];

  (* apply the final layout, if any *)
  If[finalCoordFunc =!= None,
    vertexCoordinates = Map[First /* finalCoordFunc, vertexList];
    If[!MatrixQ[vertexCoordinates], ReturnFailed[head::badvcoords]];
    layoutDimension = Switch[Length @ First @ vertexCoordinates, 2, 2, 3, 3, _, Automatic];
  ,
    vertexCoordinates = Automatic;
    Switch[latticeLayout,
      "2D", vertexLayout = "SpringElectricalEmbedding"; layoutDimension = 2,
      "3D", vertexLayout = "SpringElectricalEmbedding"; layoutDimension = 3,
      Automatic, vertexLayout = "SpringElectricalEmbedding",
      _String, vertexLayout = latticeLayout,
      _, ReturnFailed[];
    ];
  ];

  graphLayout = {"VertexLayout" -> vertexLayout, "Dimension" -> layoutDimension};

  If[head === LatticeGraph && layoutDimension === Automatic,
    (* for non-quiver graphs, we need to decide if the graph is 3D before we construct it,
    since it will change what shape function we use etc. *)
    SetAutomatic[vertexCoordinates, GraphEmbedding[Graph[vertexList, edgeList], graphLayout]];
    graphLayout[[2, 2]] = layoutDimension = Last @ Dimensions @ vertexCoordinates;
  ];

  (* apply the final vertex and edge relabeling *)
  renamingRule = toRenamingRule[vertexNaming, vertexList];
  If[FailureQ[renamingRule], ReturnFailed[head::badvertnaming, vertexNaming]];
  {vertexList, edgeList} = {vertexList, edgeList} /. renamingRule;
  If[VectorQ[vertexList, IntegerQ] && MinMax[vertexList] == {1, Length @ vertices},
    vertexList = Developer`ToPackedArray @ Sort @ vertexList;
    If[ListQ[vertexCoordinates], vertexCoordinates = Part[vertexCoordinates, Ordering @ vertices]];
    edgeList //= Sort;
  ];

  graph = If[head === LatticeGraph,
    SetAutomatic[imageSize, chooseGraphImageSize @ edgeList];
    imageSize //= toStandardImageSize;
    edgeStyle = If[directedEdges,
      {Opacity[0.8], GrayLevel[0.8], Arrowheads[{{Medium, 0.7}}]},
      {Opacity[0.5], GrayLevel[0.6]}
    ];
    Graph[vertexList, edgeList,
      Sequence @@ DeleteOptions[TakeOptions[{opts}, $simpleGraphOptions], {ImageSize, GraphLegend}],
      VertexCoordinates -> vertexCoordinates,
      VertexStyle -> Directive[Opacity[1], EdgeForm[None], GrayLevel[If[layoutDimension === 3, 0, 0.5]]],
      EdgeStyle -> Apply[Directive, edgeStyle],
      Sequence @@ If[layoutDimension === 3, {VertexShapeFunction -> "Point", EdgeShapeFunction -> "Line"}, {}],
      GraphLegend -> graphLegend, ImageSize -> imageSize
    ]
  ,
    imageSize //= toStandardImageSize;
    Quiver[vertexList, edgeList,
      FilterOptions[Graph, DeleteOptions[{opts}, {ImageSize, GraphLegend}]],
      VertexLabels -> Placed["Name", Tooltip],
      GraphLayout -> graphLayout, VertexCoordinates -> vertexCoordinates,
      GraphLegend -> graphLegend, ImageSize -> imageSize
    ]
  ];

  graph
];

quiverStateToLatticeVertex[e_] := e /. QuiverElement[a_, b_] :> LatticeVertex[b, a];

$abc = Transpose @ N @ {{Sqrt[3]/2, -(1/2)}, {0, 1}, {-(Sqrt[3]/2), -(1/2)}};

vecAngle[{0., 0.}] := 0;
vecAngle[{a_, b_}] := ArcTan[a, b];

vecSorter[v_] := Norm[v];
vecSorter[v_] /; Length[v] == 3 := {Norm[v], vecAngle @ Dot[$abc, v]};
vecSorter[v_] /; Length[v] == 2 := {Norm[v], vecAngle @ v};
vecSorter[v_] /; Length[v] == 1 := {Norm[v], v};

toRenamingRule["SpiralIndex", vertices_] :=
  AssociationThread[vertices, Ordering @ Ordering @ Map[vecSorter, N @ vertices[[All, 1]]]];

toRenamingRule["RasterIndex", vertices_] :=
  AssociationThread[vertices, Ordering @ Ordering @ ({-#2, #1}& @@@ N[vertexCoordinates])];

toRenamingRule["Coordinates", _] :=
  LatticeVertex[v_, _] :> v;

toRenamingRule["Index", _] :=
  AssociationRange[vertices];

toRenamingRule[None, _] := {};

toRenamingRule[_, _] := $Failed;

(**************************************************************************************************)

$latticeQuiverRepresentations = <||>;
$latticeNames = {};

SetHoldRest[declareLattice];
declareLattice[names_List, rep_] := (
  AppendTo[$latticeNames, First @ names];
  Scan[
    name |-> SetDelayed[$latticeQuiverRepresentations[name], rep],
    names
  ]
);

(**************************************************************************************************)

$squareQuiver := $squareQuiver =
  BouquetQuiver["xy"];

$squareQRep := $squareQRep =
  QuiverRepresentation[
    $squareQuiver,
    "Abelian"
  ];

declareLattice[{"Square", "Quadrille"}, $squareQRep];

(**************************************************************************************************)

$cubicQuiver := $cubicQuiver =
  BouquetQuiver["xyz"];

$cubicQRep := $cubicQRep =
  QuiverRepresentation[
    $cubicQuiver,
    "Abelian"
  ];

declareLattice[{"Cubic"}, $cubicQRep];

(**************************************************************************************************)

$triangularQuiver := $triangularQuiver =
  BouquetQuiver["abc"];

$triangularQRep := $triangularQRep =
  QuiverRepresentation[
    $triangularQuiver,
    "Redundant"
  ];

declareLattice[{"Triangular", "Deltille"}, $triangularQRep];

(**************************************************************************************************)

$hexagonalQuiver := $hexagonalQuiver =
  Quiver[{DirectedEdge[1, 2, "a" | "b" | "c"]}];

$hexagonalQRep := $hexagonalQRep =
  QuiverRepresentation[
    $hexagonalQuiver,
    "Redundant"
  ];

declareLattice[{"Hexagonal", "Hextille"}, $hexagonalQRep];

(**************************************************************************************************)

$rhombilleQuiver := $rhombilleQuiver =
  Quiver[{DirectedEdge[1, 2, "a" | "b" | "c"], DirectedEdge[2, 3, "a" | "b" | "c"]}];

$rhombilleQRep := $rhombilleQRep =
  QuiverRepresentation[
    $rhombilleQuiver,
    "Redundant"
  ];

declareLattice[{"Rhombille"}, $rhombilleQRep];

(**************************************************************************************************)

vecAngle2[{x_, y_}] := ArcTan[-y + $MachineEpsilon, -x - $MachineEpsilon];
sortAngle[vecs_] := SortBy[vecs, vecAngle2];
Rotate90[{a_, b_}] := {b, -a};

$rhombitrihexagonalQuiver := $rhombitrihexagonalQuiver = Quiver[{
  Labeled[{1 -> 2, 5 -> 4}, 6], Labeled[{6 -> 4, 1 -> 3}, 1], Labeled[{2 -> 6, 3 -> 5}, 5],
  Labeled[{6 -> 1, 4 -> 3}, 4], Labeled[{1 -> 5, 2 -> 4}, 3],
  Labeled[{2 -> 3, 6 -> 5}, Negated @ 2]}
];

$tgroup12 := $tgroup12 = Module[{circle6a, circle6b, circle},
  circle6a = sortAngle @ CirclePoints[6];
  circle6b = Rotate90 /@ circle6a;
  circle = sortAngle @ Join[circle6a, circle6b];
  TranslationGroup[Take[circle, 6]]
];

$rhombitrihexagonalQRep := $rhombitrihexagonalQRep =
  QuiverRepresentation[
    $rhombitrihexagonalQuiver,
    $tgroup12
  ];

declareLattice[{"Rhombitrihexagonal", "Rhombihexadeltille"}, $rhombitrihexagonalQRep];

(**************************************************************************************************)

$trihexagonalQuiver := $trihexagonalQuiver = Quiver[{
    Labeled[{1 -> 3, 3 -> 1}, "a"],
    Labeled[{1 -> 2 ,2 -> 1}, "b"],
    Labeled[{2 -> 3, 3 -> 2}, "c"]
  }];

$trihexagonalQRep := $trihexagonalQRep =
  QuiverRepresentation[
    $trihexagonalQuiver,
    "Redundant"
  ];

declareLattice[{"Trihexagonal", "Hexadeltille"}, $trihexagonalQRep];

(**************************************************************************************************)

$snubTrihexagonalQuiver := $snubTrihexagonalQuiver = Quiver[{
  Labeled[{{3 -> 2, 5 -> 6}, {1 -> 5, 2 -> 4, 6 -> 3}}, "a"],
  Labeled[{{6 -> 1, 4 -> 3}, {1 -> 4, 2 -> 6, 3 -> 5}}, Negated @ "b"],
  Labeled[{{1 -> 2, 5 -> 4}, {2 -> 5, 3 -> 1, 4 -> 6}}, "c"]
}];

$snubTrihexagonalQRep := $snubTrihexagonalQRep =
  QuiverRepresentation[
    $snubTrihexagonalQuiver,
    "cab" -> "Redundant"
  ];

declareLattice[{"SnubTrihexagonal", "SnubHextille"}, $snubTrihexagonalQRep];

(**************************************************************************************************)

$truncatedSquareQuiver := $truncatedSquareQuiver = Quiver[{
  Labeled[{1 -> 2}, "a"],
  Labeled[{4 -> 3}, "b"],
  Labeled[{3 -> 1, 2 -> 4}, "c"],
  Labeled[{1 -> 4, 3 -> 2}, "d"]
}];

$truncatedSquareQRep := $truncatedSquareQRep = QuiverRepresentation[
  $truncatedSquareQuiver,
  TranslationGroup[{{1, 0}, {0, 1}, {1, 1}/2, {-1, 1}/2}]
];

declareLattice[{"TruncatedSquare", "TruncatedQuadrille"}, $truncatedSquareQRep];

(**************************************************************************************************)

$truncatedTrihexagonalQuiver := $truncatedTrihexagonalQuiver =
  Quiver[{
    Labeled[{2 -> 3, 9 -> 8, 5 -> 12, 6 -> 11}, Negated @ "a"],
    Labeled[{3 -> 4, 10 -> 9}, Negated @ "b"],
    Labeled[{5 -> 4, 10 -> 11, 1 -> 8, 2 -> 7}, "c"],
    Labeled[{6 -> 5, 11 -> 12}, "d"],
    Labeled[{7 -> 6, 12 -> 1, 3 -> 10, 4 -> 9}, "e"],
    Labeled[{1 -> 2, 8 -> 7}, "f"]
  }];

$truncatedTrihexagonalQRep := $truncatedTrihexagonalQRep =
  QuiverRepresentation[$truncatedTrihexagonalQuiver, $tgroup12];

declareLattice[{"TruncatedTrihexagonal", "TruncatedHexadeltille"}, $truncatedTrihexagonalQRep];

(**************************************************************************************************)

$namedLatticeUsage = StringTrim @ "
* Named lattices (and their corresponding tilings) include:
| 'Square' | square tiling, aka quadrille |
| 'TruncatedSquare' | truncated square tiling, aka truncated quadrille |
| 'Cubic' | cubic tiling |
| 'Triangular' | triangular tiling, aka deltille |
| 'Hexagonal' | hexagonal tiling, aka hextille |
| 'Rhombille' | rhombille tiling |
| 'Rhombitrihexagonal' | rhombitrihexagonal tiling, aka rhombihexadeltille |
| 'Trihexagonal' | trihexagonal tiling, aka hexadeltille |
| 'SnubTrihexagonal' | snub trihexagonal tiling, aka snub hextille |
| 'TruncatedTrihexagonal' | truncated trihexagonal tiling, aka truncated hexadeltille |
";

(**************************************************************************************************)

PackageExport["LatticeQuiverRepresentation"]

SetUsage @ "
LatticeQuiverRepresentation['name$'] returns the QuiverRepresentation[$$] object for the named latice \
'name$'.
<*$namedLatticeUsage*>
"

DeclareArgumentCount[LatticeQuiverRepresentation, 1];

declareFunctionAutocomplete[LatticeQuiverRepresentation, {$latticeNames}];

LatticeQuiverRepresentation[name_] := Scope[
  Lookup[$latticeQuiverRepresentations, name, $Failed]
];

(**************************************************************************************************)

PackageExport["LatticeGraph"]

SetUsage @ "
LatticeGraph[qrep$, d$] generates the lattice graph for a quiver representation qrep$ to depth d$.
LatticeGraph['name$', d$] generates the lattice graph for the named lattice 'name$' to depth d$.
LatticeGraph[spec$] uses a default depth of 6.
<*$namedLatticeUsage*>
* The following options are supported:
| VertexNaming | 'SpiralIndex' | how to name vertices |
| DirectedEdges | False | whether the graph should be directed |
| VertexLabels | None | whether to plot vertices with their labels |
| ImageSize | Automatic | size to plot the graph |
* To plot vertex labels, pass VertexLabels -> Automatic.
* To obtain cardinal tags on the edges, use the function LatticeQuiver.
* VertexNaming accepts the following settings:
| 'SpiralIndex' | number starting at 1 for the origin, spiralling first clockwise then outward (default) |
| 'RasterIndex' | number vertices starting at the top left, moving right then down |
| 'Coordinates' | name vertices based on their automatically extracted coordinates |
| 'Index' | number vertices using the order produced by GenerateLatticeQuiver |
| None | do not rename the vertices, meaning they will be LatticeVertex objects |
"

DeclareArgumentCount[LatticeGraph, {1, 2}];

Options[LatticeGraph] = JoinOptions[
  {DirectedEdges -> False},
  $baseGenerateLatticeOptions
];

declareFunctionAutocomplete[LatticeGraph, {$latticeNames, 0}];

LatticeGraph[name_, opts:OptionsPattern[]] := LatticeGraph[name, 6, opts];

(* this avoids an obscure bug that triggers when you do ?LatticeGraph, that I should report *)
SetAttributes[LatticeGraph, ReadProtected];

LatticeGraph[spec_, depth_, opts:OptionsPattern[]] := Scope[
  UnpackOptions[directedEdges];
  iGenerateLattice[LatticeGraph, spec, depth, directedEdges, FilterOptions @ opts]
];

(**************************************************************************************************)

PackageExport["LatticeQuiver"]

SetUsage @ "
LatticeQuiver[qrep$, d$] generates the lattice quivr graph for a quiver representation qrep$ to depth d$.
LatticeQuiver['name$', d$] generates the lattice quiver graph for the named lattice 'name$' to depth d$.
LatticeQuiver[spec$] uses a default depth of 6.
<*$namedLatticeUsage*>
* LatticeQuiver behaves like LatticeGraph, but returns a quiver instead of an ordinary graph.
"

DeclareArgumentCount[LatticeQuiver, {1, 2}];

Options[LatticeQuiver] = $baseGenerateLatticeOptions;

declareFunctionAutocomplete[LatticeQuiver, {$latticeNames, 0}];

LatticeQuiver[name_, opts:OptionsPattern[]] := LatticeQuiver[name, 6, opts];

LatticeQuiver[spec_, depth_, opts:OptionsPattern[]] :=
  iGenerateLattice[LatticeQuiver, spec, depth, True, opts];

