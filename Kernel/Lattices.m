Package["GraphTools`"]


PackageImport["GeneralUtilities`"]


PackageExport["$LatticeNames"]


PackageExport["AbstractCoordinateFunction"]
PackageExport["VertexCoordinateFunction"]
PackageExport["VertexNameFunction"]

SetUsage @ "AbstractCoordinateFunction is an option to QuiverLattice and QuiverGraph."
SetUsage @ "VertexCoordinateFunction is an option to QuiverLattice and QuiverGraph."
SetUsage @ "VertexNameFunction is an option to QuiverLattice and QuiverGraph."

$baseLatticeUsage = "
* The following options are supported:
| AbstractCoordinateFunction | Automatic | function to obtain abstract vertex coordinates from representation matrices |
| VertexCoordinateFunction | Automatic | function to obtain layout coordinates from vertex coordinates |
| VertexNameFunction | 'SpiralIndex' | function to rename vertices after coordinatization |
| GraphLayout | None | the overall layout method to use for vertices and edges |
| VertexLabels | None | whether to plot vertices with their labels |
| ImageSize | Automatic | size to plot the graph |
| MaxNorm | Infinity | drop vertices with larger norm |
| NormFunction | Automatic | function to compute norm from abstract vertex coordinates |
| GraphRegionHighlight | None | regions of the graph to highlight |
| GraphLegend | Automatic | legend to attach to the entire graph |

* AbstractCoordinateFunction extracts abstract vertex coordinates from RepresentationElements, and accepts these settings:
| Automatic | pick coordinates based on the structure of the group (default) |
| None | use the entire RepresentationElement as the coordinate |
| f$ | apply f$ to the contents of each RepresentationElement |

* VertexCoordinateFunction determines the graphical coordinates of each vertex, and accepts the following settings:
| Automatic | convert representation coords to spatial coords based on the structure of the group (default) |
| None | use automatic layout of the vertices |
| f$ | apply f$ to the representation coordinates produced by AbstractCoordinateFunction |

* VertexNameFunction determines the final names given to vertices, and accepts these settings:
| 'SpiralIndex' | number vertices starting at 1 for the origin, proceeding clockwise then outward (default) |
| 'RasterIndex' | number vertices starting at the top left, proceeding right then down |
| 'LayoutCoordinates' | use layout coordinates as names |
| 'VertexCoordinates' | use abstract vertex coordinates as names |
| 'Representation' | use the original RepresentationElement[$$] objects as names |
* For non-number namings, the resulting vertices will be of the form LatticeVertex[coord$, type$], where type$ \
is the name of the corresponding vertex of the quiver.

* GraphLayout accepts these settings:
| None | use the layout provided by VertexCoordinateFunction |
| Automatic | use 'SpringElectricalEmbedding' |
| '2D' | use a two-dimensional automatic layout |
| '3D' | use a three-dimensional automatic layout |
| spec$ | use a custom specification accepted by Graph |

* VertexLabels determines how to label vertices, and accepts these settings:
| None | do not label vertices |
| Automatic | label vertices with their names |
| 'VertexCoordinates' | label vertices with their abstract vertex coordinates |
| Tooltip | label vertices with their names via a tooltip |

* EdgeLabels determines how to label edges, and accepts these settings:
| None | do not label edges |
| Automatic | label edges with their cardinals |

* GraphLegend accepts these settings:
| None | no legend |
| Automatic | use a legend indicating the cardinals |
| expr$ | use a custom legend given by expr$ |
"

$baseGenerateLatticeOptions = JoinOptions[{
  AbstractCoordinateFunction -> Automatic,
  VertexCoordinateFunction -> Automatic,
  VertexNameFunction -> "SpiralIndex",
  GraphLegend -> Automatic,
  MaxNorm -> Infinity,
  NormFunction -> Automatic,
  CardinalColors -> Automatic},
  $simpleGraphOptionRules
];

General::notquivrep = "First argument should be a QuiverRepresentationObject, or a quiver with canonically named cardinals.";
General::badvcoords = "VertexCoordinateFunction did not return vectors of consistent dimensions.";
General::badlatticename = "The specified name `` is not a known name for a lattice. Known names are: ``."
General::badparamlatticename = "The specified name `` is not a known name for a parameterized lattice. Known names are: ``."
General::badlatticedepth = "The specified depth `` is not a positive integer."
General::badvertnaming = "Unknown setting `` for VertexNameFunction.";
General::renamenotquiv = "The vertex coordinates yielded a quiver with incompatible cardinal edges. Use LatticeGraph instead.";

Options[iGenerateLattice] = $baseGenerateLatticeOptions;

iGenerateLattice[head_, quiverRepresentation_, maxDepth_, directedEdges_, opts:OptionsPattern[]] := Scope[

  defaultDepth = 6;
  depth = maxDepth;

  If[StringQ[quiverRepresentation],
    quiverRepresentation = Lookup[$latticeQuiverRepresentations, quiverRepresentation, $Failed];
    If[FailureQ[qrep], ReturnFailed[head::badlatticename, name, commaString @ $LatticeNames]];
  ];

  If[MatchQ[quiverRepresentation, {_String, __}],
    quiverRepFunction = Lookup[$parameterizedLatticeQuiverRepresentations, First @ quiverRepresentation, $Failed];
    If[FailureQ[quiverRepFunction], ReturnFailed[head::badlatticename, name, commaString @ $parameterizedLatticeNames]];
    result = quiverRepFunction @@ Rest[quiverRepresentation];
    If[FailureQ[result], ReturnFailed[]];
    {defaultDepth, quiverRepresentation} = result;
  ];

  SetAutomatic[depth, defaultDepth];
  If[!IntegerQ[depth] || !Positive[depth], ReturnFailed[head::badlatticedepth, depth]];

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
    abstractCoordinateFunction, vertexCoordinateFunction,
    graphLayout,
    graphLegend, imageSize, vertexNameFunction, arrowheadStyle
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
  Switch[abstractCoordinateFunction,
    Automatic,
      {coordTypes, abstractCoordinateFunction, isRedundant} =
        findCoordinatizationFunction[First /@ baseRep["Generators"], baseRep["Group"]];
      If[graphLayout === Automatic && FreeQ[coordTypes, None],
        {is3D, proposedVertexCoordinateFunction} = chooseLatticeCoordinatization[coordTypes, isRedundant];
        SetAutomatic[vertexCoordinateFunction, proposedVertexCoordinateFunction];
      ];
      layoutDimension = If[is3D, 3, 2],
    None,
      abstractCoordinateFunction = Identity,
    _,
      abstractCoordinateFunction = Normal /* abstractCoordinateFunction;
      lattice
  ];

  (* do the exploration *)
  {vertexList, indexEdgeList, reason} = StateTransitionGraph[function, istate,
    {"VertexList", "IndexEdgeList", "TerminationReason"},
    MaxDepth -> depth, IncludeFrontier -> False, DirectedEdges -> True
  ];

  (* rewrite the vertices via the coordinate function *)
  abstractVertexList = MapAt[abstractCoordinateFunction, vertexList, {All, 1}];

  (* rewrite the indexed edges to use the explicit vertices *)
  edgeList = DeleteDuplicates[indexEdgeList] /.
    DirectedEdge[i_, j_, c_] :> DirectedEdge[Part[abstractVertexList, i], Part[abstractVertexList, j], c];

  If[directedEdges === False, edgeList = UndirectedEdge[#1, #2]& @@@ edgeList];

  (* the coordinitization might have collapsed some vertices *)
  abstractVertexList //= DeleteDuplicates;

  (* remove any vertices that exceed the norm *)
  If[maxNorm =!= Infinity,
    SetAutomatic[normFunction, ChessboardNorm];
    initialCoord = MapAt[coordFunc, vertexList, 1];
    {abstractVertexList, edgeList} = VertexEdgeList @ Subgraph[
      Graph[abstractVertexList, edgeList],
      LatticeVertex[coords_ /; normFunction[coords, initialCoord] <= maxNorm, _]
    ];
  ];

  (* apply the final layout, if any *)
  SetAutomatic[vertexCoordinateFunction, None];
  If[vertexCoordinateFunction =!= None,
    vertexCoordinates = Map[First /* vertexCoordinateFunction, abstractVertexList];
    If[!MatrixQ[vertexCoordinates], ReturnFailed[head::badvcoords]];
    layoutDimension = Switch[Length @ First @ vertexCoordinates, 2, 2, 3, 3, _, Automatic];
  ,
    vertexCoordinates = Automatic;
    Switch[graphLayout,
      "2D", vertexLayout = "SpringElectricalEmbedding"; layoutDimension = 2,
      "3D", vertexLayout = "SpringElectricalEmbedding"; layoutDimension = 3,
      Automatic, vertexLayout = "SpringElectricalEmbedding",
      _String, vertexLayout = graphLayout,
      _, ReturnFailed[];
    ];
  ];

  graphLayout = {"VertexLayout" -> vertexLayout, "Dimension" -> layoutDimension};

  If[head === LatticeGraph && layoutDimension === Automatic,
    (* for non-quiver graphs, we need to decide if the graph is 3D before we construct it,
    since it will change what shape function we use etc. *)
    SetAutomatic[vertexCoordinates, GraphEmbedding[Graph[abstractVertexList, edgeList], graphLayout]];
    graphLayout[[2, 2]] = layoutDimension = Last @ Dimensions @ vertexCoordinates;
  ];

  (* apply the final vertex and edge relabeling *)
  renamingRule = toRenamingRule[vertexNameFunction, abstractVertexList];
  If[FailureQ[renamingRule], ReturnFailed[head::badvertnaming, vertexNameFunction]];
  {finalVertexList, edgeList} = {abstractVertexList, edgeList} /. renamingRule;
  If[VectorQ[finalVertexList, IntegerQ] && MinMax[finalVertexList] == {1, Length @ vertices},
    finalVertexList = Developer`ToPackedArray @ Sort @ finalVertexList;
    If[ListQ[vertexCoordinates], vertexCoordinates = Part[vertexCoordinates, Ordering @ vertices]];
    edgeList //= Sort;
  ];

  simpleOptions = Sequence @@ TakeOptions[{opts}, $simpleGraphOptions];
  If[head === LatticeGraph,
    graph = Graph[
      finalVertexList, edgeList,
      GraphLegend -> graphLegend, GraphPlottingFunction -> Automatic,
      ImageSize -> imageSize,
      GraphLayout -> graphLayout, VertexCoordinates -> vertexCoordinates,
      simpleOptions
    ]
  ,
    imageSize //= toStandardImageSize;
    graph = Quiver[finalVertexList, edgeList,
      GraphLayout -> graphLayout, VertexCoordinates -> vertexCoordinates,
      GraphLegend -> graphLegend, ImageSize -> imageSize,
      simpleOptions
    ];
    If[FailureQ[graph], ReturnFailed["renamenotquiv"]];
  ];

  AttachVertexAnnotations[graph, <|
    "GeneratingVertex" -> vertexList[[All, 2]],
    "AbstractCoordinates" -> abstractVertexList[[All, 1]]
  |>]
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
$LatticeNames = {};

SetHoldRest[declareLattice];
declareLattice[names_List, rep_] := (
  AppendTo[$LatticeNames, First @ names];
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

$parameterizedLatticeQuiverRepresentations = <||>;
$parameterizedLatticeNames = {};

declareParameterizedLattice[name_String, func_] := (
  AppendTo[$parameterizedLatticeNames, name];
  $parameterizedLatticeQuiverRepresentations[name] = func;
);

declareParameterizedLattice["SquareTorus", squareTorusQRep];

squareTorusQRep[m_Integer ? Positive, n_Integer ? Positive] := Scope[
  qrep = QuiverRepresentation[
    $squareQuiver,
    AbelianGroup[{m, n}]
  ];
  {m + n, qrep}
];

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

declareFunctionAutocomplete[LatticeQuiverRepresentation, {$LatticeNames}];

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
* To obtain cardinal tags on the edges, use the function LatticeQuiver.
"

DeclareArgumentCount[LatticeGraph, {1, 2}];

Options[LatticeGraph] = JoinOptions[
  {DirectedEdges -> False},
  $baseGenerateLatticeOptions
];

declareFunctionAutocomplete[LatticeGraph, {$LatticeNames, 0}];

declareSyntaxInfo[LatticeGraph, {_, _., OptionsPattern[]}];

LatticeGraph[name_, opts:OptionsPattern[]] :=
  LatticeGraph[name, Automatic, opts];

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

declareFunctionAutocomplete[LatticeQuiver, {$LatticeNames, 0}];

declareSyntaxInfo[LatticeQuiver, {_, _., OptionsPattern[]}];

LatticeQuiver[name_, opts:OptionsPattern[]] :=
  LatticeQuiver[name, Automatic, opts];

LatticeQuiver[spec_, depth_, opts:OptionsPattern[]] :=
  iGenerateLattice[LatticeQuiver, spec, depth, True, opts];

