Package["GraphTools`"]


PackageImport["GeneralUtilities`"]


PackageExport["AbstractCoordinateFunction"]
PackageExport["VertexCoordinateFunction"]
PackageExport["VertexNameFunction"]

SetUsage @ "AbstractCoordinateFunction is an option to QuiverLattice and QuiverGraph."
SetUsage @ "VertexCoordinateFunction is an option to QuiverLattice and QuiverGraph."
SetUsage @ "VertexNameFunction is an option to QuiverLattice and QuiverGraph."

$baseLatticeUsage = "
* The following options are supported:
| AbstractCoordinateFunction | Automatic | function to obtain abstract vertex coordinates from representation matrices |
| VertexCoordinateFunction | Automatic | function to obtain graphical vertex coordinates from abstract coordinates |
| VertexNameFunction | 'SpiralIndex' | function to rename vertices after abstract coordinatization |
| GraphLayout | None | the overall layout method to use for vertices and edges |
| LayoutDimension | Automatic | number of dimensions of the graph layout |
| VertexLabels | None | whether to plot vertices with their labels |
| ImageSize | Automatic | size to plot the graph |
| MaxVertices | Infinity | maximum number of lattice vertices to obtain |
| MaxEdges | Infinity | maximum number of lattice edges to obtain |
| MaxNorm | Infinity | drop vertices with larger norm |
| NormFunction | Automatic | function to compute norm from abstract vertex coordinates |
| GraphRegionHighlight | None | regions of the graph to highlight |
| GraphLegend | Automatic | legend to attach to the entire graph |

* AbstractCoordinateFunction extracts abstract vertex coordinates from RepresentationElements, and accepts these settings:
| Automatic | pick coordinates based on the structure of the group (default) |
| None | use the entire RepresentationElement as the coordinate |
| f$ | apply f$ to the contents of each RepresentationElement (a matrix) |

* VertexCoordinateFunction determines the graphical coordinates, and accepts the following settings:
| Automatic | convert representation coords to spatial coords based on the structure of the group (default) |
| None | use Graph's automatic layout of the vertices |
| f$ | apply f$ to the representation coordinates produced by AbstractCoordinateFunction |

* VertexNameFunction determines the final names given to vertices, and accepts these settings:
| 'SpiralIndex' | number vertices starting at 1 for the origin, proceeding clockwise then outward (default) |
| 'RasterIndex' | number vertices starting at the top left, proceeding right then down |
| 'Coordinates' | use abstract coordinates directly as names |
| 'Representation' | use the original RepresentationElement[$$] objects directly as names |
| None | use LatticeVertex[abstract$, type$] as names |

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
  CardinalColors -> Automatic,
  MaxVertices -> Infinity, MaxEdges -> Infinity,
  DepthTermination -> Automatic, IncludeFrontier -> Automatic},
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

  defaultDepth = Infinity;
  depth = maxDepth;

  If[StringQ[quiverRepresentation],
    quiverRepresentation = LatticeQuiverData[quiverRepresentation, "Representation"];
    If[QuiverRepresentationObjectQ[qrep], ReturnFailed[head::badlatticename, name, commaString @ $LatticeQuiverNames]];
  ];

  If[MatchQ[quiverRepresentation, {_String, __}],
    quiverRepFunction = Lookup[$parameterizedLatticeQuiverRepresentations, First @ quiverRepresentation, $Failed];
    If[FailureQ[quiverRepFunction], ReturnFailed[head::badlatticename, name, commaString @ $parameterizedLatticeNames]];
    result = quiverRepFunction @@ Rest[quiverRepresentation];
    If[FailureQ[result], ReturnFailed[]];
    {defaultDepth, quiverRepresentation} = result;
  ];

  If[RuleQ[quiverRepresentation],
    quiverRepresentation = QuiverRepresentation @@ quiverRepresentation;
    If[FailureQ[quiverRepresentation], ReturnFailed[]];
  ];

  If[QuiverQ[quiverRepresentation],
    quiverRepresentation = Quiet @ QuiverRepresentation[quiverRepresentation]];

  If[!QuiverRepresentationObjectQ[quiverRepresentation],
    ReturnFailed[head::notquivrep]];

  function = quiverStateToLatticeVertex @ quiverRepresentation["CayleyFunction", "Symmetric" -> True, "Labeled" -> True];
  baseRep = quiverRepresentation["Representation"];
  istate = List @ quiverStateToLatticeVertex @ quiverRepresentation["Identity"];

  quiver = quiverRepresentation["Quiver"];

  UnpackOptionsAs[head, opts,
    maxNorm, normFunction,
    abstractCoordinateFunction, vertexCoordinateFunction,
    graphLayout,
    graphLegend, imageSize, vertexNameFunction, arrowheadStyle,
    maxVertices, maxEdges, depthTermination, includeFrontier
  ];

  SetAutomatic[depth, If[maxVertices === Infinity, maxVertices = AtLeast[32]]; Infinity];

  If[MatchQ[maxVertices, AtLeast[_Integer]],
    maxVertices //= First;
    SetAutomatic[includeFrontier, False];
    SetAutomatic[depthTermination, "Complete"];
  ];

  SetAutomatic[depthTermination, "Immediate"];

  If[!MatchQ[depth, (_Integer ? Positive) | Infinity],
    ReturnFailed[head::badlatticedepth, depth];
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
    DirectedEdges -> True,
    MaxDepth -> depth,
    IncludeFrontier -> includeFrontier, DepthTermination -> depthTermination,
    MaxVertices -> maxVertices, MaxEdges -> maxEdges
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
    vertexLayout = "SpringElectricalEmbedding";
  ];

  (* apply the final vertex and edge relabeling *)
  renamingRule = toRenamingRule[vertexNameFunction, abstractVertexList, vertexList];
  If[FailureQ[renamingRule], ReturnFailed[head::badvertnaming, vertexNameFunction]];
  {finalVertexList, edgeList} = {abstractVertexList, edgeList} /. renamingRule;
  If[RangeQ[finalVertexList],
    (* if we renamed to integers 1..n, reorder to make sure they occur in the natural order *)
    ordering = Ordering @ finalVertexList;
    finalVertexList = Developer`ToPackedArray @ Part[finalVertexList, ordering];
    vertexList = Part[vertexList, ordering];
    abstractVertexList = Part[abstractVertexList, ordering];
    If[ListQ[vertexCoordinates], vertexCoordinates = Part[vertexCoordinates, ordering]];
    edgeList //= Sort;
  ];

  simpleOptions = Sequence @@ TakeOptions[{opts}, $simpleGraphOptions];
  If[head === LatticeGraph,
    graph = Graph[
      finalVertexList, edgeList,
      GraphLegend -> graphLegend, GraphPlottingFunction -> Automatic,
      ImageSize -> imageSize, ArrowheadStyle -> "Plain",
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

toRenamingRule["SpiralIndex", vertices_, origVertices_] :=
  AssociationThread[vertices, Ordering @ Ordering @ Map[vecSorter, N @ vertices[[All, 1]]]];

toRenamingRule["RasterIndex", vertices_, origVertices_] :=
  AssociationThread[vertices, Ordering @ Ordering @ ({-#2, #1}& @@@ N[vertexCoordinates])];

toRenamingRule["Representation", _, _] :=
  AssociationThread[vertices,  origVertices[[All, 1]]];

toRenamingRule["Coordinates", _, _] :=
  LatticeVertex[v_, _] :> v;

toRenamingRule["Index", _, _] :=
  AssociationRange[vertices];

toRenamingRule[None, _] := {};

toRenamingRule[_, _] := $Failed;

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

declareFunctionAutocomplete[LatticeGraph, {$LatticeQuiverNames, 0}];

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

declareFunctionAutocomplete[LatticeQuiver, {$LatticeQuiverNames, 0}];

declareSyntaxInfo[LatticeQuiver, {_, _., OptionsPattern[]}];

LatticeQuiver[name_, opts:OptionsPattern[]] :=
  LatticeQuiver[name, Automatic, opts];

LatticeQuiver[spec_, depth_, opts:OptionsPattern[]] :=
  iGenerateLattice[LatticeQuiver, spec, depth, True, opts];
