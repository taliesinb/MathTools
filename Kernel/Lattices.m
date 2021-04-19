Package["GraphTools`"]


PackageImport["GeneralUtilities`"]


PackageExport["VertexCoordinatizationFunction"]

SetUsage @ "
VertexCoordinatizationFunction is an option to GenerateQuiverLattice.
"

PackageExport["LatticeLayout"]

SetUsage @ "
LatticeLayout is an option to GenerateQuiverLattice.
"

PackageExport["GenerateQuiverLattice"]

Options[GenerateQuiverLattice] = Join[{
  VertexCoordinatizationFunction -> Automatic,
  LatticeLayout -> Automatic,
  ArrowheadSize -> Automatic,
  ArrowheadStyle -> Automatic,
  GraphLegend -> Automatic,
  MaxNorm -> Infinity,
  NormFunction -> ChessboardDistance
  },
  Options[StateTransitionGraph]
];

SetOptions[GenerateQuiverLattice, MaxDepth -> 3];

GenerateQuiverLattice::notquivrep =
  "First argument should be a QuiverRepresentationObject, or a quiver with canonically named cardinals.";

quiverStateToLatticeVertex[e_] := e /. QuiverElement[a_, b_] :> LatticeVertex[b, a];

GenerateQuiverLattice[quiverRepresentation_, opts:OptionsPattern[]] := Scope[

  If[RuleQ[quiverRepresentation],
    quiverRepresentation = QuiverRepresentation @@ quiverRepresentation;
    If[FailureQ[quiverRepresentation], ReturnFailed[]];
  ];

  If[QuiverQ[quiverRepresentation],
    quiverRepresentation = Quiet @ QuiverRepresentation[quiverRepresentation];
    If[FailureQ[quiverRepresentation], ReturnFailed["notquivrep"]];
  ];

  If[Head[quiverRepresentation] =!= QuiverRepresentationObject,
    ReturnFailed["notquivrep"]];

  function = quiverStateToLatticeVertex @ quiverRepresentation["CayleyFunction", "Symmetric" -> True, "Labeled" -> True];
  baseRep = quiverRepresentation["Representation"];
  istate = List @ quiverStateToLatticeVertex @ quiverRepresentation["Identity"];

  quiver = quiverRepresentation["Quiver"];

  UnpackOptions[
    maxDepth, maxNorm, normFunction,
    vertexCoordinatizationFunction, latticeLayout,
    arrowheadSize, arrowheadStyle, graphLegend,
    imageSize
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

  coordTypes = None; isRedundant = False;
  Switch[vertexCoordinatizationFunction,
    Automatic,
      {coordTypes, coordFunc, isRedundant} = findCoordinatizationFunction[First /@ baseRep["Generators"], baseRep["Group"]],
    None,
      coordFunc = Identity,
    _,
      coordFunc = Normal /* vertexCoordinatizationFunction
  ];

  If[latticeLayout === Automatic && FreeQ[coordTypes, None],
    {is3D, coordinateLayoutFunc} = chooseLatticeCoordinatization[coordTypes, isRedundant];
  ,
    is3D = False; coordinateLayoutFunc = None;
  ];
  zPrint["coordTypes", coordTypes];
  zPrint["coordFunc", coordFunc];
  zPrint["redundant?", isRedundant];

  (* do the exploration *)
  {vertexList, indexEdgeList, reason} = StateTransitionGraph[function, istate,
    {"VertexList", "IndexEdgeList", "TerminationReason"},
    MaxDepth -> maxDepth, IncludeFrontier -> False,
    FilterOptions[StateTransitionGraph, DeleteCases[{opts}, (MaxNorm | NormFunction -> _)]],
    DirectedEdges -> True
  ];

  (* rewrite the vertices via the coordinate function *)
  vertexList = MapAt[coordFunc, vertexList, {All, 1}];
  zPrint[vertexList];

  (* rewrite the indexed edges to use the explicit vertices *)
  edges = DeleteDuplicates[indexEdgeList] /.
    DirectedEdge[i_, j_, c_] :> DirectedEdge[Part[vertexList, i], Part[vertexList, j], c];

  (* apply the layout, if any *)
  If[coordinateLayoutFunc =!= None,
    vertexCoordinates = Map[vertex |-> vertex -> coordinateLayoutFunc[First[vertex]], vertexList];
    graphLayout = If[is3D, {"Dimension" -> 3}, Automatic];
  ,
    vertexCoordinates = None;
    graphLayout = {"VertexLayout" -> "SpringElectricalEmbedding", "Dimension" -> If[is3D, 3, 2]};
  ];

  Which[
    latticeLayout === "2D",
      graphLayout = {"VertexLayout" -> "SpringElectricalEmbedding", "Dimension" -> 2},
    latticeLayout === "3D",
      graphLayout = {"VertexLayout" -> "SpringElectricalEmbedding", "Dimension" -> 3},
    True,
      Null
  ];

  latticeQuiver = Quiver[edges,
    FilterOptions[Graph, DeleteOptions[{opts}, GraphLegend]],
    VertexLabels -> Placed["Name", Tooltip],
    GraphLayout -> graphLayout, VertexCoordinates -> vertexCoordinates,
    ArrowheadSize -> arrowheadSize, ArrowheadStyle -> arrowheadStyle,
    GraphLegend -> graphLegend
  ];

  If[maxNorm =!= Infinity,
    initialCoord = vertexList[[1, 1]];
    latticeQuiver = Subgraph[latticeQuiver, LatticeVertex[coords_ /; normFunction[coords, initialCoord] <= maxNorm, _]];
  ];

  latticeQuiver
];

(**************************************************************************************************)

$latticeQuiverRepresentations = <||>;

(**************************************************************************************************)

$squareQRep := $squareQRep =
  QuiverRepresentation[BouquetQuiver["xy"], "Abelian"];

$latticeQuiverRepresentations["Square"] := $squareQRep;

(**************************************************************************************************)

$cubicQRep := $squareQRep =
  QuiverRepresentation[BouquetQuiver["xyz"], "Abelian"];

$latticeQuiverRepresentations["Cubic"] := $cubicQRep;

(**************************************************************************************************)


$triangularQRep := $triangularQRep =
  QuiverRepresentation[BouquetQuiver["abc"], "Redundant"];

$latticeQuiverRepresentations["Triangular"] := $triangularQRep;

(**************************************************************************************************)


$hexagonalQRep := $hexagonalQRep =
  QuiverRepresentation[
    Quiver[{DirectedEdge[1, 2, "a" | "b" | "c"]}],
    "Redundant"
  ];

$latticeQuiverRepresentations["Hexagonal"] := $hexagonalQRep;

(**************************************************************************************************)

$rhombilleQRep := $rhombilleQRep =
  QuiverRepresentation[
    Quiver[{DirectedEdge[1, 2, "a" | "b" | "c"], DirectedEdge[2, 3, "a" | "b" | "c"]}],
    "Redundant"
  ];

$latticeQuiverRepresentations["Rhombille"] := $rhombilleQRep;

(**************************************************************************************************)

vecAngle2[{x_, y_}] := ArcTan[-y + $MachineEpsilon, -x - $MachineEpsilon];
sortAngle[vecs_] := SortBy[vecs, vecAngle2];
Rotate90[{a_, b_}] := {b, -a};

$rhombiHexQuiver := $rhombiHexQuiver = Quiver[{
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

$rhombihexQRep := $rhombihexQRep = QuiverRepresentation[$rhombiHexQuiver, $tgroup12];

$latticeQuiverRepresentations["Rhombihexadeltille"] := $rhombihexQRep;

(**************************************************************************************************)

$snubhexQuiver := $snubhexQuiver = Quiver[{
  Labeled[{{3 -> 2, 5 -> 6}, {1 -> 5, 2 -> 4, 6 -> 3}}, "a"],
  Labeled[{{6 -> 1, 4 -> 3}, {1 -> 4, 2 -> 6, 3 -> 5}}, Negated @ "b"],
  Labeled[{{1 -> 2, 5 -> 4}, {2 -> 5, 3 -> 1, 4 -> 6}}, "c"]
}];

$snubhexQRep := $snubhexQRep = QuiverRepresentation[$snubhexQuiver, "cab" -> "Redundant"];

$latticeQuiverRepresentations["SnubHextille"] := $snubhexQRep;

(**************************************************************************************************)

PackageExport["VertexNaming"]

SetUsage @ "
VertexNaming is an option to LatticeGraph.
"

PackageExport["LatticeGraph"]

SetUsage @ "
LatticeGraph['name$', d$] generates the lattice graph for the named lattice 'name$' to depth d$.
LatticeGraph['name$'] uses a default depth of 6.
* Named lattices (and their corresponding tilings) include:
| 'Square' | square tiling, aka quadrille |
| 'Cubic' | cubic tiling |
| 'Triangular' | triangular tiling, aka deltille |
| 'Hexagonal' | hexagonal lattice, aka hextille |
| 'SnubHextille' | snub hextille tiling, aka snub trihexagonal |
| 'Rhombille' | rhombille lattice |
| 'Rhombihexadeltille' | rhombihexadeltille tiling, aka rhombitrihexagonal |
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
| 'Index' | number vertices using the order produced by GenerateQuiverLattice |
| None | do not rename the vertices, meaning they will be LatticeVertex objects |
"

Options[LatticeGraph] = JoinOptions[{
  ImageSize -> Automatic,
  VertexNaming -> "SpiralIndex",
  DirectedEdges -> False
}, $simpleGraphOptionRules];

LatticeGraph::badlatticename = "The specified name `` is not a known name for a lattice graph. Known names are: ``."
LatticeGraph::baddepth = "The specified depth `` is not a positive integer."
LatticeGraph::badvertnaming = "Unknown setting `` for VertexNaming.";

$latticeNames = Sort @ Keys[$latticeQuiverRepresentations];
declareFunctionAutocomplete[LatticeGraph, {$latticeNames, 0}];

LatticeGraph[name_, opts:OptionsPattern[]] := LatticeGraph[name, 6, opts];

(* this avoids an obscure bug that triggers when you do ?LatticeGraph, that I should report *)
SetAttributes[LatticeGraph, ReadProtected];

LatticeGraph[name_, depth_, opts:OptionsPattern[]] := Scope[

  qrep = Lookup[$latticeQuiverRepresentations, name, $Failed];
  If[FailureQ[qrep], ReturnFailed["badlatticename", name, commaString @ $latticeNames]];

  If[!IntegerQ[depth] || !Positive[depth], ReturnFailed["baddepth", depth]];

  UnpackOptions[vertexNaming, directedEdges, imageSize];
  graph = GenerateQuiverLattice[qrep, MaxDepth -> depth, ImageSize -> imageSize];

  vertexCoordinates = LookupOption[graph, VertexCoordinates];
  {vertices, edges} = VertexEdgeList[graph];

  renamingRule = toRenamingRule[vertexNaming, vertices];
  If[FailureQ[renamingRule], ReturnFailed[LatticeGraph::badvertnaming, vertexNaming]];

  {vertices, edges} = {vertices, edges} /. renamingRule;
  If[FalseQ @ directedEdges, edges = UndirectedEdge @@@ edges];

  is3D = Length[First @ vertexCoordinates] === 3;

  edgeStyle = If[directedEdges,
    {Opacity[0.8], GrayLevel[0.8], Arrowheads[{{Medium, 0.7}}]},
    {Opacity[0.5], GrayLevel[0.6]}
  ];

  Graph[vertices, edges,
    Sequence @@ DeleteOptions[TakeOptions[{opts}, $simpleGraphOptions], ImageSize],
    VertexCoordinates -> vertexCoordinates,
    VertexStyle -> Directive[Opacity[1], EdgeForm[None], GrayLevel[If[is3D, 0, 0.5]]],
    EdgeStyle -> Apply[Directive, edgeStyle],
    Sequence @@ If[is3D, {VertexShapeFunction -> "Point", EdgeShapeFunction -> "Line"}, {}],
    ImageSize -> LookupOption[graph, ImageSize]
  ]
];

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
  AssociationThread[vertices, Ordering @ Ordering @ ({-#2, #1}& @@@ vertexCoordinates)];

toRenamingRule["Coordinates", _] :=
  LatticeVertex[v_, _] :> v;

toRenamingRule["DefaultIndex", _] :=
  AssociationRange[vertices];

toRenamingRule[None, _] := {};

toRenamingRule[_, _] := $Failed;
