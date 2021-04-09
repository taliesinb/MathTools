Package["GraphTools`"]


PackageImport["GeneralUtilities`"]

PackageExport["VertexCoordinatizationFunction"]
PackageExport["LatticeLayout"]

SetUsage @ "
VertexCoordinatizationFunction is an option to GenerateQuiverLattice.
"

SetUsage @ "
LatticeLayout is an option to GenerateQuiverLattice.
"

PackageExport["GenerateQuiverLattice"]

Options[GenerateQuiverLattice] = Join[{
  VertexCoordinatizationFunction -> Automatic,
  LatticeLayout -> Automatic,
  ArrowheadSize -> Automatic,
  GraphLegend -> Automatic
  },
  Options[StateTransitionGraph]
];

SetOptions[GenerateQuiverLattice, MaxDepth -> 3];

GenerateQuiverLattice::notquivrep =
  "First argument should be a CardinalQuiverRepresentationObject, or a quiver with canonically named cardinals.";

GenerateQuiverLattice[quiverRepresentation_, opts:OptionsPattern[]] := Scope[

  If[RuleQ[quiverRepresentation],
    quiverRepresentation = CardinalQuiverRepresentation @@ quiverRepresentation;
    If[FailureQ[quiverRepresentation], ReturnFailed[]];
  ];

  If[CardinalQuiverQ[quiverRepresentation],
    quiverRepresentation = Quiet @ CardinalQuiverRepresentation[quiverRepresentation];
    If[FailureQ[quiverRepresentation], ReturnFailed["notquivrep"]];
  ];

  If[Head[quiverRepresentation] =!= CardinalQuiverRepresentationObject,
    ReturnFailed["notquivrep"]];

  function = quiverRepresentation["SymmetricCayleyFunction"];
  baseRep = quiverRepresentation["Representation"];
  id = baseRep["Identity"];
  quiver = quiverRepresentation["Quiver"];
  firstVertex = First @ VertexList[quiver];
  istate = {LatticeVertex[id, firstVertex]};
  (* If[NumberQ[maxNorm], function = function /* Select[LatticeVertexNorm[#, normFunction] <= maxNorm&]]; *)

  UnpackOptions[maxDepth, vertexCoordinatizationFunction, latticeLayout, arrowheadSize, graphLegend];

  $quiverLabel := CardinalQuiver[quiver, ImageSize -> Tiny, GraphLegend -> Placed[Automatic, Left]];

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
      {coordTypes, coordFunc, isRedundant} = findCoordinatizationFunction[First /@ baseRep["Generators"]],
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

  (* do the exploration *)
  {vertexList, indexEdgeList, reason} = StateTransitionGraph[function, istate,
    {"VertexList", "IndexEdgeList", "TerminationReason"},
    MaxDepth -> maxDepth, IncludeFrontier -> False,
    FilterOptions[StateTransitionGraph, opts], DirectedEdges -> True
  ];

  indexEdgeList = indexEdgeList /. DirectedEdge[a_, b_, Negated[c_]] :> DirectedEdge[b, a, c];

  (* rewrite the vertices via the coordinate function *)
  vertexList = MapAt[coordFunc, vertexList, {All, 1}];

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

  CardinalQuiver[edges, FilterOptions[Graph, DeleteCases[{opts}, GraphLegend -> _]],
    VertexLabels -> Placed["Name", Tooltip],
    GraphLayout -> graphLayout, VertexCoordinates -> vertexCoordinates,
    ArrowheadSize -> arrowheadSize,
    GraphLegend -> graphLegend
  ]
];


PackageExport["QuiverLatticePlot"]

QuiverLatticePlot[quiver_ -> rep_, args___] := Scope[
  rep = CardinalQuiverRepresentation[quiver, rep];
  If[FailureQ[rep], ReturnFailed[]];
  GeneratorAndShowLattice[rep, args]
];

QuiverLatticePlot[rep_, args___] := Scope[
  quiver = If[CardinalQuiverRepresentationObjectQ[rep], rep["Quiver"], rep];
  lattice = GenerateQuiverLattice[rep, args, ImageSize -> Medium, GraphLegend -> None, MaxDepth -> 6];
  Row[{quiver, "  ", lattice}]
];
