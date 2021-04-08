Package["GraphTools`"]


PackageImport["GeneralUtilities`"]

PackageExport["VertexCoordinatizationFunction"]

SetUsage @ "
VertexCoordinatizationFunction is an option to GenerateQuiverLattice.
"

PackageExport["GenerateQuiverLattice"]

Options[GenerateQuiverLattice] = Join[{
  VertexCoordinatizationFunction -> Automatic,
  LatticeLayout -> Automatic
  },
  Options[StateTransitionGraph]
];

SetOptions[GenerateQuiverLattice, MaxDepth -> 3];

GenerateQuiverLattice::notquivrep =
  "First argument should be a CardinalQuiverRepresentationObject, or a quiver with canonically named cardinals.";

GenerateQuiverLattice[quiverRepresentation_, opts:OptionsPattern[]] := Scope[
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
  UnpackOptions[maxDepth, vertexCoordinatizationFunction, latticeLayout];
  coordTypes = None;
  Switch[vertexCoordinatizationFunction,
    Automatic,
      {coordTypes, coordFunc} = findCoordinatizationFunction[First /@ baseRep["Generators"]],
    None,
      coordFunc = Identity,
    _,
      coordFunc = Normal /* vertexCoordinatizationFunction
  ];

  If[latticeLayout === Automatic && FreeQ[coordTypes, None],
    {is3D, coordinateLayoutFunc} = chooseLatticeCoordinatization[coordTypes];
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

  CardinalQuiver[edges, FilterOptions[Graph, opts],
    VertexLabels -> Placed["Name", Tooltip],
    GraphLayout -> graphLayout, VertexCoordinates -> vertexCoordinates
  ]
];

