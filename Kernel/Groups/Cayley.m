PublicFunction[CayleyFunction]

SetUsage @ "
CayleyFunction[obj$] returns the function that takes an element of obj$ and \
returns a list of successors elements that represent the action of generators of obj$ on the element.
* rep can be a group, LinearRepresentationObject, QuiverRerpresentationObject, or RootSystem.
CayleyFunction takes the following options:
| 'Symmetric' | True | whether to include the action of the inverses of the generators |
| 'Labeled' | True | whether to yield successors that are Labeled with the name of the corresponding generator |
* For 'Symmetric' -> True and 'Labeled' -> True, the inverses successors are labeled with Inverted[gen$].
"

Options[CayleyFunction] = {
  "Symmetric" -> True,
  "Labeled" -> True
};

DeclareArgumentCount[CayleyFunction, 1];

CayleyFunction::badobj = "Input was not a PathRepresentationObject, Group, or RewritingSystemObject.";

CayleyFunction[object_, OptionsPattern[]] := Scope[
  UnpackOptions[labeled, symmetric];
  rep = Which[
    PathRepresentationObjectQ[object], object,
    RepresentationObjectQ[object], object,
    GroupQ[object], LinearGroupRepresentation[object],
    RewritingSystemObjectQ[object], object,
    True, ReturnFailed["badobj"]
  ];
  rep["CayleyFunction", "Labeled" -> symmetric, "Symmetric" -> symmetric]
];

(**************************************************************************************************)

PublicFunction[CayleyQuiver]

SetUsage @ "
CayleyQuiver[obj$] returns the cardinal quiver representing the Cayley graph of a LinearRepresentationObject or Group.
"

DeclareArgumentCount[CayleyQuiver, 1];

Options[CayleyQuiver] = JoinOptions[
  Cardinals -> Automatic,
  MaxDepth -> Automatic,
  MaxVertices -> Automatic,
  $ExtendedGraphOptions
];

CayleyQuiver[rep_, opts:OptionsPattern[]] := Scope[
  rep = CoerceToRep[1];
  rep["CayleyQuiver", opts] (* <- this just dispatches to representationCayleyQuiver *)
];

(**************************************************************************************************)

representationProperty[assoc_, "CayleyFunction", opts___Rule] :=
  computeCayleyFunction[assoc, opts];

Options[computeCayleyFunction] = {"Symmetric" -> True, "Labeled" -> True};
computeCayleyFunction[data_, OptionsPattern[]] := Scope[
  UnpackAssociation[data, generators];
  UnpackOptions[symmetric, labeled];
  list = Flatten @ KeyValueMap[
    {label, gen} |-> {
      If[labeled, Labeled[label], Id] @ gen,
      If[symmetric && (igen = ToInverseFunction[gen]) =!= gen && igen =!= None,
        If[labeled, Labeled[Inverted @ label], Id] @ igen,
        Nothing
      ]
    },
    generators
  ];
  ApplyThrough[list]
];

(**************************************************************************************************)

representationProperty[assoc_, "CayleyGraph" | "CayleyQuiver", opts___] :=
  computeCayleyQuiver[assoc, opts];

CayleyQuiver::incomplete = "Cayley graph is incomplete. Try specifying MaxDepth or MaxVertices options."
CayleyQuiver::cardlen = "Number of cardinals did not match number of generators."

computeCayleyQuiver[data_, opts:OptionsPattern[CayleyQuiver]] := Scope[
  UnpackOptions[cardinals];

  UnpackAssociation[data, generators];
  If[cardinals =!= Automatic,
    If[!SameLengthQ[cardinals, generators], ReturnFailed[CayleyQuiver::cardlen]];
    generators = AssociationThread[cardinals, Values @ generators]
  ];
  cfunc = computeCayleyFunction[<|"Generators" -> generators|>, "Labeled" -> True, "Symmetric" -> True];
  istate = List @ representationProperty[data, "Identity"];
  {vertices, indexEdges, reason} = MultiwaySystem[
    cfunc, istate, {"VertexList", "IndexEdgeList", "TerminationReason"},
    FilterOptions @ opts, MaxDepth -> 10, MaxVertices -> 200
  ];
  If[reason =!= "Complete" && !ContainsQ[{opts}, MaxDepth | MaxVertices],
    Message[CayleyQuiver::incomplete]];
  indexEdges //= DeleteDuplicates;
  FromIndexedEdges[vertices, indexEdges, FilterOptions @ opts, GraphTheme -> "CayleyQuiver"]
];

(**************************************************************************************************)

DefineGraphTheme["CayleyQuiver",
  EdgeLength -> 50,
  CollapseMultiedges -> True,
  VertexLayout -> SmartLayout[],
  EdgeColorFunction -> "Cardinal",
  ArrowheadShape -> "Line",
  VertexStyle -> GrayLevel[0.2],
  TwoWayStyle -> "Invisible"
];
