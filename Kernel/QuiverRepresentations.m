Package["GraphTools`"]


PackageImport["GeneralUtilities`"]


PackageExport["CardinalQuiverRepresentation"]

SetUsage @ "
CardinalQuiverRepresentation[quiver$, cardinals$ -> representation$] attachs a representation to a quiver, returning \
a CardinalQuiverRepresentationObject.
* The list cardinals$ is matched with the generators of representation$ in order.
* The list cardinals$ can also be given as a single string whose letters are the cardinals.
* If no cardinals are provided, the cardinals present in quiver$ will be used, in sorted order.
CardinalQuiverRepresentation[quiver$] chooses a representation based on the names of the cardinals in quiver$.
* For cardinals {'x', 'y'} or {'x', 'y', 'z'}, a representation of InfiniteAbelianGroup is used.
* For cardinals {'a', 'b', 'c'}, RedundantAbelianRepresentation[3] is used.
"

CardinalQuiverRepresentation::notquiver =
  "The first argument to CardinalQuiverRepresentation should be a cardinal quiver Graph."

CardinalQuiverRepresentation::notrep =
  "The second argument to CardinalQuiverRepresentation should be a RepresentationObject or one of the forms documented in ?CardinalQuiverRepresentation."

CardinalQuiverRepresentation::noautorep =
  "No automatic representation is defined for the cardinal set ``.";

CardinalQuiverRepresentation::gencount =
  "The number of generators in the representation (``) did not match the number of cardinals in the graph (``).";

parseRepresentationSpec = MatchValues[
  Automatic         := {Automatic, Automatic};
  "Abelian"         := {Automatic, "Abelian"};
  "Redundant"       := {Automatic, "Redundant"};
  s_String          := {Characters[s], Automatic};
  s_String -> rep_  := {Characters[s], rep};
  list_List -> rep_ := {list, rep};
  rep_              := {Automatic, rep}
];

chooseAutoRepresentation[cardinalList_] :=
  Switch[
    ToLowerCase @ Sort @ cardinalList,
      {"a", "b", "c"}, RedundantAbelianRepresentation[2],
      {"a", "b", "c", "d"}, RedundantAbelianRepresentation[3],
      {"x", "y"}, InfiniteAbelianGroup[2],
      {"x", "y", "z"}, InfiniteAbelianGroup[3],
      {"w", "x", "y", "z"}, InfiniteAbelianGroup[4],
      _, Message[CardinalQuiverRepresentation::noautorep, cardinalList]; Return[$Failed, Block]
  ];

CardinalQuiverRepresentation[quiver_, representation_:Automatic] := Scope[
  If[!CardinalQuiverQ[quiver = toCardinalQuiver[quiver]], ReturnFailed["notquiver"]];
  {cardinalList, representation} = parseRepresentationSpec[representation];
  SetAutomatic[cardinalList, CardinalList[quiver]];
  SetAutomatic[representation, chooseAutoRepresentation[cardinalList]];
  If[FailureQ[representation = toRepresentation[representation, Length[cardinalList]]],
    ReturnFailed["notrep"]];
  generatorList = representation["Generators"];
  If[Length[cardinalList] =!= Length[generatorList],
    ReturnFailed["gencount", Length[generatorList], Length[cardinalList]]];
  generators = AssociationThread[cardinalList, generatorList];
  assoc = Association[
    "Quiver" -> quiver,
    "Cardinals" -> cardinalList,
    "Generators" -> generators,
    "Representation" -> representation
  ];
  constructCardinalQuiverRepresentationObject[assoc]
];

constructCardinalQuiverRepresentationObject[assoc_] :=
  System`Private`ConstructNoEntry[CardinalQuiverRepresentationObject, assoc];

Format[RepresentationObject[matrix_?MatrixQ], StandardForm] :=
  renderRepresentationMatrix[matrix];

$representationIcon =
 Framed[Style["R", FontSize -> 20], FrameStyle -> Gray,
  ImageSize -> {35, 35}, Alignment -> Center]

$cardinalIconSize = 50 * {1, 1};
cardinalIcon[graph_] :=
  GraphPlot[Global`$g = graph,
    EdgeShapeFunction -> Automatic,
    ImageSize -> $cardinalIconSize,
    ImagePadding -> 2, PlotRangePadding -> 0,
    BaseStyle -> {}
  ];


PackageExport["CardinalQuiverRepresentationObjectQ"]

SetUsage @ "
CardinalQuiverRepresentationObjectQ[obj$] returns True if obj$ is a valid CardinalQuiverRepresentationObject.
"

CardinalQuiverRepresentationObjectQ[_CardinalQuiverRepresentationObject ? System`Private`HoldNoEntryQ] := True;
CardinalQuiverRepresentationObjectQ[_] := False;

CardinalQuiverRepresentationObject /: MakeBoxes[object:CardinalQuiverRepresentationObject[data_Association] ? System`Private`HoldNoEntryQ, format_] := ModuleScope[
  UnpackAssociation[data, quiver, cardinals, generators, representation];
  dimension = representation["Dimension"];
  group = representation["Group"];
  icon = cardinalIcon[quiver];
  icon = Insert[icon, AspectRatio -> All, 2];
  vertices = VertexCount[quiver];
  edges = EdgeCount[quiver];
  order = GroupOrder[group];
  BoxForm`ArrangeSummaryBox[
    CardinalQuiverRepresentationObject, object, icon,
    (* Always displayed *)
    {
     {summaryItem["Group", group], summaryItem["Cardinals", Row[cardinals, ","]]},
     {summaryItem["Dimension", dimension], summaryItem["Vertices", vertices]},
     {summaryItem["Order", order], summaryItem["Edges", edges]}
     },
    (* Displayed on request *)
    {},
    format,
    "Interpretable" -> Automatic
  ]
];

(CardinalQuiverRepresentationObject[data_Association] ? System`Private`HoldNoEntryQ)[property_String] :=
  getCqrProperty[data, property];

getCqrProperty[data_, prop_String] := Lookup[data, prop];

getCqrProperty[data_, "Identity"] := data["Representation"]["Identity"];

getCqrProperty[data_, "SymmetricCayleyFunction"] := computeCayleyFunction[data];

CayleyFunction[cqrep_ ? CardinalQuiverRepresentationObjectQ] := cqrep["SymmetricCayleyFunction"];

computeCayleyFunction[data_] := Scope[
  UnpackAssociation[data, generators, quiver];
  quiverEdges = EdgeList[quiver];
  cayleyComponents = Apply[
    {inVertex, outVertex, cardinal} |-> {
      gen = generators[cardinal]; igen = InverseFunction[gen];
      inVertex -> Labeled[LatticeVertex[gen[$arg1], outVertex], cardinal],
      If[igen =!= gen, outVertex -> Labeled[LatticeVertex[igen[$arg1], inVertex], Negated[cardinal]], Nothing]
    },
    quiverEdges, {1}
  ];
  switch = $switch[$arg2, Sequence @@ KeyValueMap[Sequence, Merge[cayleyComponents, Identity]]];
  func = Construct[Function, switch] //. {
    $arg1 -> Construct[Slot, 1],
    $arg2 -> Construct[Slot, 2],
    $switch[_, _, b_] :> b,
    $switch -> Switch
  };
  Apply @ func
];


PackageExport["CardinalQuiverRepresentationObject"]

SetUsage @ "
CardinalQuiverRepresentationObject[$$] represents a CardinalQuiver with an associated representation.
"


