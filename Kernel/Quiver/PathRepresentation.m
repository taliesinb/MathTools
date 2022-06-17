PublicFunction[PathRepresentation]

SetUsage @ "
PathRepresentation[fquiver$, cardinals$ -> representation$, initialVertex$] attachs a path representation to a \
fundamental quiver, returning a PathRepresentationObject.
* The list cardinals$ is matched with the generators of representation$ in order.
* The list cardinals$ can also be given as a single string whose letters are the cardinals.
* If no cardinals are provided, the cardinals present in quiver$ will be used, in sorted order.
PathRepresentation[quiver$] chooses a representation based on the names of the cardinals in quiver$.
* For cardinals {'x', 'y'} or {'x', 'y', 'z'}, a representation of InfiniteAbelianGroup is used.
* For cardinals {'a', 'b', 'c'}, {'a', 'b', 'c', 'd'}, a redundant representation of an InfiniteAbelianGroup is used.
* initialVertex$ should be a vertex of quiver$, or Automatic to choose the first one.
"

DeclareArgumentCount[PathRepresentation, {1, 3}];

declareSyntaxInfo[PathRepresentation, {_, _., _.}];

PathRepresentation::noautorep =
  "No automatic representation is defined for the cardinal set ``.";

PathRepresentation::gencount =
  "The number of generators in the representation (``) did not match the number of cardinals in the graph (``).";

$namedRep = "Abelian" | "Dihedral" | "Redundant" | "RedundantDihedral";

parseRepresentationSpec = MatchValues[
  Automatic          := {Automatic, Automatic};
  name:$namedRep     := {Automatic, name};
  s_String           := {carChars[s], Automatic};
  s_String -> rep_   := {carChars[s], rep};
  list_List -> rep_  := {list, rep};
  a_Association      := {Keys @ a, CustomRepresentation @ Values @ a};
  rep_               := {Automatic, rep}
];

carChars[str_] := Characters[str] /. "_" -> None;

chooseAutoRepresentation[cardinalList_] :=
  Switch[
    ToLowerCase @ Sort @ cardinalList,
      {"x"}, InfiniteAbelianGroup[1],
      {"x", "y"} | {"b", "r"}, InfiniteAbelianGroup[2],
      {"x", "y", "z"} | {"b", "g", "r"}, InfiniteAbelianGroup[3],
      {"w", "x", "y", "z"}, InfiniteAbelianGroup[4],
      {"a", "b", "c"}, InfiniteAbelianGroup[3, "Redundant"],
      {"a", "b", "c", "d"}, InfiniteAbelianGroup[4, "Redundant"],
      _, Message[PathRepresentation::noautorep, cardinalList]; Return[$Failed, Block]
  ];

PathRepresentation[quiver_, representation_:Automatic, initialVertex_:Automatic] := Scope[
  quiver = CoerceToQuiver[1];
  {cardinalListSpec, representation} = parseRepresentationSpec[representation];
  cardinalList = DeleteNone[cardinalListSpec];
  SetAutomatic[cardinalList, CardinalList[quiver]];
  SetAutomatic[representation, chooseAutoRepresentation[cardinalList]];
  If[FailureQ[representation = toRepresentation[representation, Length[cardinalList]]],
    ReturnFailed["notrep", "second"]];
  generatorList = representation["Generators"];
  If[ContainsQ[cardinalListSpec, None],
    generatorList = Part[generatorList, SelectIndices[cardinalListSpec, # =!= None&]];
  ];
  If[Length[cardinalList] =!= Length[generatorList],
    ReturnFailed["gencount", Length[generatorList], Length[cardinalList]]];
  generators = AssociationThread[cardinalList, generatorList];
  SetAutomatic[initialVertex, Part[VertexList[quiver], 1]];
  assoc = Association[
    "Quiver" -> quiver,
    "Cardinals" -> cardinalList,
    "Generators" -> generators,
    "Representation" -> representation,
    "InitialVertex" -> initialVertex
  ];
  constructPathRepresentationObject[assoc]
];

constructPathRepresentationObject[assoc_] :=
  System`Private`ConstructNoEntry[PathRepresentationObject, assoc];

Format[RepresentationObject[matrix_?MatrixQ], StandardForm] :=
  renderRepresentationMatrix[matrix];

(**************************************************************************************************)

PublicFunction[PathRepresentationPlot]

DeclareArgumentCount[PathRepresentationPlot, 1];

Options[PathRepresentationPlot] = JoinOptions[
  Transposed -> False,
  Quiver
];

PathRepresentationPlot[qrep_, opts:OptionsPattern[]] := Scope[

  UnpackOptions[plotLabel, transposed];
  If[StringQ[qrep],
    SetAutomatic[plotLabel, ToTitleString[qrep]];
    qrep = LatticeQuiverData[qrep, "Representation"]];

  If[!PathRepresentationObjectQ[qrep], ReturnFailed[]];

  quiver = qrep["Quiver"];
  quiverPlot = Quiver[quiver,
    PlotLabel -> plotLabel, FilterOptions @ opts, ImageSize -> {60, 80}, GraphLegend -> None,
    VertexSize -> Large, ArrowheadShape -> {"Line", EdgeThickness -> 2.5}
  ] // CombineMultiedges;

  colors = LookupCardinalColors[quiver];
  labeledGenerators = makeLabeledGenerators[qrep["Generators"], colors];

  SpacedRow[quiverPlot, labeledGenerators, Transposed -> transposed]
];

PrivateFunction[makeLabeledGenerators]

makeLabeledGenerators[generators_, cardinalColors_] :=
  Row[
    KeyValueMap[
      Labeled[#2, Row[{makeLegendArrowheadGraphic[cardinalColors @ #1, "Arrow"], "\[ThinSpace]", #1}]]&,
      generators
    ],
    "  ", BaseStyle -> $CardinalLabelStyle
  ];

(**************************************************************************************************)

PublicObject[PathRepresentationObject]

SetUsage @ "
PathRepresentationObject[$$] represents a Quiver with an associated representation.
"

PathRepresentationObject /: MakeBoxes[object:PathRepresentationObject[data_Association] ? System`Private`HoldNoEntryQ, format_] := ModuleScope[
  UnpackAssociation[data, quiver, generators, representation];
  dimension = representation["Dimension"];
  group = representation["Group"];
  icon = ExtendedGraphPlot @ ExtendedGraph[quiver, ImageSize -> {60, 50}, GraphLegend -> None];
  cardinalColors = LookupCardinalColors @ quiver;
  coloredCardinals = KeyValueMap[Style[#1, Bold, #2]&, cardinalColors];
  vertices = VertexCount[quiver];
  edges = EdgeCount[quiver];
  order = representation["GroupOrder"];
  labeledGenerators = makeLabeledGenerators[generators, cardinalColors];
  BoxForm`ArrangeSummaryBox[
    PathRepresentationObject, object, icon,
    (* Always displayed *)
    {
     {summaryItem["Group", group], summaryItem["Cardinals", Row[coloredCardinals, ","]]},
     {summaryItem["Dimension", dimension], summaryItem["Vertices", vertices]},
     {summaryItem["Order", order], summaryItem["Edges", edges]}
     },
    (* Displayed on request *)
    {
      {labeledGenerators, SpanFromLeft}
    },
    format,
    "Interpretable" -> Automatic
  ]
];


declareObjectPropertyDispatch[PathRepresentationObject, pathRepresentationProperty];

pathRepresentationProperty[data_, "Identity"] := PathValue[
  data["InitialVertex"],
  data["Representation"]["Identity"]
];

pathRepresentationProperty[data_, "AllIdentities"] := Scope[
  idRep = data["Representation"]["Identity"];
  Map[
    PathValue[idRep, #]&,
    VertexList @ data["Quiver"]
  ]
];

pathRepresentationProperty[data_, "CayleyFunction", opts___Rule] :=
  computeCayleyFunction[data, opts];

makeQuiverElementRule[inVertex_, outVertex_, gen_, cardinal_] :=
  PathValue[inVertex, \[FormalR] : _] :> Labeled[PathValue[outVertex, gen[\[FormalR]]], cardinal];

Options[computeCayleyFunction] = {"Symmetric" -> True, "Labeled" -> True};

(**************************************************************************************************)

RenameCardinals[qrep_PathRepresentationObject, renaming_List] :=
  RenameCardinals[qrep, RuleThread[qrep["Cardinals"], renaming]];

RenameCardinals[PathRepresentationObject[data_Association], renaming:{__Rule}] := Scope[
  UnpackAssociation[data, quiver, cardinals, generators, representation];

  quiver = RenameCardinals[quiver, renaming];
  cardinals = cardinals /. renaming;
  generators = KeyMap[Replace[renaming], generators];

  assoc = Association[
    "Quiver" -> quiver,
    "Cardinals" -> cardinals,
    "Generators" -> generators,
    "Representation" -> representation
  ];

  constructPathRepresentationObject[assoc]
];

(**************************************************************************************************)

PublicHead[PathValue]

SetUsage @ "
PathValue[v$, state$] represents a quiver vertex v$ with associated state state$.
"

computeCayleyFunction[data_, OptionsPattern[]] := Scope[
  UnpackAssociation[data, generators, quiver];
  UnpackOptions[symmetric, labeled];
  quiverEdges = EdgeList[quiver];
  rules = Flatten @ Apply[
    {inVertex, outVertex, cardinal} |-> (
      gen = generators[cardinal];
      If[MissingQ[gen], Nothing, {
        makeQuiverElementRule[inVertex, outVertex, gen, cardinal],
        If[symmetric && (igen = ToInverseFunction[gen]) =!= gen && igen =!= None,
          makeQuiverElementRule[outVertex, inVertex, igen, Inverted @ cardinal],
          Nothing
        ]
      }]
    ),
    quiverEdges, {1}
  ];
  If[!labeled, rules = rules /. Labeled[g_, _] :> g];
  ReplaceList[rules]
];

(**************************************************************************************************)

PublicFunction[PathRepresentationObjectQ]

SetUsage @ "
PathRepresentationObjectQ[obj$] returns True if obj$ is a valid PathRepresentationObject.
"

PathRepresentationObjectQ[_PathRepresentationObject ? System`Private`HoldNoEntryQ] := True;
PathRepresentationObjectQ[_] := False;



