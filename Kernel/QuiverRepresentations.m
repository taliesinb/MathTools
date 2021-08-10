PackageExport["QuiverRepresentation"]

SetUsage @ "
QuiverRepresentation[quiver$, cardinals$ -> representation$] attachs a representation to a quiver, returning \
a QuiverRepresentationObject.
* The list cardinals$ is matched with the generators of representation$ in order.
* The list cardinals$ can also be given as a single string whose letters are the cardinals.
* If no cardinals are provided, the cardinals present in quiver$ will be used, in sorted order.
QuiverRepresentation[quiver$] chooses a representation based on the names of the cardinals in quiver$.
* For cardinals {'x', 'y'} or {'x', 'y', 'z'}, a representation of InfiniteAbelianGroup is used.
* For cardinals {'a', 'b', 'c'}, {'a', 'b', 'c', 'd'}, a redundant representation of an InfiniteAbelianGroup is used.
"

DeclareArgumentCount[QuiverRepresentation, {1, 2}];

declareSyntaxInfo[QuiverRepresentation, {_, _.}];

QuiverRepresentation::noautorep =
  "No automatic representation is defined for the cardinal set ``.";

QuiverRepresentation::gencount =
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
      _, Message[QuiverRepresentation::noautorep, cardinalList]; Return[$Failed, Block]
  ];

QuiverRepresentation[quiver_, representation_:Automatic] := Scope[
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
  assoc = Association[
    "Quiver" -> quiver,
    "Cardinals" -> cardinalList,
    "Generators" -> generators,
    "Representation" -> representation
  ];
  constructQuiverRepresentationObject[assoc]
];

constructQuiverRepresentationObject[assoc_] :=
  System`Private`ConstructNoEntry[QuiverRepresentationObject, assoc];

Format[RepresentationObject[matrix_?MatrixQ], StandardForm] :=
  renderRepresentationMatrix[matrix];

(**************************************************************************************************)

PackageExport["QuiverRepresentationPlot"]

DeclareArgumentCount[QuiverRepresentationPlot, 1];

Options[QuiverRepresentationPlot] = JoinOptions[
  "Transposed" -> False,
  Quiver
];

QuiverRepresentationPlot[qrep_, opts:OptionsPattern[]] := Scope[

  UnpackOptions[plotLabel, transposed];
  If[StringQ[qrep],
    SetAutomatic[plotLabel, ToTitleString[qrep]];
    qrep = LatticeQuiverData[qrep, "Representation"]];

  If[!QuiverRepresentationObjectQ[qrep], ReturnFailed[]];

  quiver = qrep["Quiver"];
  quiverPlot = Quiver[quiver,
    PlotLabel -> plotLabel, FilterOptions @ opts, ImageSize -> {60, 80}, GraphLegend -> None,
    VertexSize -> Large, ArrowheadShape -> {"Line", EdgeThickness -> 2.5}, GraphLayout -> {"MultiEdgeDistance" -> 0.5}
  ];

  colors = LookupCardinalColors[quiver];
  labeledGenerators = makeLabeledGenerators[qrep["Generators"], colors];

  SpacedRow[quiverPlot, labeledGenerators, "Transposed" -> transposed]
];

PackageScope["makeLabeledGenerators"]

makeLabeledGenerators[generators_, cardinalColors_] :=
  Row[
    KeyValueMap[
      Labeled[#2, Row[{makeLegendArrowheadGraphic[cardinalColors @ #1, "Arrow"], " ", #1}]]&,
      generators
    ],
    "  ", BaseStyle -> $LabelStyle
  ];

(**************************************************************************************************)

PackageExport["QuiverRepresentationObject"]

SetUsage @ "
QuiverRepresentationObject[$$] represents a Quiver with an associated representation.
"

QuiverRepresentationObject /: MakeBoxes[object:QuiverRepresentationObject[data_Association] ? System`Private`HoldNoEntryQ, format_] := ModuleScope[
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
    QuiverRepresentationObject, object, icon,
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


declareObjectPropertyDispatch[QuiverRepresentationObject, quiverRepresentationProperty];

quiverRepresentationProperty[data_, "Identity"] := QuiverElement[
  Part[VertexList[data["Quiver"]], MaximumIndex[BetweennessCentrality @ data["Quiver"]]],
  data["Representation"]["Identity"]
];

quiverRepresentationProperty[data_, "AllIdentities"] := Scope[
  idRep = data["Representation"]["Identity"];
  Map[
    QuiverElement[idRep, #]&,
    VertexList @ data["Quiver"]
  ]
];

quiverRepresentationProperty[data_, "CayleyFunction", opts___Rule] :=
  computeCayleyFunction[data, opts];

makeQuiverElementRule[inVertex_, outVertex_, gen_, cardinal_] :=
  QuiverElement[inVertex, \[FormalR] : _] :> Labeled[QuiverElement[outVertex, gen[\[FormalR]]], cardinal];

Options[computeCayleyFunction] = {"Symmetric" -> True, "Labeled" -> True};

(**************************************************************************************************)

RenameCardinals[qrep_QuiverRepresentationObject, renaming_List] :=
  RenameCardinals[qrep, RuleThread[qrep["Cardinals"], renaming]];

RenameCardinals[QuiverRepresentationObject[data_Association], renaming:{__Rule}] := Scope[
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

  constructQuiverRepresentationObject[assoc]
];

(**************************************************************************************************)

PackageExport["QuiverElement"]

SetUsage @ "
QuiverElement[v$, state$] represents a quiver vertex v$ with associated state state$.
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
          makeQuiverElementRule[outVertex, inVertex, igen, Negated @ cardinal],
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

PackageExport["QuiverRepresentationObjectQ"]

SetUsage @ "
QuiverRepresentationObjectQ[obj$] returns True if obj$ is a valid QuiverRepresentationObject.
"

QuiverRepresentationObjectQ[_QuiverRepresentationObject ? System`Private`HoldNoEntryQ] := True;
QuiverRepresentationObjectQ[_] := False;



