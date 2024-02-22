PublicFunction[PathRepresentation]

SetUsage @ "
PathRepresentation[fquiver$, representation$, initialVertex$] attachs a path representation to a \
fundamental quiver, returning a PathRepresentationObject.
* representation$ can be a LinearGroupRepresentation, or a valid group.
* If the labels of fquiver$ do not match the generator names in representation$, then they override those in representation, and are used in matching order.
* If no cardinals are provided, the cardinals present in quiver$ will be used, in sorted order.
* initialVertex$ should be a vertex of quiver$, or Automatic to choose the first one.
"

DeclareArgumentCount[PathRepresentation, {1, 3}];

declareSyntaxInfo[PathRepresentation, {_, _., _.}];

PathRepresentation::noautorep =
  "No automatic representation is defined for the cardinal set ``.";

PathRepresentation::gencount =
  "The number of generators in the representation (``) did not match the number of cardinals in the graph (``).";

$namedRep = "Abelian" | "Dihedral" | "Redundant" | "RedundantDihedral";

PathRepresentation[quiver_, representation_, initialVertex_:Auto] := Scope[
  quiver = CoerceToQuiver[1];
  representation = CoerceToRep[2];
  cardinals = CardinalList[quiver];
  generators = representation["Generators"];
  If[!SameLengthQ[cardinals, generators],
    ReturnFailed["gencount", Len @ generatorList, Len @ cardinalList]];
  If[!SameSetQ[cardinals, Keys @ generators],
    generators = AssocThread[cardinals, Values @ generators];
  ];
  SetAutomatic[initialVertex, F[VertexList[quiver]]];
  assoc = Assoc[
    "Quiver" -> quiver,
    "Cardinals" -> cardinals,
    "Generators" -> generators,
    "Representation" -> representation,
    "InitialVertex" -> initialVertex
  ];
  constructPathRepresentationObject[assoc]
];

constructPathRepresentationObject[assoc_] :=
  ConstructNoEntry[PathRepresentationObject, assoc];

Format[LinearRepresentationObject[matrix_?MatrixQ], StandardForm] :=
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
  If[StrQ[qrep],
    SetAutomatic[plotLabel, ToTitleString[qrep]];
    qrep = LatticeQuiverData[qrep, "Representation"]];

  If[!PathRepresentationObjectQ[qrep], ReturnFailed[]];

  quiver = qrep["Quiver"];
  quiverPlot = Quiver[quiver,
    PlotLabel -> plotLabel, FilterOptions @ opts, ImageSize -> {60, 80}, GraphLegend -> None,
    VertexSize -> Large, ArrowheadShape -> {"Line", EdgeThickness -> 2.5}
  ] // CombineMultiedges;

  colors = LookupCardinalColors[quiver];
  labeledGenerators = makeColoredGenerators[qrep["Generators"], colors];

  SpacedRow[quiverPlot, labeledGenerators, Transposed -> transposed]
];

PrivateFunction[makeColoredGenerators]

makeColoredGenerators[generators_, cardinalColors_] :=
  Row[
    KVMap[
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

PathRepresentationObject /: MakeBoxes[object:PathRepresentationObject[data_Assoc] ? HoldNoEntryQ, format_] := ModuleScope[
  UnpackAssociation[data, quiver, generators, representation];
  dimension = representation["Dimension"];
  group = representation["Group"];
  icon = ExtendedGraphPlot @ ExtendedGraph[quiver, ImageSize -> {60, 50}, GraphLegend -> None];
  cardinalColors = LookupCardinalColors @ quiver;
  coloredCardinals = KVMap[Style[#1, Bold, #2]&, cardinalColors];
  vertices = VertexCount[quiver];
  edges = EdgeCount[quiver];
  order = representation["GroupOrder"];
  coloredGenerators = makeColoredGenerators[generators, cardinalColors];
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
      {coloredGenerators, SpanFromLeft}
    },
    format,
    "Interpretable" -> Auto
  ]
];


DefineObjectPropertyDispatch[PathRepresentationObject, pathRepresentationProperty];

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

RenameCardinals[PathRepresentationObject[data_Assoc], renaming:{__Rule}] := Scope[
  UnpackAssociation[data, quiver, cardinals, generators, representation];

  quiver = RenameCardinals[quiver, renaming];
  cardinals = cardinals /. renaming;
  generators = KMap[Rep[renaming], generators];

  assoc = Assoc[
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
PathValue[v$, state$] represents a fundamental quiver vertex v$ with associated state state$.
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

PathRepresentationObjectQ[_PathRepresentationObject ? HoldNoEntryQ] := True;
PathRepresentationObjectQ[_] := False;


