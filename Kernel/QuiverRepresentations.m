Package["GraphTools`"]


PackageImport["GeneralUtilities`"]


PackageExport["QuiverRepresentation"]

SetUsage @ "
QuiverRepresentation[quiver$, cardinals$ -> representation$] attachs a representation to a quiver, returning \
a QuiverRepresentationObject.
* The list cardinals$ is matched with the generators of representation$ in order.
* The list cardinals$ can also be given as a single string whose letters are the cardinals.
* If no cardinals are provided, the cardinals present in quiver$ will be used, in sorted order.
QuiverRepresentation[quiver$] chooses a representation based on the names of the cardinals in quiver$.
* For cardinals {'x', 'y'} or {'x', 'y', 'z'}, a representation of InfiniteAbelianGroup is used.
* For cardinals {'a', 'b', 'c'}, RedundantAbelianRepresentation[3] is used.
"

QuiverRepresentation::notquiver =
  "The first argument to QuiverRepresentation should be a cardinal quiver Graph."

QuiverRepresentation::notrep =
  "The second argument to QuiverRepresentation should be a RepresentationObject or one of the forms documented in ?QuiverRepresentation."

QuiverRepresentation::noautorep =
  "No automatic representation is defined for the cardinal set ``.";

QuiverRepresentation::gencount =
  "The number of generators in the representation (``) did not match the number of cardinals in the graph (``).";

parseRepresentationSpec = MatchValues[
  Automatic         := {Automatic, Automatic};
  "Abelian"         := {Automatic, "Abelian"};
  "Redundant"       := {Automatic, "Redundant"};
  s_String          := {carChars[s], Automatic};
  s_String -> rep_  := {carChars[s], rep};
  list_List -> rep_ := {list, rep};
  rep_              := {Automatic, rep}
];

carChars[str_] := Characters[str] /. "_" -> None;

chooseAutoRepresentation[cardinalList_] :=
  Switch[
    ToLowerCase @ Sort @ cardinalList,
      {"a", "b", "c"}, RedundantAbelianRepresentation[2],
      {"a", "b", "c", "d"}, RedundantAbelianRepresentation[3],
      {"x", "y"}, InfiniteAbelianGroup[2],
      {"x", "y", "z"}, InfiniteAbelianGroup[3],
      {"w", "x", "y", "z"}, InfiniteAbelianGroup[4],
      _, Message[QuiverRepresentation::noautorep, cardinalList]; Return[$Failed, Block]
  ];

QuiverRepresentation[quiver_, representation_:Automatic] := Scope[
  If[!QuiverQ[quiver = toQuiver[quiver]], ReturnFailed["notquiver"]];
  {cardinalListSpec, representation} = parseRepresentationSpec[representation];
  cardinalList = DeleteNone[cardinalListSpec];
  SetAutomatic[cardinalList, CardinalList[quiver]];
  SetAutomatic[representation, chooseAutoRepresentation[cardinalList]];
  If[FailureQ[representation = toRepresentation[representation, Length[cardinalList]]],
    ReturnFailed["notrep"]];
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

PackageExport["QuiverRepresentationPlot"]

QuiverRepresentationPlot[qrep_, opts:OptionsPattern[Quiver]] := Scope[
  If[!QuiverRepresentationObjectQ[qrep], ReturnFailed[]];

  quiver = qrep["Quiver"];
  quiverPlot = Quiver[quiver, opts, ImageSize -> Tiny, GraphLegend -> None];

  colors = CardinalColors[quiver];
  labeledGenerators = KeyValueMap[
    Labeled[#2, Row[{ColoredArrowhead[colors[#1], 10], " ", #1}]]&,
    qrep["Generators"]];
  Row[{quiverPlot, "  ", Row[labeledGenerators, " "]}]
];


PackageExport["QuiverRepresentationObjectQ"]

SetUsage @ "
QuiverRepresentationObjectQ[obj$] returns True if obj$ is a valid QuiverRepresentationObject.
"

QuiverRepresentationObjectQ[_QuiverRepresentationObject ? System`Private`HoldNoEntryQ] := True;
QuiverRepresentationObjectQ[_] := False;

QuiverRepresentationObject /: MakeBoxes[object:QuiverRepresentationObject[data_Association] ? System`Private`HoldNoEntryQ, format_] := ModuleScope[
  UnpackAssociation[data, quiver, cardinals, generators, representation];
  dimension = representation["Dimension"];
  group = representation["Group"];
  icon = cardinalIcon[quiver];
  icon = Insert[icon, AspectRatio -> All, 2];
  vertices = VertexCount[quiver];
  edges = EdgeCount[quiver];
  order = GroupOrder[group];
  BoxForm`ArrangeSummaryBox[
    QuiverRepresentationObject, object, icon,
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


declareObjectPropertyDispatch[QuiverRepresentationObject, quiverRepresentationProperty];

quiverRepresentationProperty[data_, "Identity"] := QuiverElement[
  First @ VertexList @ data["Quiver"],
  data["Representation"]["Identity"]
];

quiverRepresentationProperty[data_, "CayleyFunction", opts___Rule] :=
  computeCayleyFunction[data, opts];

quiverRepresentationProperty[data_, other___] := Print[other];


makeQuiverElementRule[inVertex_, outVertex_, gen_, cardinal_] :=
  QuiverElement[inVertex, \[FormalR] : _] :> Labeled[QuiverElement[outVertex, gen[\[FormalR]]], cardinal];

Options[computeCayleyFunction] = {"Symmetric" -> True, "Labeled" -> True};

computeCayleyFunction[data_, OptionsPattern[]] := Scope[
  UnpackAssociation[data, generators, quiver];
  UnpackOptions[symmetric, labeled];
  quiverEdges = EdgeList[quiver];
  rules = Flatten @ Apply[
    {inVertex, outVertex, cardinal} |-> {
      gen = generators[cardinal];
      makeQuiverElementRule[inVertex, outVertex, gen, cardinal],
      If[symmetric && (igen = InverseFunction[gen]) =!= gen,
        makeQuiverElementRule[outVertex, inVertex, igen, Negated @ cardinal]],
        Nothing
    },
    quiverEdges, {1}
  ];
  If[!labeled, rules = rules /. Labeled[g_, _] :> g];
  ReplaceList[rules]
];


PackageExport["QuiverElement"]

SetUsage @ "
QuiverElement[v$, state$] represents a quiver vertex v$ with associated state state$.
"


PackageExport["QuiverRepresentationObject"]

SetUsage @ "
QuiverRepresentationObject[$$] represents a Quiver with an associated representation.
"


