PackageExport["GroupoidQ"]

SetUsage @ "
GroupoidQ[groupoid$] returns True if groupoid$ is a valid groupoid.
"

GroupoidQ[e_ ? GroupQ] := True;

(**************************************************************************************************)

PackageExport["GroupoidGenerator"]

SetUsage @ "
GroupoidGenerator[f$, g$] represents an element of a groupoid that acts on a GroupoidState[$$] by f$, and
acts on a GroupoidMirrorState[$$] by g$.
"

gen_GroupoidGenerator[states_List] := Map[gen, states];
GroupoidGenerator[f_, g_][s_] := makeState[f, s];
GroupoidGenerator[f_, g_][Negated[s_]] := Negated @ makeState[g, s];

ToInverseFunction[GroupoidGenerator[f_, g_]] :=
  GroupoidGenerator[g, f];

makeState[f_, s_] := Scope[
  s2 = f[s];
  If[s2 === s || s2 === Nothing, Negated[s], s2]
];

declareFormatting[
  GroupoidGenerator[f_, g_] :> RawBoxes @ FractionBox[ToBoxes @ f, ToBoxes @ g]
];

(**************************************************************************************************)

PackageExport["GroupoidObject"]

PackageScope["constructGroupoid"]

constructGroupoid[assoc_] := Scope[
  assoc = assoc;
  If[!KeyExistsQ[assoc, "States"], assoc["States"] = Indeterminate];
  If[!KeyExistsQ[assoc, "Generators"], assoc["Generators"] = assoc["MirroredGenerators"][[All, 1]]];
  System`Private`ConstructNoEntry[GroupoidObject, assoc]
];

declareObjectPropertyDispatch[GroupoidObject, groupoidProperty];

groupoidProperty[data_, "CayleyFunction", opts___Rule] :=
  computeCayleyFunction[data, opts];

groupoidProperty[data_, "CayleyGraph", opts___Rule] :=
  computeCayleyGraph[data, opts];

groupoidProperty[data_, "CompleteStates"] := Scope[
  states = data["States"];
  If[states === Indeterminate, Return @ Indeterminate];
  Join[states, Negated /@ states]
];

(**************************************************************************************************)

MakeBoxes[go_GroupoidObject ? System`Private`HoldNoEntryQ, form_] :=
  groupoidObjectBoxes[go, form];

groupoidObjectBoxes[object:GroupoidObject[data_], form_] := Scope[
  UnpackAssociation[data, type, generators, states, initialStates];
  BoxForm`ArrangeSummaryBox[
    GroupoidObject, object, None,
    (* Always displayed *)
    {
     {summaryItem["Type", type], SpanFromLeft},
     {summaryItem["Generators", Length @ generators]},
     {summaryItem["Initial states", Length @ initialStates]},
     If[states =!= Indeterminate, {summaryItem["States", Length @ states]}, Nothing]
     },
    (* Displayed on request *)
    {{Column[generators], SpanFromLeft}},
    form,
    "Interpretable" -> Automatic
  ]
];

(**************************************************************************************************)

Options[computeCayleyFunction] = {"Symmetric" -> True, "Labeled" -> True};

computeCayleyFunction[data_, OptionsPattern[]] := Scope[
  UnpackAssociation[data, generators];
  UnpackOptions[symmetric, labeled];
  list = Flatten @ MapIndexed[
    {gen, index} |-> {
      If[labeled, Labeled[First @ index], Identity] @ gen,
      If[symmetric && (igen = ToInverseFunction[gen]) =!= gen,
        If[labeled, Labeled[Negated @ First @ index], Identity] @ igen,
        Nothing
      ]
    },
    generators
  ];
  ApplyThrough[list]
];

(**************************************************************************************************)

computeCayleyGraph[data_, opts___Rule] := Scope[
  init = data["InitialStates"];
  cfunc = computeCayleyFunction[data, "Symmetric" -> False];
  MultiwaySystem[cfunc, init, "CayleyGraph", opts, MaxVertices -> 10^3, SelfLoops -> False]
];

(**************************************************************************************************)

PackageExport["GroupoidPermutationGroup"]

GroupoidPermutationGroup[groupoid_] := Scope[
  data = getObjectData[groupoid];
  UnpackAssociation[data, mirroredGenerators, states];
  statesBasis = Join[states, Negated /@ states];
  PermutationGroup @ Table[
    FindPermutation[statesBasis, generator @ statesBasis],
    {generator, generators}
  ]
];

(**************************************************************************************************)

PackageExport["GroupoidPermutationTable"]

GroupoidPermutationTable[groupoid_] := Scope[
  data = getObjectData[groupoid];
  UnpackAssociation[data, generators, states];
  statesBasis = Join[states, Negated /@ states];
  TableForm[
    # @ statesBasis& /@ generators,
    TableHeadings -> {Automatic, statesBasis}
  ]
];

(**************************************************************************************************)

PackageExport["GroupoidObjectQ"]

GroupoidObjectQ[go_GroupoidObject ? System`Private`HoldNoEntryQ] := True;
GroupoidObjectQ[_] := False;


(**************************************************************************************************)

PackageExport["ColoredTokenGroupoid"]

ColoredTokenGroupoid[n_Integer, colors_Integer] := constructGroupoid @ Association[
  "Type" -> "ColorToken",
  "Generators" -> makeColoredTokenGenerators[n, colors],
  "MirroredGenerators" -> makeMirroredColoredTokenGenerators[n, colors],
  "InitialStates" -> {ColoredTokens @ Ones[n]},
  "States" -> Map[ColoredTokens, Tuples[Range[colors], n]]
];

makeColoredTokenGenerators[n_, colors_] :=
  Flatten @ Table[
    If[j === k, Nothing, SetTokenColor[i, j, k]],
    {i, 1, n}, {j, colors}, {k, colors}
  ];

makeMirroredColoredTokenGenerators[n_, colors_] :=
  Flatten @ Table[
    If[j === k, Nothing, GroupoidGenerator[SetTokenColor[i, j, k], SetTokenColor[i, k, j]]],
    {i, 1, n}, {j, colors}, {k, colors}
  ];


PackageExport["ColoredTokens"]

$TokenColorPalette = Prepend[$ColorPalette, GrayLevel[0.98]];

declareFormatting[
  ColoredTokens[list_List] :> Pane[Row[Part[$TokenColorPalette, list], " "], ContentPadding -> False, FrameMargins -> {{0, 0}, {2, 2}}]
];


PackageExport["SetTokenColor"]

SetTokenColor[n_, c_][ColoredTokens[list_]] :=
  If[list[[n]] === c, Nothing, ColoredTokens @ ReplacePart[list, n -> c]];

SetTokenColor[n_, c1_, c2_][ColoredTokens[list_]] :=
  If[list[[n]] != c1, Nothing, ColoredTokens @ ReplacePart[list, n -> c2]];

(**************************************************************************************************)

PackageExport["TicTacToeGroupoid"]

