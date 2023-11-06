PublicObject[RewritingSystemObject]

declareObjectPropertyDispatch[RewritingSystemObject, rewritingSystemProperty];

Options[RewritingSystemObject] = {
  CanonicalizationFunction -> None,
  "CustomProperties" -> {}
};

(**************************************************************************************************)

PrivateFunction[constructRewritingSystem]

RewritingSystemObject::badrules = "Invalid rewriting rules.";

constructRewritingSystem[type_, rules_, opts:OptionsPattern[RewritingSystemObject]] := Scope[
  UnpackOptions[canonicalizationFunction, customProperties];
  assoc = Assoc[
    "Type" -> type,
    "Rules" -> rules,
    "CanonicalizationFunction" -> canonicalizationFunction,
    customProperties
  ];
  ConstructNoEntry[RewritingSystemObject, assoc]
];

(* TODO: system for normalizing rules (e.g. handling <->), allowing manually tagged rules,
and reporting failure. *)

(**************************************************************************************************)

PrivateSpecialFunction[declareRewritingSystemDispatch, rewritingSystemProperty]

$rewritingSystemProperyDispatch = <||>;

rewritingSystemProperty[data_, args___] :=
  $rewritingSystemProperyDispatch[data["Type"]][data, args];

declareRewritingSystemDispatch[type_, dispatchFunction_] :=
  $rewritingSystemProperyDispatch[type] = dispatchFunction;

(**************************************************************************************************)

PublicFunction[RewritingSystemObjectQ]

RewritingSystemObjectQ = Case[
  rs_RewritingSystemObject ? HoldNoEntryQ := True;
  _ := False;
];

(**************************************************************************************************)

MakeBoxes[rs_RewritingSystemObject ? HoldNoEntryQ, form_] :=
  rewritingSystemObjectBoxes[rs, form];

rewritingSystemObjectBoxes[rs:RewritingSystemObject[data_], form_] := Scope[
  UnpackAssociation[data, type, rules];
  BoxForm`ArrangeSummaryBox[
    RewritingSystemObject, rs, None,
    (* Always displayed *)
    {
     {summaryItem["Type", type]},
     If[ListQ[rules], {summaryItem["Rules", Column @ rules]}, Nothing]
    },
    (* Displayed on request *)
    {},
    form,
    "Interpretable" -> Automatic
  ]
];

(**************************************************************************************************)

PublicFunction[RewriteStates]

Options[RewriteStates] = {
  MaxVertices -> Infinity,
  MaxEdges -> Infinity,
  ProgressFunction -> None,
  Verbose -> False
};

RewriteStates[system_RewritingSystemObject, initialStates_, opts:OptionsPattern[]] := Scope[
  
  UnpackOptions[verbose];

  rewriteMultiwaySystem[
    system, initialStates, False, verbose, "VertexList",
    FilterOptions @ opts
  ]
];

(**************************************************************************************************)

PublicFunction[RewriteQuiver, RewriteGraph]

Options[RewriteQuiver] = Options[RewriteGraph] = JoinOptions[
  RewriteStates,
  DirectedEdges -> True,
  $ExtendedGraphOptions
];

declareSyntaxInfo[LatticeGraph, {_, _, OptionsPattern[]}];
declareSyntaxInfo[LatticeQuiver, {_, _, OptionsPattern[]}];


RewriteQuiver[system_RewritingSystemObject, initialState_, opts:OptionsPattern[RewriteQuiver]] :=
  rewriteGraphQuiver[system, initialState, True, opts];

RewritingSystemObject::noallstates = "Rewriting system cannot enumerate set of all states."
RewritingSystemObject::noinitstates = "Empty set of initial states.";

RewriteGraph[system_RewritingSystemObject, initialStates_, opts:OptionsPattern[RewriteQuiver]] :=
  rewriteGraphQuiver[system, initialStates, False, opts];

rewriteGraphQuiver[system_, initialStates_, isQuiver_, opts:OptionsPattern[RewriteQuiver]] := Scope[

  UnpackOptions[layoutDimension, verbose];
  
  If[layoutDimension === 3, opts = Sequence[opts, VertexLayout -> SpringElectricalLayout[]]];
  
  result = rewriteMultiwaySystem[
    system, initialStates, isQuiver, verbose,
    {"VertexList", "IndexEdgeList", "TerminationReason"},
    FilterOptions[MultiwaySystem, opts]
  ];

  If[!ListQ[result], ReturnFailed[]];
  {vertices, indexEdgeList, terminationReason} = result;
  If[verbose, Print["TerminationReason: ", terminationReason]];

  opts = {
    GraphTheme -> If[isQuiver, "RewriteQuiver", "RewriteGraph"]
  };

  If[Len[indexEdgeList] > 0,
    If[isQuiver,
      cards = Union @ Part[indexEdgeList, All, 3];
      If[cards =!= {None}, AppendTo[opts, Cardinals -> cards]];
    ,
      If[(Len @ P1 @ indexEdgeList) === 3,
        indexEdgeList = Take[indexEdgeList, All, 2]];
    ];
  ];
  If[initialStates === All || Len[initialStates] > 1,
    AppendTo[opts, VertexLayout -> SpringElectricalLayout[]]];

  FromIndexedEdges[vertices, Union @ indexEdgeList, Seq @@ opts]
]

(**************************************************************************************************)

rewriteMultiwaySystem[system_, states_, isLabeled_, verbose_, args___] := Scope[
  
  Switch[states,
    All,
      states = system["AllStates"];
      If[!ListQ[states], ReturnFailed[RewritingSystemObject::noallstates]],
    {},
      ReturnFailed[RewritingSystemObject::noinitstates],
    _List,
      Null,
    _,
      states //= List;
  ];

  cayleyFunction = system["CayleyFunction", "Labeled" -> isLabeled];

  result = CachedMultiwaySystem[
    If[verbose, tapped[cayleyFunction], cayleyFunction], states,
    args,
    CanonicalizationFunction -> system["CanonicalizationFunction"]
  ]
];

(* TODO: fix Automatic options to inherit from the theme options *)

tapped[f_][arg_] := Module[{res = f[arg], res2}, res2 = Map[InputForm, StripLabel[res]]; Echo[InputForm[arg] -> If[ListQ[res2], Column[res2], res2]]; res];

(**************************************************************************************************)

DefineGraphTheme["RewriteQuiver",
  AspectRatioClipping -> False,
  ArrowheadSize -> 15,
  VertexSize -> 5, VertexFontSize -> 12,
  ImagePadding -> {Left -> 25, Right -> 25},
  ArrowheadPosition -> 0.45,
  ArrowheadShape -> "NarrowArrow",
  TwoWayStyle -> "CrossLine",
  ImageSize -> ("ShortestEdge" -> 65),
  VertexLayout -> TreeVertexLayout[Orientation -> Left, Balanced -> True],
  CollapseMultiedges -> True
];

DefineGraphTheme["RewriteGraph" -> "RewriteQuiver",
  ArrowheadStyle -> $Gray
];


