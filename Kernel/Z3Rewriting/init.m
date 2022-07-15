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
  If[rules =!= Null,
    rules //= ToList;
    If[!RuleListQ[rules /. TwoWayRule -> Rule], ReturnFailed[RewritingSystemObject::badrules]];
  ];
  UnpackOptions[canonicalizationFunction, customProperties];
  assoc = Association[
    "Type" -> type,
    "Rules" -> rules,
    "CanonicalizationFunction" -> canonicalizationFunction,
    customProperties
  ];
  System`Private`ConstructNoEntry[RewritingSystemObject, assoc]
];

(**************************************************************************************************)

PrivateFunction[declareRewritingSystemDispatch, rewritingSystemProperty]

$rewritingSystemProperyDispatch = <||>;

rewritingSystemProperty[data_, args___] :=
  $rewritingSystemProperyDispatch[data["Type"]][data, args];

declareRewritingSystemDispatch[type_, dispatchFunction_] :=
  $rewritingSystemProperyDispatch[type] = dispatchFunction;

(**************************************************************************************************)

PublicFunction[RewritingSystemObjectQ]

RewritingSystemObjectQ = Case[
  rs_RewritingSystemObject ? System`Private`HoldNoEntryQ := True;
  _ := False;
];

(**************************************************************************************************)

MakeBoxes[rs_RewritingSystemObject ? System`Private`HoldNoEntryQ, form_] :=
  rewritingSystemObjectBoxes[rs, form];

rewritingSystemObjectBoxes[rs:RewritingSystemObject[data_], form_] := Scope[
  UnpackAssociation[data, type, rules];
  BoxForm`ArrangeSummaryBox[
    RewritingSystemObject, object, None,
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

PublicFunction[RewriteQuiver, RewriteGraph]

Options[rewriteGraphQuiver] = Options[RewriteQuiver] = Options[RewriteGraph] = $ExtendedGraphOptions;

declareSyntaxInfo[LatticeGraph, {_, _, OptionsPattern[]}];
declareSyntaxInfo[LatticeQuiver, {_, _, OptionsPattern[]}];

RewriteQuiver[system_RewritingSystemObject, initialState_, opts:OptionsPattern[]] :=
  rewriteGraphQuiver[system, initialState, True, opts];

RewritingSystemObject::noallstates = "Rewriting system cannot enumerate set of all states."

RewriteGraph[system_RewritingSystemObject, initialState_, opts:OptionsPattern[]] :=
  rewriteGraphQuiver[system, initialState, False, opts];

rewriteGraphQuiver[system_, initialState_, isQuiver_, opts:OptionsPattern[]] := Scope[
  cayleyFunction = system["CayleyFunction", "Labeled" -> True (* isQuiver *)];
  canonFunction = system["CanonicalizationFunction"];
  If[initialState === All,
    initialState = system["AllStates"];
    If[!ListQ[initialState], ReturnFailed[RewritingSystemObject::noallstates]]
  ];
  initialState //= ToList;
  If[canonFunction =!= None,
    cayleyFunction = cayleyFunction /* applyCanonicalizationFunction[canonFunction];
    initialState //= Map[canonFunction]
  ];
  UnpackOptions[layoutDimension];
  If[layoutDimension === 3, opts = Sequence[opts, VertexLayout -> SpringElectricalLayout[]]];
  result = LatticeQuiver[
    <|"CayleyFunction" -> cayleyFunction, "InitialStates" -> initialState|>, opts,
    GraphTheme -> If[isQuiver, "RewriteQuiver", "RewriteGraph"],
    DirectedEdges -> True,
    VertexNameFunction -> None
  ];
  (* there is a bug in LatticeGraph, see RewriteGraph[StringRewritingSystem[{"10" -> "01", "010" -> "100"}],
  "010"] // EdgeList, which reverses one edge and doesn't include a self-loop *)
  If[!isQuiver, result = RemoveEdgeTags @ result];
  cards = CardinalList[result];
  If[cards =!= None, result = ExtendedGraph[result, Cardinals -> Sort[cards]]];
  result
]

applyCanonicalizationFunction[f_][list_] :=
  DeleteDuplicatesBy[Map[applyCanon1[f], list], stripLabel];

applyCanon1[f_][Labeled[e_, l_]] := Labeled[f[e], l];
stripLabel[Labeled[e_, _]] := e;

(* TODO: fix Automatic options to inherit from the theme options *)

(**************************************************************************************************)

$RewriteQuiverThemeRules = {
  AspectRatioClipping -> False,
  ArrowheadSize -> 15,
  VertexSize -> 5, VertexFontSize -> 12,
  ImagePadding -> {Left -> 25, Right -> 25},
  ArrowheadPosition -> 0.45,
  ArrowheadShape -> "NarrowArrow",
  ImageSize -> ("ShortestEdge" -> 65),
  VertexLayout -> TreeVertexLayout[Orientation -> Left, Balanced -> True]
};

$RewriteGraphThemeRules = JoinOptions[
  ArrowheadStyle -> $Gray,
  ArrowheadShape -> "NarrowArrow",
  $RewriteQuiverThemeRules
];

$GraphThemeData["RewriteQuiver"] := $RewriteQuiverThemeRules;
$GraphThemeData["RewriteGraph"] := $RewriteGraphThemeRules;


