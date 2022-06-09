PackageExport["RewritingSystemObject"]

declareObjectPropertyDispatch[RewritingSystemObject, rewritingSystemProperty];

PackageScope["constructRewritingSystem"]
PackageScope["declareRewritingSystemDispatch"]
PackageScope["rewritingSystemProperty"]

PackageExport["CanonicalizationFunction"]

Options[RewritingSystemObject] = {
  CanonicalizationFunction -> None
};

constructRewritingSystem[type_, rules_, opts:OptionsPattern[RewritingSystemObject]] := Scope[
  rules //= ToList;
  If[!RuleListQ[rules /. TwoWayRule -> Rule], ReturnFailed[]];
  UnpackOptions[canonicalizationFunction];
  assoc = Association[
    "Type" -> type,
    "Rules" -> rules,
    "CanonicalizationFunction" -> canonicalizationFunction
  ];
  System`Private`ConstructNoEntry[RewritingSystemObject, assoc]
];

(**************************************************************************************************)

declareObjectPropertyDispatch[RewritingSystemObject, rewritingSystemProperty];

$rewritingSystemProperyDispatch = <||>;

rewritingSystemProperty[data_, args___] :=
  $rewritingSystemProperyDispatch[data["Type"]][data, args];

declareRewritingSystemDispatch[type_, dispatchFunction_] :=
  $rewritingSystemProperyDispatch[type] = dispatchFunction;

(**************************************************************************************************)

PackageExport["RewritingSystemObjectQ"]

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
     {summaryItem["Rules", Column @ rules]}
    },
    (* Displayed on request *)
    {},
    form,
    "Interpretable" -> Automatic
  ]
];

(**************************************************************************************************)

PackageExport["RewriteQuiver"]
PackageExport["RewriteGraph"]

Options[RewriteQuiver] = Options[RewriteGraph] = Options[LatticeQuiver];

applyCanonicalizationFunction[f_][list_] :=
  DeleteDuplicatesBy[Map[applyCanon1[f], list], stripLabel];

applyCanon1[f_][Labeled[e_, l_]] := Labeled[f[e], l];
stripLabel[Labeled[e_, _]] := e;

RewriteQuiver[system_RewritingSystemObject, initialState_, args___] :=
  rewriteGraphQuiver[system, initialState, True, args];

RewriteGraph[system_RewritingSystemObject, initialState_, args___] :=
  rewriteGraphQuiver[system, initialState, False, args];

rewriteGraphQuiver[system_, initialState_, isQuiver_, args___] := Scope[
  cayleyFunction = system["CayleyFunction", "Labeled" -> True (* isQuiver *)];
  canonFunction = system["CanonicalizationFunction"];
  If[canonFunction =!= None,
    cayleyFunction = cayleyFunction /* applyCanonicalizationFunction[canonFunction];
    initialState //= canonFunction
  ];
  result = LatticeQuiver[
    <|"CayleyFunction" -> cayleyFunction, "InitialStates" -> {initialState}|>, args,
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

(* TODO: fix Automatic options to inherit from the theme options *)

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


(**************************************************************************************************)

PackageExport["StripLabel"]

StripLabel[items:{___Labeled}] := Part[items, All, 1];
StripLabel[Labeled[e_, _]] := e;
StripLabel[e_] := e;



