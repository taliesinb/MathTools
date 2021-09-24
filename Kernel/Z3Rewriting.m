PackageExport["MatchData"]
PackageExport["TokenData"]

MatchData[a_, None] := MatchData[a];

(**************************************************************************************************)

PackageExport["RewritingSystemObject"]

declareObjectPropertyDispatch[RewritingSystemObject, rewritingSystemProperty];

constructRewritingSystsem[type_, rules_] := Scope[
	rules //= ToList;
	If[!RuleListQ[rules], ReturnFailed[]];
	assoc = Association[
		"Type" -> type,
		"Rules" -> rules
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

PackageExport["StringRewritingSystem"]

StringRewritingSystem[rules_] := Scope[
	constructRewritingSystsem["String", rules]
]

declareRewritingSystemDispatch["String", stringRewritingSystemProperty]

stringRewritingSystemProperty[data_, "CayleyFunction", opts___Rule] := Scope[
	UnpackAssociation[data, rules];
	UnpackStringOptions[{opts}, True, labeled];
	If[labeled,
		StringLabeledReplaceList[rules],
		StringReplaceListOperator[rules]
	]
];

(**************************************************************************************************)

PackageExport["StringReplaceListOperator"]

StringReplaceListOperator[rules_][str_] :=
	StringReplaceList[str, rules];

(**************************************************************************************************)

PackageExport["StringLabeledReplaceList"]

StringLabeledReplaceList[str_, rules_List] :=
	Catenate @ MapIndexed[
		StringLabeledReplaceList[str, #1, First @ #2]&,
		rules
	];

StringLabeledReplaceList[str_String, rule_, matchIndex_:None] := Scope[
	Map[
		span |-> Labeled[
			StringReplacePart[str, StringReplace[chunk = StringTake[str, span], rule], span],
			MatchData[
				Map[TokenData, Transpose @ {Characters @ chunk, Range @@ span}],
				matchIndex
			]
		],
		StringPosition[str, First @ rule]
	]
];

StringLabeledReplaceList[rule_][str_] := StringLabeledReplaceList[str, rule];
	
stringTokens[str_String, All] :=
	stringTokens[str, {1, StringLength @ str}];

stringTokens[str_String, span:{i_, j_}] :=
	Transpose[{
		Characters @ StringTake[str, span],
		Range[i, j]
	}];


(**************************************************************************************************)

PackageExport["PetriNet"]

PetriNet[rules_] := Scope[
	constructRewritingSystsem["PetriNet", rules]
]

(**************************************************************************************************)

PackageExport["RewriteQuiver"]
PackageExport["RewriteGraph"]

Options[RewriteQuiver] = Options[RewriteGraph] = Options[LatticeQuiver];

RewriteQuiver[system_RewritingSystemObject, initialState_, args___] :=
	rewriteGraphQuiver[system, initialState, True, args];

RewriteGraph[system_RewritingSystemObject, initialState_, args___] :=
	rewriteGraphQuiver[system, initialState, False, args];

rewriteGraphQuiver[system_, initialState_, isQuiver_, args___] := Scope[
	cayleyFunction = system["CayleyFunction", "Labeled" -> isQuiver];
	result = If[isQuiver, LatticeQuiver, LatticeGraph][
		<|"CayleyFunction" -> cayleyFunction, "InitialStates" -> {initialState}|>, args,
		GraphTheme -> If[isQuiver, "RewriteQuiver", "RewriteGraph"],
		DirectedEdges -> True,
		VertexNameFunction -> None
	];
	result
]

(* TODO: fix Automatic options to inherit from the theme options *)

$RewriteQuiverThemeRules = {
  AspectRatioClipping -> False,
  ArrowheadSize -> 15,
  VertexSize -> 5, VertexFontSize -> 12,
  ImagePadding -> {Left -> 25, Right -> 25},
  ArrowheadPosition -> 0.5,
  ArrowheadShape -> "NarrowArrow",
  ImageSize -> ("ShortestEdge" -> 65),
  GraphLayout -> "HorizontalCenteredTree"
};

$RewriteGraphThemeRules = JoinOptions[
	ArrowheadStyle -> $Gray,
	ArrowheadShape -> "NarrowArrow",
	$RewriteQuiverThemeRules
];

$GraphThemeData["RewriteQuiver"] := $RewriteQuiverThemeRules;
$GraphThemeData["RewriteGraph"] := $RewriteGraphThemeRules;
