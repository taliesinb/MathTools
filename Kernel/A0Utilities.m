Package["GraphTools`"]


PackageImport["GeneralUtilities`"]


PackageScope["summaryItem"]

summaryItem[a_, b_] := BoxForm`SummaryItem[{a <> ": ", b}];


PackageScope["declareFormatting"]
PackageScope["$isTraditionalForm"]

declareFormatting[rules__RuleDelayed] := Scan[declareFormatting, {rules}];
declareFormatting[lhs_ :> rhs_] :=
  With[{head = First @ PatternHead[lhs]}, {isProtected = ProtectedFunctionQ[head]},
    If[isProtected, Unprotect[head]];
    Format[lhs, StandardForm] := Block[{$isTraditionalForm = False}, rhs];
    Format[lhs, TraditionalForm] := Block[{$isTraditionalForm = True}, rhs];
    If[isProtected, Protect[head]];
  ];

declareFormatting[___] := Panic["BadFormatting"]


PackageExport["FirstRest"]

FirstRest[list_] := {First @ list, Rest @ list};


PackageExport["AssociationRange"]

AssociationRange[list_] :=
  AssociationThread[list, Range @ Length @ list];


PackageExport["MinimumIndexBy"]

MinimumIndexBy[list_, f_] :=
  First @ Ordering[f /@ list, 1];


PackageExport["MinimumIndex"]

MinimumIndex[list_] :=
  First @ Ordering[list, 1];


PackageExport["MinimumBy"]

MinimumBy[list_, f_] :=
  Part[list, First @ Ordering[f /@ list, 1]];


PackageExport["FirstIndex"]

SetAttributes[FirstIndex, HoldRest];
FirstIndex[list_, pattern_, default_:None] :=
  First @ FirstPosition[list, pattern, {default}, 1, Heads -> False]


PackageScope["toListOfLists"]

toListOfLists = MatchValues[
  list:{__List} := list;
  list_List := {list};
  _ := $Failed;
];


PackageExport["MapStaggered"]

MapStaggered[f_, list_] := f @@@ Partition[list, 2, 1];


PackageExport["NegatedQ"]

NegatedQ[_Negated] = True;
NegatedQ[_] = False;


PackageExport["Negated"]

SetUsage @ "
Negated[elem$] represents the negation of elem$.
* Negated[a$ \[DirectedEdge] b$] represents the edge a$ \[DirectedEdge] b$ traversed in the reverse direction.
* DirectedEdge[a$, b$, Negated[c$]] evaluates to DirectedEdge[b$, a$, c$].
* Negated[Negated[c$]] evaluates to c$.
* Negated[c$] display as OverBar[c$].
"

Negated[Negated[e_]] := e;
Negated /: DirectedEdge[a_, b_, Negated[c_]] := DirectedEdge[b, a, c];

declareFormatting[
  Negated[e_] :> OverBar[e]
];


PackageScope["StripNegated"]

StripNegated[Negated[e_]] := e;
StripNegated[e_] := e;


PackageScope["ImageToGraphics"]

ImageToGraphics[img_, {xalign_, yalign_}, size_] := Scope[
  {w, h} = ImageDimensions[img];
  yrat = h / w;
  x = (xalign - 1)/2;
  y = yrat * (yalign - 1)/2;
  Graphics[
    {Opacity[1], Raster[Reverse[ImageData@img, {1}], {{x, y}, {x + 1, y + yrat}} * size]},
    ImageSize -> size, AspectRatio -> 1, PlotRangePadding -> None
  ]
];


PackageExport["LookupOption"]

LookupOption[obj_, opt_] := Quiet @ Lookup[Options[obj, opt], opt];


PackageExport["JoinOptions"]

JoinOptions[opts___] := DeleteDuplicatesBy[
  Join @@ Replace[{opts}, s_Symbol :> Options[s], {1}],
  First
];


PackageExport["DeleteOptions"]

DeleteOptions[opts_, keys_List] :=
  DeleteCases[opts, (Alternatives @@ keys) -> _];

DeleteOptions[opts_, key_] :=
  DeleteCases[opts, key -> _];


PackageExport["TakeOptions"]

TakeOptions[sym_Symbol, spec_] :=
  TakeOptions[Options[sym], spec];

TakeOptions[opts_List, keys_List] :=
  Cases[opts, Verbatim[Rule][Alternatives @@ keys, _]];

TakeOptions[opts_List, key_] :=
  Cases[opts, Verbatim[Rule][key, _]];


PackageExport["ReplaceOptions"]

ReplaceOptions[g_Graph, rule_Rule | rule_List] :=
  Graph[g, rule];

ReplaceOptions[obj_, key_ -> value_] := If[
  ContainsQ[obj, key -> _],
  Replace[obj, Rule[key, _] :> Rule[key, value], {1}],
  Append[obj, key -> value]
];

ReplaceOptions[obj_, rules_List] :=
  Fold[ReplaceOption, obj, rules];


PackageExport["UpdateOptions"]

UpdateOptions[obj_, option_, func_] :=
  ReplaceOptions[obj, option -> func[LookupOption[obj, option]]]


General::noobjprop = "There is no property named \"``\". Valid properties include: ``.";
General::noobjoptprop = "There is no property named \"``\" that accepts options. Such properties include: ``.";


PackageExport["HasAnnotationQ"]

HasAnnotationQ[obj_, key_] :=
  !MatchQ[AnnotationValue[obj, key], None | $Failed];


PackageExport["AttachAnnotation"]

AttachAnnotation[obj_, key_ -> None] :=
  If[AnnotationValue[obj, key] === $Failed, obj,
    AnnotationDelete[obj, key]];

AttachAnnotation[obj_, key_ -> value_] :=
  Annotate[obj, key -> value];

AttachAnnotation[obj_, rules_List] :=
  Fold[AttachAnnotation, obj, rules];



PackageScope["commaString"]

qs[s_String] := "\"" <> s <> "\"";
qs[e_] := e;
commaString[list_List] := TextString[Row[Map[qs, list], ", "]];


PackageScope["declareObjectPropertyDispatch"]
PackageScope["getObjectData"]
PackageScope["$SelfObject"]

getObjectData[_] := $Failed;

declareObjectPropertyDispatch[head_Symbol, dispatch_Symbol] := (
  getObjectData[head[data_Association] ? System`Private`NoEntryQ] := data;
  (obj:Blank[head] ? System`Private`NoEntryQ)[key_String, opts___Rule] := Block[{$SelfObject = obj},
    dispatch[getObjectData @ obj, key, opts]
  ];
  dispatch[data_, key_String] :=
    Lookup[data, key,
      Message[MessageName[head, "noobjprop"], key, commaString @ getValidProps[dispatch, data]];
      $Failed
    ];
  dispatch[data_, key_String, opts___Rule] := (
      Message[MessageName[head, "noobjoptprop"], key, commaString @ getValidOptProps[dispatch]];
      $Failed
    );
);

getValidProps[symbol_, data_] := Union[
  Cases[DownValues[symbol], HoldPattern[Verbatim[HoldPattern][symbol[_, key_String]] :> _] :> key],
  Keys @ data
];

getValidOptProps[symbol_] := Union @
  Cases[DownValues[symbol], HoldPattern[Verbatim[HoldPattern][symbol[_, key_String, __]] :> _] :> key];


PackageScope["declareFunctionAutocomplete"]

If[$Notebooks,

declareFunctionAutocomplete[function_Symbol, spec_] := With[
  {functionName = SymbolName[function]},
    FE`Evaluate[FEPrivate`AddSpecialArgCompletion[functionName -> spec]]
  ];
declareFunctionAutocomplete[___] := Panic["BadArgs"];

];


PackageScope["CheckQuiverArg"]
PackageScope["CheckGraphArg"]
PackageScope["CheckRepArg"]

General::notgroup = "`` is not a valid group.";
General::notquiver = "The `` argument should be a quiver graph.";
General::notgraph = "The `` argument should be a Graph.";
General::notrep = "The `` argument should be a group, RepresentationObject, QuiverRepresentationObject, or RootSystem.";

$numNames = <|1 -> "first", 2 -> "second", 3 -> "third", 4 -> "fourth"|>;

defineCheckArgMacro[macroName_Symbol, msgName_String, test_] :=
  DefineMacro[macroName,
    macroName[n_] := With[
      {nthArg = Part[$LHSPatternSymbols, n], numStr = $numNames[n]},
      Quoted @ Replace[test[nthArg], $Failed :> ReturnFailed[MessageName[$LHSHead, msgName], numStr]]
    ]
  ];

defineCheckArgMacro[CheckQuiverArg, "notquiver", ToQuiver];
defineCheckArgMacro[CheckGraphArg, "notgraph", ToGraph];
defineCheckArgMacro[CheckRepArg, "notrep", ToRepresentation];


PackageScope["UnpackOptionsAs"]

DefineMacro[UnpackOptionsAs,
UnpackOptionsAs[head_Symbol, opts_, syms___Symbol] := mUnpackOptionsAs[head, opts, {syms}]
];

SetHoldAllComplete[mUnpackOptionsAs];
mUnpackOptionsAs[head_, opts_, syms_] :=
  ToQuoted[Set, Quoted[syms],
    ToQuoted[OptionValue, head, List @ opts,
      GeneralUtilities`Control`PackagePrivate`capFirst /@ Map[HoldSymbolName, Unevaluated[syms]]
    ]
  ];


PackageScope["SetUsage"]

toUsageStr[list:{__String}] := commaString[list];
toUsageStr[e_] := TextString[e];

subsituteUsageSlots[s_String] :=
  StringReplace[s, "<*" ~~ Shortest[slot___] ~~ "*>" :> toUsageStr[ToExpression[slot, InputForm]]]

SetUsage[str_String] :=
  GeneralUtilities`SetUsage @ Evaluate @ FixedPoint[subsituteUsageSlots, str];