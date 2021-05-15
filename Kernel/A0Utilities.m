Package["GraphTools`"]


PackageImport["GeneralUtilities`"]


(**************************************************************************************************)

PackageExport["Tau"]

Tau = 2 * Pi;

(**************************************************************************************************)

PackageScope["ToPacked"]

ToPacked = Developer`ToPackedArray;

PackageScope["ToPackedReal"]

ToPackedReal[e_] := Developer`ToPackedArray[e, Real];

PackageScope["ToPackedRealArrays"]

ToPackedRealArrays[array_ ? Developer`PackedArrayQ] := array;

ToPackedRealArrays[array_] := Scope[
  array = ToPackedReal[array];
  If[Developer`PackedArrayQ[array], array, Map[ToPackedRealArrays, array]]
];

(**************************************************************************************************)

PackageScope["summaryItem"]

summaryItem[a_, b_] := BoxForm`SummaryItem[{a <> ": ", b}];

(**************************************************************************************************)

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

(**************************************************************************************************)

PackageExport["OrList"]
PackageExport["AndList"]

OrList = Apply[Or];
AndList = Apply[And];

(**************************************************************************************************)

PackageScope["$posIntOrInfinityP"]

$posIntOrInfinityP = _Integer ? Positive | Infinity;

(**************************************************************************************************)

PackageScope["declareBoxFormatting"]

declareBoxFormatting[rules__RuleDelayed] := Scan[declareBoxFormatting, {rules}];
declareBoxFormatting[lhs_ :> rhs_] :=
  With[{head = First @ PatternHead[lhs]}, {isProtected = ProtectedFunctionQ[head]},
    If[isProtected, Unprotect[head]];
    MakeBoxes[lhs, StandardForm] := Block[{$isTraditionalForm = False}, rhs];
    MakeBoxes[lhs, TraditionalForm] := Block[{$isTraditionalForm = True}, rhs];
    If[isProtected, Protect[head]];
  ];

declareBoxFormatting[___] := Panic["BadFormatting"]

(**************************************************************************************************)

PackageExport["Interpolated"]

Interpolated[a_, b_, n_] := Table[b * i + a * (1 - i), {i, 0, 1, 1/(n-1)}];

(**************************************************************************************************)

PackageExport["AngleInterpolated"]

AngleInterpolated[a_, b_, n_] := NestList[PlusOperator[angleDelta[a, b] / (n-1)], a, n-1];
angleDelta[a_, b_] := If[Abs[b - a] > Pi, Mod[b, 2 Pi] - Mod[a, 2 Pi], b - a];

(**************************************************************************************************)

PackageExport["RealVectorQ"]

RealVectorQ[list_] := VectorQ[list, Internal`RealValuedNumberQ];

(**************************************************************************************************)

PackageExport["RealMatrixQ"]

RealMatrixQ[list_] := MatrixQ[list, Internal`RealValuedNumberQ];

(**************************************************************************************************)

PackageExport["ComplexVectorQ"]

ComplexVectorQ[list_] := VectorQ[list, NumericQ] && !FreeQ[list, Complex];

(**************************************************************************************************)

PackageExport["ContainsComplexQ"]

ContainsComplexQ[expr_] := !FreeQ[expr, Complex];

(**************************************************************************************************)

PackageExport["ContainsNegativeQ"]

ContainsNegativeQ[expr_] := !FreeQ[expr, n_Real | n_Rational | n_Integer ? Negative];

(**************************************************************************************************)

PackageExport["ArrayLabelIndices"]

ArrayLabelIndices[array_, labels_] :=
  Replace[array, RuleRange @ labels, {1}];

ArrayLabelIndices[array_, labels_, level_] :=
  Replace[array, RuleRange @ labels, List[level]];

(**************************************************************************************************)

PackageExport["ArrayLabeling"]

ArrayLabeling[array_, level_:1] := Scope[
  assoc = <||>;
  List[
    Map[
      e |-> Lookup[assoc, Key @ e, assoc[e] = Length[assoc] + 1],
      array, {level}
    ],
    assoc
  ]
];

ExtractIndices[array_, indices_ /; VectorQ[indices, Internal`NonNegativeMachineIntegerQ]] :=
  Part[array, indices];

ExtractIndices[array_, indices_List] := Map[Part[array, #]&, indices, {-1}]

(**************************************************************************************************)

PackageExport["FirstColumn"]

FirstColumn[matrix_] := Part[matrix, All, 1];

(**************************************************************************************************)

PackageExport["LastColumn"]

LastColumn[matrix_] := Part[matrix, All, -1];

(**************************************************************************************************)

PackageExport["MostColumns"]

MostColumns[matrix_] := Part[matrix, All, All ;; -2];

(**************************************************************************************************)

PackageExport["RestColumns"]

RestColumns[matrix_] := Part[matrix, All, 2 ;; All];

(**************************************************************************************************)

PackageExport["FirstRest"]

FirstRest[list_] := {First @ list, Rest @ list};

(**************************************************************************************************)

PackageExport["AssociationRange"]

AssociationRange[list_] :=
  AssociationThread[list, Range @ Length @ list];

(**************************************************************************************************)

PackageExport["RuleRange"]

RuleRange[labels_] := MapIndexed[#1 -> First[#2]&, labels];

(**************************************************************************************************)

PackageExport["MinimumIndexBy"]

MinimumIndexBy[list_, f_] :=
  First @ Ordering[f /@ list, 1];

PackageExport["MaximumIndexBy"]

MaximumIndexBy[list_, f_] :=
  First @ Ordering[f /@ list, -1];

(**************************************************************************************************)

PackageExport["MinimumIndex"]

MinimumIndex[list_] :=
  First @ Ordering[list, 1];

PackageExport["MaximumIndex"]

MaximumIndex[list_] :=
  First @ Ordering[list, -1];

(**************************************************************************************************)

PackageExport["MinimumBy"]

MinimumBy[list_, f_] :=
  Part[list, First @ Ordering[f /@ list, 1]];

PackageExport["MaximumBy"]

MaximumBy[list_, f_] :=
  Part[list, First @ Ordering[f /@ list, -1]];

(**************************************************************************************************)

PackageExport["FirstIndex"]

SetAttributes[FirstIndex, HoldRest];
FirstIndex[list_, pattern_, default_:None] :=
  First @ FirstPosition[list, pattern, {default}, 1, Heads -> False]

(**************************************************************************************************)

PackageScope["toListOfLists"]

toListOfLists = MatchValues[
  list:{__List} := list;
  list_List := {list};
  _ := $Failed;
];

(**************************************************************************************************)

PackageExport["RangeQ"]

RangeQ[list_] := VectorQ[list, IntegerQ] && MinMax[list] == {1, Length @ list};

(**************************************************************************************************)

PackageExport["MapStaggered"]

MapStaggered[f_, list_] := f @@@ Partition[list, 2, 1];

(**************************************************************************************************)

PackageExport["AtLeast"]

SetUsage @ "
AtLeast[n$] is a symbolic expression indicating that at least n$ values should be obtained.
"

(**************************************************************************************************)

PackageExport["ToInverseFunction"]

SetUsage @ "
ToInverseFunction[f$] returns InverseFunction[f$].
* ToInverseFunction exists to enable a fast-path for GraphTools-specific functions.
"

ToInverseFunction[e_] := InverseFunction[e];

(**************************************************************************************************)

PackageExport["NegatedQ"]

NegatedQ[_Negated] = True;
NegatedQ[_] = False;

(**************************************************************************************************)

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
  Negated[e_] :> NegatedForm[e]
];

(**************************************************************************************************)

PackageExport["NegatedForm"]

declareBoxFormatting[
  NegatedForm[e_] :> OverscriptBox[ToBoxes @ e, "_"]
];

(**************************************************************************************************)

PackageScope["StripNegated"]

StripNegated[Negated[e_]] := e;
StripNegated[e_] := e;

(**************************************************************************************************)

PackageExport["LookupOption"]

LookupOption[obj_, opt_, default_:Automatic] :=
  Quiet @ Lookup[Options[obj, opt], opt, default];

(**************************************************************************************************)

PackageExport["JoinOptions"]

JoinOptions[opts___] := DeleteDuplicatesBy[
  Join @@ Replace[{opts}, s_Symbol :> Options[s], {1}],
  First
];

(**************************************************************************************************)

PackageExport["DeleteOptions"]

DeleteOptions[opts_, keys_List] :=
  DeleteCases[opts, (Alternatives @@ keys) -> _];

DeleteOptions[opts_, key_] :=
  DeleteCases[opts, key -> _];

(**************************************************************************************************)

PackageExport["TakeOptions"]

TakeOptions[sym_Symbol, spec_] :=
  TakeOptions[Options[sym], spec];

TakeOptions[opts_List, keys_List] :=
  Cases[opts, Verbatim[Rule][Alternatives @@ keys, _]];

TakeOptions[opts_List, key_] :=
  Cases[opts, Verbatim[Rule][key, _]];

(**************************************************************************************************)

PackageExport["ReplaceOptions"]

ReplaceOptions[g_Graph, rule_Rule | rule_List] :=
  Graph[g, rule];

ReplaceOptions[obj_, key_ -> value_] := If[
  ContainsQ[obj, key -> _],
  Replace[obj, Rule[key, _] :> Rule[key, value], {1}],
  Append[obj, key -> value]
];

ReplaceOptions[obj_, rules_List] :=
  Fold[ReplaceOptions, obj, rules];

ReplaceOptions[rules_][obj_] := ReplaceOptions[obj, rules];

(**************************************************************************************************)

PackageExport["UpdateOptions"]

UpdateOptions[obj_, option_, func_] :=
  ReplaceOptions[obj, option -> func[LookupOption[obj, option]]]


General::noobjprop = "There is no property named \"``\". Valid properties include: ``.";
General::noobjoptprop = "There is no property named \"``\" that accepts options. Such properties include: ``.";

(**************************************************************************************************)

PackageExport["LookupAnnotation"]

SetHoldRest[LookupAnnotation];

LookupAnnotation[obj_, key_, default_:Automatic] :=
  Replace[AnnotationValue[obj, key], $Failed :> default];

LookupAnnotation[obj_, key_List, default_:Automatic] :=
  Replace[AnnotationValue[obj, key], $Failed :> default, {1}];

(**************************************************************************************************)

PackageScope["commaString"]

qs[s_String] := "\"" <> s <> "\"";
qs[e_] := e;
commaString[list_List] := TextString[Row[Map[qs, list], ", "]];

(**************************************************************************************************)

PackageScope["declareObjectPropertyDispatch"]
PackageScope["getObjectData"]
PackageScope["$SelfObject"]

getObjectData[_] := $Failed;

declareObjectPropertyDispatch[head_Symbol, dispatch_Symbol] := (
  getObjectData[head[data_Association] ? System`Private`NoEntryQ] := data;
  (obj:Blank[head] ? System`Private`NoEntryQ)[key_String, opts___Rule] := Block[{$SelfObject = obj},
    dispatch[getObjectData @ obj, key, opts]
  ];
  dispatch[data_, key_String] := Block[{res = Lookup[data, key, $Failed]}, res /; res =!= $Failed];
  dispatch[args___] := failDispatch[head, dispatch][args];
);

failDispatch[head_, dispatch_][data_, key_String] :=
  Message[MessageName[head, "noobjprop"], key, commaString @ getValidProps[dispatch, data]];

failDispatch[head_, dispatch_][data_, key_String, __Rule] :=
  Message[MessageName[head, "noobjoptprop"], key, commaString @ getValidOptProps @ dispatch];

getValidProps[symbol_, data_] := Union[
  Cases[DownValues[symbol], HoldPattern[Verbatim[HoldPattern][symbol[_, key_String]] :> _] :> key],
  Keys @ data
];

getValidOptProps[symbol_] := getValidOptProps[symbol] =
  Union @ Cases[DownValues[symbol], HoldPattern[Verbatim[HoldPattern][symbol[_, key_String, __]] :> _] :> key];

(**************************************************************************************************)

PackageScope["declareFunctionAutocomplete"]
PackageScope["declareSyntaxInfo"]

If[$Notebooks,

declareFunctionAutocomplete[function_Symbol, spec_] := With[
  {functionName = SymbolName[function]},
    FE`Evaluate[FEPrivate`AddSpecialArgCompletion[functionName -> spec]]
  ];
declareFunctionAutocomplete[___] := Panic["BadArgs"];

toOptionName[sym_Symbol] := SymbolName[sym];
toOptionName[str_String] := str;

declareSyntaxInfo[function_Symbol, argPatterns_List] := Scope[
  info = {"ArgumentsPattern" -> argPatterns};
  If[ContainsQ[argPatterns, Verbatim[OptionsPattern[]]],
    AppendTo[info, "OptionNames" -> Map[toOptionName, Keys @ Options @ function]]];
  SyntaxInformation[function] = info;
];

];

(**************************************************************************************************)

$numNames = <|1 -> "first", 2 -> "second", 3 -> "third", 4 -> "fourth"|>;

defineCheckArgMacro[checkMacro_Symbol, checker_, msgName_String] := DefineMacro[coerceMacro,
  coerceMacro[n_] := With[
    {nthArg = Part[$LHSPatternSymbols, n], numStr = $numNames[n]},
    Quoted @ Replace[coercer[nthArg], $Failed :> ReturnFailed[MessageName[$LHSHead, msgName], numStr]]
  ]
];

defineCoerceArgMacro[coerceMacro_Symbol, coercer_, msgName_String] := DefineMacro[coerceMacro,
  coerceMacro[n_] := With[
    {nthArg = Part[$LHSPatternSymbols, n], numStr = $numNames[n]},
    Quoted @ Replace[coercer[nthArg], $Failed :> ReturnFailed[MessageName[$LHSHead, msgName], numStr]]
  ]
];

_defineCheckArgMacro := Panic["BadArgMacro"];
_defineCoerceArgMacro := Panic["BadArgMacro"];

(**************************************************************************************************)

PackageScope["CheckIsQuiver"]
PackageScope["CoerceToQuiver"]

General::notquiver = "The `` argument should be a quiver.";
General::notquiverc = "The `` argument should be a quiver or list of quiver edges.";
defineCheckArgMacro[CheckIsQuiver, QuiverQ, "notquiver"];
defineCoerceArgMacro[CoerceToQuiver, ToQuiver, "notquiverc"];


PackageScope["CheckIsGraph"]
PackageScope["CoerceToGraph"]

General::notgraph = "The `` argument should be a Graph.";
General::notgraphc = "The `` argument should be a Graph or list of edges.";
defineCheckArgMacro[CheckIsGraph, GraphQ, "notgraph"];
defineCoerceArgMacro[CoerceToGraph, ToGraph, "notgraphc"];


PackageScope["CheckIsRep"]
PackageScope["CoerceToRep"]

General::notrep = "The `` argument should be a RepresentationObject.";
General::notrepc = "The `` argument should be a group, groupoid, RepresentationObject, QuiverRepresentationObject, or RootSystem.";
defineCheckArgMacro[CheckIsRep, RepresentationObjectQ, "notrep"];
defineCoerceArgMacro[CoerceToRep, ToRepresentation, "notrepc"];


PackageScope["CheckIsGroup"]

General::notgroup = "`` is not a valid group.";
defineCheckArgMacro[CheckIsGroup, GroupQ, "notgroup"];


PackageScope["CheckIsGraphics"]

General::notgraphics = "The `` argument should be a Graphics or Graphics3D expression."
defineCheckArgMacro[CheckIsGraphics, GraphicsQ, "notgraphics"];

(**************************************************************************************************)

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

(**************************************************************************************************)

PackageScope["FunctionSection"]

DefineMacro[FunctionSection,
FunctionSection[expr_] := Quoted[expr]
];

(**************************************************************************************************)

PackageScope["SetAutomatic"]
PackageScope["SetMissing"]
PackageScope["SetNone"]
PackageScope["SetAll"]
PackageScope["SetInherited"]

DefineLiteralMacro[SetAutomatic, SetAutomatic[lhs_, rhs_] := If[lhs === Automatic, lhs = rhs, lhs]];
DefineLiteralMacro[SetMissing, SetMissing[lhs_, rhs_] := If[MissingQ[lhs], lhs = rhs, lhs]];
DefineLiteralMacro[SetNone, SetNone[lhs_, rhs_] := If[lhs === None, lhs = rhs, lhs]];
DefineLiteralMacro[SetAll, SetAll[lhs_, rhs_] := If[lhs === All, lhs = rhs, lhs]];
DefineLiteralMacro[SetInherited, SetInherited[lhs_, rhs_] := If[lhs === Inherited, lhs = rhs, lhs]];

SetHoldAll[SetAutomatic, SetMissing, SetNone, SetAll, SetInherited];

(**************************************************************************************************)

PackageScope["SetUsage"]

toUsageStr[list:{__String}] := commaString[list];
toUsageStr[e_] := TextString[e];

substituteUsageSlots[s_String] :=
  StringReplace[s, "<*" ~~ Shortest[slot___] ~~ "*>" :> toUsageStr[ToExpression[slot, InputForm]]]

SetUsage[str_String] :=
  GeneralUtilities`SetUsage @ Evaluate @ FixedPoint[substituteUsageSlots, str];

(**************************************************************************************************)

With[{fmv := GeneralUtilities`Control`PackagePrivate`findMutatedVariables},
  If[FreeQ[DownValues[fmv], ApplyTo],
    DownValues[fmv] = Insert[
      DownValues[fmv],
      Unevaluated @ ApplyTo[GeneralUtilities`Control`PackagePrivate`lhs_Symbol, _],
      {1, 2, 1, 1, 2, 1, 1, 2}
    ]
  ];
];