PackageExport["Tau"]

Tau = 2 * Pi;

(**************************************************************************************************)

(* this takes the place of MatchValues in GU *)

PackageExport["Case"]

SetHoldAll[Case, setupCases];

Case /: (Set|SetDelayed)[sym_Symbol, Case[args___]] := setupCases[sym, args];

setupCases[sym_Symbol, CompoundExpression[args__SetDelayed, rewrites_List]] :=
  setupCases[sym, CompoundExpression[args], rewrites];

setupCases[sym_Symbol, CompoundExpression[args__SetDelayed, Null...], rewrites_:{}] := Module[{holds},
  Clear[sym];
  holds = Hold @@@ Hold[args];
  holds = ReplaceAll[holds, Unevaluated @ rewrites];
  PrependTo[holds, Hold[case_, UnmatchedCase[sym, case]]];
  holds = ReplaceAll[holds, HoldPattern[Out[]] :> sym];
  Replace[List @@ holds, Hold[a_, b_] :> SetDelayed[sym[a], b], {1}];
];

Case::baddef = "Bad case definition for ``."

setupCases[sym_, args___] := Message[Case::baddef, sym];

(**************************************************************************************************)

PackageScope["ToPacked"]

ToPacked = ToPackedArray;

PackageScope["ToPackedReal"]

ToPackedReal[e_] := ToPackedArray[e, Real];

PackageScope["ToPackedRealArrays"]

ToPackedRealArrays[array_ ? PackedArrayQ] := array;

ToPackedRealArrays[array_] := Scope[
  array = ToPackedReal[array];
  If[PackedArrayQ[array], array, Map[ToPackedRealArrays, array]]
];

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

(**************************************************************************************************)

(* fix a bug in IndexOf, which accidentally didn't limit itself to level one *)
With[{io := GeneralUtilities`IndexOf},
  Unprotect[io];
  If[FreeQ[DownValues[io], {1}],
    DownValues[io] = ReplaceAll[
      DownValues[io],
      HoldPattern[FirstPosition][a_, b_, c_, Heads -> False] :>
        FirstPosition[a, b, c, {1}, Heads -> False]
    ];
  ];
  Protect[io];
];

(**************************************************************************************************)

$slotRegularExpression = RegularExpression["<\\*([^*]+)\\*>"];

substituteUsageSlots[s_String] :=
  StringReplace[s, "<*" ~~ Shortest[slot___] ~~ "*>" :> Block[
    {$ContextPath = {"System`", "QuiverGeometry`", "QuiverGeometry`PackageScope`"}},
    toUsageStr[ToExpression[slot, InputForm]]
  ]];

toUsageStr[list:{__String}] := commaString[list];
toUsageStr[e_] := TextString[e];

(**************************************************************************************************)

$literalStringRegex = RegularExpression["'[A-Z][a-zA-Z0-9]+'"];
$literalStringColor = RGBColor[{0.4, 0.4, 0.4}];

$literalSymbolRegex = RegularExpression["(Automatic|True|False|None|Inherited|Left|Right|Above|Below|Center|Top|Bottom|Infinity|Tiny|Small|Medium|Large|Negated)"];
$literalSymbolColor = RGBColor[{0.15, 0.15, 0.15}];

$mainSymbolRegex = RegularExpression["^\\$?[A-Za-z][A-Za-z]*"];
$mainSymbolColor = RGBColor[{0.71, 0.03, 0.}];

colorLiterals[usageString_] := Scope[
  usageString //= StringTrim;
  $mainSymbol ^= First @ StringCases[usageString, $mainSymbolRegex, 1];
  StringReplace[
    usageString, {
      string:$literalStringRegex :> makeStyleBox[
        "\\\"" <> StringTake[string, {2, -2}] <> "\\\"",
        FontColor -> $literalStringColor, ShowStringCharacters -> True,
        FontWeight -> "Medium"],
      literal:$literalSymbolRegex :> makeStyleBox[
        literal,
        FontColor -> $literalSymbolColor,
        FontWeight -> "Medium"]
    }
  ]
];

colorMainSymbol[usageString_] := StringReplace[
  usageString, {
  ("\"" ~~ $mainSymbol ~~ "\"") :> StringTake[makeMainSymbolInlineSyntax[], {4, -2}],
  WordBoundary ~~ $mainSymbol ~~ WordBoundary :> makeMainSymbolInlineSyntax[],
  "<|" -> "\[LeftAssociation]",
  "|>" -> "\[RightAssociation]",
  "-StyleBox[\"" -> "StyleBox[\"-"
}];

makeMainSymbolInlineSyntax[] := makeStyleBox[$mainSymbol,
  FontColor -> $mainSymbolColor,
  FontWeight -> "Medium"
];

(**************************************************************************************************)

$optionSymbolColor = RGBColor[{0.086, 0.367, 0.615}];

colorOptionSymbols[usageString_] := StringReplace[
  usageString, {
    "%%" ~~ w:WordCharacter.. :>
      makeStyleBox[w, FontColor -> $literalSymbolColor, FontWeight -> "Medium"],
    "%" ~~ w:WordCharacter.. :>
      makeStyleBox[w, FontColor -> $optionSymbolColor, FontWeight -> "Medium"]
  }
];

makeStyleBox[str_, opts___] := StringJoin[
  "\!\(\*StyleBox[\"", str, "\", ", StringTake[ToString[{opts}, InputForm], {2, -2}], "]\)"
];

(**************************************************************************************************)

$headerLineRegex = RegularExpression["(?m)^## ([^\n]*)$"];

addHeaderLines[usageString_] := StringReplace[
  usageString, {
    $headerLineRegex :>
      addInlinePane @ makeStyleBox["$1", FontWeight -> "Bold"],
    "\n\n" :>
      "\!\(\*PaneBox[\"\", FrameMargins -> {{0,0}, {5, 0}}]\)\n"
  }
];

addInlinePane[str_String] := StringJoin[
  "\!\(\*PaneBox[",
  StringTake[str, {4, -3}],
  "], FrameMargins -> {{0, 0}, {1, 4}}]\)"
];

(**************************************************************************************************)

$gridBoxL = "\(\*TagBox[GridBox[";
$gridBoxR = ", \"Grid\"]\)";
$shorterGridBoxL = "\(\*PaneBox[GridBox[";
$shorterGridBoxR = ", Rule[ImageMargins, List[List[0,0], List[-5,-5]]], Rule[FrameMargins, List[List[0,0], List[-3,-6]]]]\)";
$shorterGridBoxR = ", Rule[ImageMargins, List[List[0,0], List[$BOT, $TOP]]]]\)";

shortenGridBoxes[usageString_] := StringReplace[
  usageString,
  $gridBoxL ~~ Shortest[content__] ~~ $gridBoxR :> Block[{offset, bot, top},
    offset = StringCount[content, "{\""];
    bot = TextString @ Round[ - 0.5*offset];
    top = TextString @ Round[ - 0.5*offset];
    $shorterGridBoxL <> content <> StringReplace[$shorterGridBoxR, {"$BOT" -> bot, "$TOP" -> top}]
  ]
];

(**************************************************************************************************)

$fmtUsageOuter = True;

PackageExport["ClearUsageCache"]

ClearUsageCache[] := Clear[GeneralUtilities`Private`$SetUsageFormatCache];

(* this speeds up the processing of usage string messages, which are otherwise quite expensive *)
If[!AssociationQ[GeneralUtilities`Private`$SetUsageFormatCache],
  GeneralUtilities`Private`$SetUsageFormatCache = Data`UnorderedAssociation[];
  GeneralUtilities`Code`PackagePrivate`fmtUsageString[str_String] /; $fmtUsageOuter := Block[
    {$fmtUsageOuter = False},
    GeneralUtilities`CacheTo[
      GeneralUtilities`Private`$SetUsageFormatCache, Hash[str],
      Compose[
        colorMainSymbol, addHeaderLines, shortenGridBoxes,
        GeneralUtilities`Code`PackagePrivate`fmtUsageString,
        colorOptionSymbols, colorLiterals, str
      ]
    ]
  ];
];

(**************************************************************************************************)

(* the default behavior of System`InformationDump` will introduce LineSpacing that messes up
my SetUsage inline tables, so remove it. *)

dummy::usage = "Dummy";
ToBoxes[Information[dummy]];
System`InformationDump`subtitleStyled[sub_] := Style[sub, "InformationUsageText"];

(**************************************************************************************************)

PackageScope["SetUsage"]

preprocessUsageString[usageString_] :=
  FixedPoint[substituteUsageSlots, usageString]

SetUsage[usageString_String] :=
  GeneralUtilities`SetUsage[Evaluate @ preprocessUsageString @ usageString];

SetUsage[symbol_Symbol, usageString_String] :=
  GeneralUtilities`SetUsage[symbol, Evaluate @ preprocessUsageString @ usageString];

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
    Format[$LHS:lhs, StandardForm] := Block[{$isTraditionalForm = False}, Interpretation[rhs, $LHS]];
    Format[$LHS:lhs, TraditionalForm] := Block[{$isTraditionalForm = True}, Interpretation[rhs, $LHS]];
    If[isProtected, Protect[head]];
  ];

declareFormatting[___] := Panic["BadFormatting"]

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

PackageExport["PlusVector"]

PlusVector[matrix_, 0|0.|{0.,0.}] := matrix;
PlusVector[matrix_, v_] := v + #& /@ matrix;
PlusVector[v_][matrix_] := PlusVector[matrix, v];

(**************************************************************************************************)

PackageExport["Lerp"]

Lerp[a_, b_, f_] := a * (1 - f) + b * f;

(**************************************************************************************************)

PackageExport["Interpolated"]

Interpolated[a_, b_, n_] := Table[b * i + a * (1 - i), {i, 0, 1, 1/(n-1)}];

(**************************************************************************************************)

PackageExport["AngleRange"]

AngleRange[a_, b_, Into[0]] := {};
AngleRange[a_, b_, Into[1]] := {Mod[(a + b), Tau] / 2};
AngleRange[a_, b_, Into[n_]] := NestList[PlusOperator[AngleDifference[a, b] / (n-1)], a, n-1];
AngleRange[a_, b_, da_] := AngleRange[a, b, Into[Ceiling[1 + Abs[AngleDifference[a, b]] / da]]];


PackageExport["AngleDifference"]

AngleDifference[a_, b_] := If[Abs[b - a] > Pi, Mod[Mod[b, Tau] - Mod[a, Tau], Tau, -Pi], b - a];

(**************************************************************************************************)

PackageExport["SameLengthQ"]

SameLengthQ[a_, b_] := Length[a] === Length[b];
SameLengthQ[a_][b_] := SameLengthQ[a, b];

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

PackageExport["PrependColumn"]

PrependColumn[matrix_, column_] := Transpose @ Prepend[Transpose @ matrix, column];
PrependColumn[column_][matrix_] := PrependColumn[matrix, column];

(**************************************************************************************************)

PackageExport["AppendColumn"]

AppendColumn[matrix_, column_] := Transpose @ Append[Transpose @ matrix, column];
AppendColumn[column_][matrix_] := AppendColumn[matrix, column];

(**************************************************************************************************)

PackageExport["FirstRest"]

FirstRest[list_] := {First @ list, Rest @ list};

(**************************************************************************************************)

PackageExport["FirstLast"]

FirstLast[list_] := {First @ list, Last @ list};

(**************************************************************************************************)

PackageExport["AssociationRange"]

AssociationRange[list_] :=
  AssociationThread[list, Range @ Length @ list];

(**************************************************************************************************)

PackageExport["RuleRange"]

RuleRange[labels_] :=
  MapIndexed[#1 -> First[#2]&, labels];

(**************************************************************************************************)

PackageExport["RuleThread"]

RuleThread[keys_, values_] :=
  MapThread[Rule, {keys, values}];

(**************************************************************************************************)

PackageExport["MinimumIndexBy"]

MinimumIndexBy[list_, f_] :=
  First @ Ordering[f /@ list, 1];

PackageExport["MaximumIndexBy"]

MaximumIndexBy[list_, f_] :=
  First @ Ordering[f /@ list, -1];

(**************************************************************************************************)

PackageExport["MinimumIndices"]

MinimumIndices[list_] :=
  MinimalBy[Range @ Length @ list, Part[list, #]&];

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

PackageExport["Minimum"]

Minimum[list_] :=
  Part[list, First @ Ordering[list, 1]];

PackageExport["Maximum"]

Maximum[list_] :=
  Part[list, First @ Ordering[list, -1]];

(**************************************************************************************************)

PackageExport["FirstIndex"]

SetAttributes[FirstIndex, HoldRest];
FirstIndex[list_, pattern_, default_:None] :=
  First @ FirstPosition[list, pattern, {default}, 1, Heads -> False]

(**************************************************************************************************)

PackageScope["toListOfLists"]

toListOfLists[list:{__List}] := list;
toListOfLists[list_List] := {list};
toListOfLists[_] := $Failed;

(**************************************************************************************************)

PackageExport["RangeQ"]

RangeQ[list_] := VectorQ[list, IntegerQ] && MinMax[list] == {1, Length @ list};

(**************************************************************************************************)

PackageExport["DropWhile"]

DropWhile[list_, f_] := Drop[list, LengthWhile[list, f]];

(**************************************************************************************************)

PackageExport["MapStaggered"]

MapStaggered[f_, list_] := f @@@ Partition[list, 2, 1];

(**************************************************************************************************)

(* add ability for PositionIndex to index at level 2 *)
Unprotect[PositionIndex];
PositionIndex[list_, 2] := Scope[
  assoc = <||>;
  ScanIndexed[
    {e, part} |-> KeyAppendTo[assoc, e, First @ part],
    list, {2}
  ];
  assoc
];
Protect[PositionIndex];

(**************************************************************************************************)

PackageExport["AtLeast"]

SetUsage @ "
AtLeast[n$] is a symbolic expression indicating that at least n$ values should be obtained.
"

declareFormatting[
  AtLeast[n_] :> Row[{">", n}]
];

(**************************************************************************************************)

PackageExport["ToInverseFunction"]

SetUsage @ "
ToInverseFunction[f$] returns %InverseFunction[f$].
* ToInverseFunction exists to enable a fast-path for QuiverGeometry-specific functions.
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
* %DirectedEdge[a$, b$, Negated[c$]] evaluates to %DirectedEdge[b$, a$, c$].
* Negated[Negated[c$]] evaluates to c$.
* Negated[c$] display as %OverBar[c$].
"

Negated[Negated[e_]] := e;
Negated /: DirectedEdge[a_, b_, Negated[c_]] := DirectedEdge[b, a, c];
Negated[CardinalSet[cards_]] := CardinalSet[Negated /@ cards];

declareBoxFormatting[
  Negated[e_] :> UnderNegatedBoxForm[e]
];

(**************************************************************************************************)

PackageExport["UnderNegatedForm"]

declareBoxFormatting[
  UnderNegatedForm[e_] :> UnderNegatedBoxForm[e]
];

SetHoldFirst[UnderNegatedBoxForm];
UnderNegatedBoxForm[e_] := UnderscriptBox[MakeBoxes @ e, "_"];

$db = 0.03;
$letterShifts = {
  "f" -> 0.05 + $db,
  "g" -> -0.4 + $db,
  "c" :>  -0.09,
  "h" | "i" -> 0.05 + $db, "j" -> -0.4 + $db, "k" -> 0.05 + $db,
  "m" | "n" -> 0.05 + $db, "p" | "q" -> -0.35, "r" -> 0.05 + $db,
  "v" | "w" | "x" | "z" -> 0.05 + $db,
  "y" -> -0.4 + $db,
  _ -> 0
};

UnderNegatedBoxForm[letter_String] := With[{shift = Replace[letter, $letterShifts]},
  UnderscriptBox[MakeBoxes @ letter, If[shift === 0, "_", AdjustmentBox["_", BoxBaselineShift -> shift]]]
];

(**************************************************************************************************)

PackageExport["NegatedForm"]

declareBoxFormatting[
  NegatedForm[e_] :> NegatedBoxForm[e]
];

SetHoldFirst[NegatedBoxForm];
NegatedBoxForm[e_String] := StyleBox[MakeBoxes @ e, Underlined];
NegatedBoxForm[e_] := OverscriptBox[MakeBoxes @ e, AdjustmentBox["_", BoxBaselineShift -> -0.3]]

(**************************************************************************************************)

PackageExport["StripNegated"]

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

DeleteOptions[key_][opts_] :=
  DeleteOptions[opts, key];

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
  MemberQ[obj, key -> _],
  Replace[obj, Rule[key, _] :> Rule[key, value], {1}],
  Append[obj, key -> value]
];

ReplaceOptions[obj_, rules_List] :=
  Fold[ReplaceOptions, obj, rules];

ReplaceOptions[rules_][obj_] := ReplaceOptions[obj, rules];

(**************************************************************************************************)

PackageExport["UpdateOptions"]

UpdateOptions[obj_, option_, func_] :=
  ReplaceOptions[obj, option -> func[LookupOption[obj, option]]];


General::noobjprop = "There is no property named \"``\". Valid properties include: ``.";
General::noobjoptprop = "There is no property named \"``\" that accepts options. Such properties include: ``.";

(**************************************************************************************************)

PackageExport["DeleteNull"]

DeleteNull[e_] := DeleteCases[e, Null];

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

PackageScope["GraphCachedScope"]
PackageExport["$GraphCacheStore"]

$GraphCacheStore = Language`NewExpressionStore["GraphCache"];

DefineMacro[GraphCachedScope,
GraphCachedScope[graph_, args___, body_] := mGraphCachedScope[graph, {$LHSHead, args}, body]
];

SetHoldAllComplete[mGraphCachedScope];

mGraphCachedScope[graph_, key_, body_] := With[{body2 = MacroExpand @ Scope @ body},
  Quoted @ Module[
    {$cacheTemp$ = $GraphCacheStore["get"[graph, key]]},
    If[$cacheTemp$ === Null,
      $cacheTemp$ = body2;
      If[!FailureQ[$cacheTemp$], $GraphCacheStore["put"[graph, key, $cacheTemp$]]];
    ];
    $cacheTemp$
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
