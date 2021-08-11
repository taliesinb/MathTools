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

PackageScope["$literalSymbolRegex"]
$literalSymbolStr = "\\$Failed Automatic True False None Inherited Left Right Above Below Center \
Top Bottom Infinity Tiny Small Medium Large Negated Into";
$literalSymbolRegex = RegularExpression["(" <> StringReplace[$literalSymbolStr, " " -> "|"] <> ")"];
$literalSymbolColor = RGBColor[{0.15, 0.15, 0.15}];

PackageScope["$mainSymbolRegex"]

$mainSymbolRegex = RegularExpression["^\\$?[A-Za-z][A-Za-z]*"];
$mainSymbolColor = RGBColor[{0.71, 0.03, 0.}];

colorLiterals[usageString_] := Scope[
  usageString //= StringTrim;
  StringReplace[
    usageString, {
      string:$literalStringRegex :> makeStyleBox[
        "\\\"" <> StringTake[string, {2, -2}] <> "\\\"",
        FontColor -> $literalStringColor, ShowStringCharacters -> True,
        FontWeight -> "Medium"],
      WordBoundary ~~ literal:$literalSymbolRegex ~~ WordBoundary :> makeLiteralSymbolBox[literal]
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

$otherSymbolColor = RGBColor[{0.086, 0.367, 0.615}];

colorOtherSymbols[usageString_] := StringReplace[
  usageString, {
    "%%" ~~ w:WordCharacter.. :> makeLiteralSymbolBox[w],
    "%" ~~ w:WordCharacter.. :> makeOtherSymbolBox[w]
  }
];

makeLiteralSymbolBox[w_] := makeStyleBox[w, FontColor -> $literalSymbolColor, FontWeight -> "Medium"];
makeOtherSymbolBox[w_] := makeStyleBox[w, FontColor -> $otherSymbolColor, FontWeight -> "Medium"];

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

ClearUsageCache[] := (
  Clear[GeneralUtilities`Private`$SetUsageFormatCache];
  $RawUsageStringTable = Association[];
  GeneralUtilities`Code`PackagePrivate`$relatedSymbolTable = Data`UnorderedAssociation[];
);

(* this speeds up the processing of usage string messages, which are otherwise quite expensive *)
If[!AssociationQ[GeneralUtilities`Private`$SetUsageFormatCache],
  GeneralUtilities`Private`$SetUsageFormatCache = Data`UnorderedAssociation[];
  GeneralUtilities`Code`PackagePrivate`fmtUsageString[str_String] /; $fmtUsageOuter := Block[
    {$fmtUsageOuter = False},
    storeRawUsageString[str];
    GeneralUtilities`CacheTo[
      GeneralUtilities`Private`$SetUsageFormatCache, Hash[str],
      Compose[customSetUsageProcessor, str]
    ]
  ];
  With[{arn := GeneralUtilities`Code`PackagePrivate`appendRelatedNote},
    If[FreeQ[DownValues[arn], makeOtherSymbolBox],
      DownValues[arn] = ReplaceAll[
        DownValues[arn], HoldPattern[Riffle[z_, ", "]] :> Riffle[makeOtherSymbolBox /@ z, ", "]
      ]
    ];
  ];
];

customSetUsageProcessor = Composition[
  colorMainSymbol,
  addHeaderLines, shortenGridBoxes,
  GeneralUtilities`Code`PackagePrivate`fmtUsageString,
  colorOtherSymbols, colorLiterals
];

(**************************************************************************************************)

PackageScope["$RawUsageStringTable"]

$RawUsageStringTable = Association[];

storeRawUsageString[rawUsageString_String] := Block[
  {usageString = StringTrim @ rawUsageString},
  (* $mainSymbol will be picked up later in the customSetUsageProcessor composition chain *)
  $mainSymbol = First @ StringCases[usageString, $mainSymbolRegex, 1];
  $RawUsageStringTable[$mainSymbol] = usageString;
];

(**************************************************************************************************)

(* the default behavior of System`InformationDump` will introduce LineSpacing that messes up
my SetUsage inline tables, so remove it. *)

dummy::usage = "Dummy";
ToBoxes[Information[dummy]];
System`InformationDump`subtitleStyled[sub_] := Style[sub, "InformationUsageText"];

(**************************************************************************************************)

PackageScope["SetUsage"]

PackageExport["$DisableSetUsage"]

$DisableSetUsage = True;

preprocessUsageString[usageString_] :=
  FixedPoint[substituteUsageSlots, usageString]

SetUsage[___] /; $DisableSetUsage := Null;

SetUsage[usageString_String] :=
  GeneralUtilities`SetUsage[Evaluate @ preprocessUsageString @ usageString];

SetUsage[symbol_Symbol, usageString_String] :=
  GeneralUtilities`SetUsage[symbol, Evaluate @ preprocessUsageString @ usageString];

(**************************************************************************************************)

PackageExport["Tau"]

SetUsage @ "
Tau is an alias for 2 * %Pi.
"

Tau = 2 * Pi;

(**************************************************************************************************)

PackageExport["RandomSeeded"]

SetUsage @ "
RandomSeeded[body$, seeding$] evaluates body$ with %%RandomSeeding -> seeding$.
* seeding$ of Automatic does not localize the RNG when evaluating body$.
"

SetHoldFirst[RandomSeeded];

RandomSeeded[body_, Automatic] := body;
RandomSeeded[body_, other_] := BlockRandom[body, RandomSeeding -> other];

(**************************************************************************************************)

PackageExport["OnFailed"]

SetUsage @ "
OnFailed[expr$, body$] evaluates and returns body$ if expr$ is $Failed, otherwise returns expr$.
"

SetHoldRest[OnFailed];

OnFailed[$Failed, e_] := e;
OnFailed[e_, _] := e;

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
  holds = ReplaceAll[holds, procRewrites @ rewrites];
  PrependTo[holds, Hold[case_, UnmatchedCase[sym, case]]];
  holds = ReplaceAll[holds, HoldPattern[Out[]] :> sym];
  Replace[List @@ holds, Hold[a_, b_] :> SetDelayed[sym[a], b], {1}];
];

Case::baddef = "Bad case definition for ``."

setupCases[sym_, args___] := Message[Case::baddef, sym];

SetHoldAllComplete[procRewrites];
procRewrites[l_List] := Map[procRewrites, Unevaluated @ l];
procRewrites[a_ -> b_] := HoldPattern[a] -> b;
procRewrites[a_ :> b_] := HoldPattern[a] :> b;

SetUsage @ "
Case[rules$$] is a macro for defining functions of one variable, specifying LHS and RHS rules for the argument.
Case[rules$$, {alias$1, alias$2, $$}] applies temporary aliases to the rules$ before evaluation.
* Use the form func$ = Case[$$] to attach the rules to the function func$.
* Each of the rules should be of the form patt$ :> body$, and should be seperated by semicolons.
* The aliases can be used to transform the rules before they attached used as definitions.
* Use \[Rule] in an alias to have the RHS of the alias evaluate, and \[RuleDelayed] to perform a literal replacement.
* Aliases can be thought of as 'local macros' that make a particular function definition cleaner or more concise.
"

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

PackageScope["summaryItem"]

summaryItem[a_, b_] := BoxForm`SummaryItem[{a <> ": ", b}];

(**************************************************************************************************)

PackageScope["declareFormatting"]
PackageScope["$isTraditionalForm"]

getPatternHead[sym_Symbol] := sym;
getPatternHead[expr_] := First @ PatternHead @ expr;

declareFormatting[rules__RuleDelayed] := Scan[declareFormatting, {rules}];
declareFormatting[lhs_ :> rhs_] :=
  With[{head = getPatternHead[lhs]}, {isProtected = ProtectedFunctionQ[head]},
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

SetHoldAllComplete[getPatternHead];

declareBoxFormatting[rules__RuleDelayed] := Scan[declareBoxFormatting, {rules}];
declareBoxFormatting[lhs_ :> rhs_] :=
  With[{head = getPatternHead[lhs]}, {isProtected = ProtectedFunctionQ[head]},
    If[isProtected, Unprotect[head]];
    MakeBoxes[lhs, StandardForm] := Block[{$isTraditionalForm = False}, rhs];
    MakeBoxes[lhs, TraditionalForm] := Block[{$isTraditionalForm = True}, rhs];
    If[isProtected, Protect[head]];
  ];

declareBoxFormatting[___] := Panic["BadFormatting"]

(**************************************************************************************************)

PackageExport["PlusVector"]

SetUsage @ "
PlusVector[matrix$, vector$] adds vector$ to each row vector of matrix$.
PlusVector[vector$] is an operator form of PlusVector.
* PlusVector is useful because normally matrix$ + vector$ adds vector$ column-wise to matrix$ via Listability.
"

PlusVector[matrix_, 0|0.|{0.,0.}] := matrix;
PlusVector[matrix_, v_] := v + #& /@ matrix;
PlusVector[v_][matrix_] := PlusVector[matrix, v];

(**************************************************************************************************)

PackageExport["Lerp"]

SetUsage @ "
Lerp[a$, b$, f$] linearly interpolates between a$ and b$, where f$ = 0 gives a$ and f$ = 1 gives b$.
Lerp[a$, b$, {f$1, f$2, $$}] gives a list of interpolations.
Lerp[a$, b$, Into[n$]] gives the n$ values interpolated between a$ and b$.
Lerp[f$] is the operator form of Lerp$.
* a$ and b$ can be numbers, arrays, etc.
"

Lerp[a_, b_, f_] := a * (1 - f) + b * f;
Lerp[a_, b_, f_List] := Lerp[a, b, #]& /@ f;

Lerp[a_, b_, Into[0]] := {};
Lerp[a_, b_, Into[1]] := (a + b) / 2;
Lerp[a_, b_, Into[2]] := {a, b};
Lerp[a_, b_, Into[n_]] := Lerp[a, b, Range[0, 1, 1/(n-1)]]

Lerp[n_][a_, b_] := Lerp[a, b, n];

(**************************************************************************************************)

PackageExport["Interpolated"]

SetUsage @ "
Interpolated[a$, b$, n$] is equivalent to %Lerp[a$, b$, Into[n$]].
"

Interpolated[a_, b_, n_] := Table[b * i + a * (1 - i), {i, 0, 1, 1/(n-1)}];

(**************************************************************************************************)

PackageExport["AngleRange"]

SetRelatedSymbolGroup[AngleRange, AngleDifference];

SetUsage @ "
AngleRange[a$, b$, Into[n$]] gives n$ angles between a$ and b$.
* The angles are chosen in the direction that yields the shortest distance modulo %%Tau.
* All values are given modulo %%Tau.
"

AngleRange[a_, b_, Into[0]] := {};
AngleRange[a_, b_, Into[1]] := {Mod[(a + b), Tau] / 2};
AngleRange[a_, b_, Into[n_]] := NestList[PlusOperator[AngleDifference[a, b] / (n-1)], a, n-1];
AngleRange[a_, b_, da_] := AngleRange[a, b, Into[Ceiling[1 + Abs[AngleDifference[a, b]] / da]]];


PackageExport["AngleDifference"]

SetUsage @ "
AngleDifference[a$, b$, Into[n$]] gives the signed distance between two angles a$ and b$.
* This is the smallest difference between a$ and b$ modulo %%Tau.
"

AngleDifference[a_, b_] := If[Abs[b - a] > Pi, Mod[Mod[b, Tau] - Mod[a, Tau], Tau, -Pi], b - a];

(**************************************************************************************************)

PackageExport["$RulePattern"]

$RulePattern = _Rule | _RuleDelayed; 

(**************************************************************************************************)

PackageExport["$RuleListPattern"]

$RuleListPattern = {RepeatedNull[_Rule | _RuleDelayed]};

(**************************************************************************************************)

PackageExport["RuleListQ"]

RuleListQ[$RuleListPattern] := True;
RuleListQ[_] := False;

(**************************************************************************************************)

PackageExport["SameLengthQ"]

SetUsage @ "
SameLengthQ[a$, b$] gives True if %Length[a$] === %Length[b$].
"

SameLengthQ[a_, b_] := Length[a] === Length[b];
SameLengthQ[a_][b_] := SameLengthQ[a, b];

(**************************************************************************************************)

PackageExport["RealVectorQ"]

SetUsage @ "
RealVectorQ[list$] gives True if list$ is a vector of real-valued numbers.
* Integers, rationals, etc. are considered real-valued numbers.
"

RealVectorQ[list_] := VectorQ[list, Internal`RealValuedNumberQ];

(**************************************************************************************************)

PackageExport["UnitIntervalArrayQ"]

SetUsage @ "
UnitIntervalArrayQ[arr$] gives True if arr$ is an array whose values are between 0 and 1 inclusive.
"

UnitIntervalArrayQ[arr_] := Scope[
  {min, max} = MinMax @ arr;
  TrueQ[0 <= min <= max <= 1]
];

(**************************************************************************************************)

PackageExport["RealMatrixQ"]

SetUsage @ "
RealVectorQ[list$] gives True if list$ is a matrix of real-valued numbers.
* Integers, rationals, etc. are considered real-valued numbers.
"

RealMatrixQ[list_] := MatrixQ[list, Internal`RealValuedNumberQ];

(**************************************************************************************************)

PackageExport["ComplexVectorQ"]

SetUsage @ "
ComplexVectorQ[list$] gives True if list$ is a vector of complex-valued numbers.
* At least one element of list$ should be a Complex expression.
* See %ContainsComplexQ.
"

ComplexVectorQ[list_] := VectorQ[list, NumericQ] && !FreeQ[list, Complex];

(**************************************************************************************************)

PackageExport["ContainsComplexQ"]

SetUsage @ "
ContainsComplexQ[expr$] gives True if expr$ contains at least one Complex expression.
* See %ComplexVectorQ.
"

ContainsComplexQ[expr_] := !FreeQ[expr, Complex];

(**************************************************************************************************)

PackageExport["ContainsNegativeQ"]

SetUsage @ "
ContainsNegativeQ[expr$] gives True if expr$ contains at least one negative real, rational, or integer.
"

ContainsNegativeQ[expr_] := !FreeQ[expr, n_Real | n_Rational | n_Integer ? Negative];

(**************************************************************************************************)

PackageExport["EquivalenceClassIndices"]

EquivalenceClassIndices[list_, fn_] :=
  Gather[Range @ Length @ list, fn[Part[list, #1], Part[list, #2]]&];

(**************************************************************************************************)
  
PackageExport["EquivalenceClassLabels"]

EquivalenceClassLabels[list_] := Scope[
  n = Max[list];
  arr = ConstantArray[0, n];
  ScanIndexed[Set[Part[arr, #1], First[#2]]&, list];
  arr
]

(**************************************************************************************************)

PackageExport["ArrayLabelIndices"]

SetUsage @ "
ArrayLabelIndices[array$, labels$] gives an array of the same shape as array$, whose values are indices of labels$.
* %Part[result$, p$] = i$ if %Part[array$, p$] = %Part[labels$, i$].
* Scalars not present in labels$ are left unchanged.
* ArrayLabelIndices is the inverse of %ArrayLabeling.
"

ArrayLabelIndices[array_, labels_] :=
  Replace[array, RuleRange @ labels, {1}];

ArrayLabelIndices[array_, labels_, level_] :=
  Replace[array, RuleRange @ labels, List[level]];

(**************************************************************************************************)

PackageExport["ArrayLabeling"]

SetUsage @ "
ArrayLabeling[list$] gives the result {indices$, assoc$}, where indices$ is a list the same length as array$, \
and assoc$ is an assocation whose values are indices and whose keys are elements of array$.
ArrayLabeling[array$, level$] examines the array$ at level$i, yielding an array of indices of depth level$.
* %Part[indices$, p$] = i$ if %Part[array$, p$] = assoc$[i].
* ArrayLabeling is the inverse of %ArrayLabelIndices.
"

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

(**************************************************************************************************)

PackageExport["ExtractIndices"]

SetUsage @ "
ExtractIndices[array$, indices$] gives a list of the parts of array$ given by indices$.
* indices$ can be an array of any depth, whose values are positive integer parts.
"

ExtractIndices[array_, indices_ /; VectorQ[indices, Internal`NonNegativeMachineIntegerQ]] :=
  Part[array, indices];

ExtractIndices[array_, indices_List] := Map[Part[array, #]&, indices, {-1}]

(**************************************************************************************************)

PackageExport["FirstColumn"]

SetRelatedSymbolGroup[FirstColumn, LastColumn, MostColumns, RestColumns];

SetUsage @ "
FirstColumn[matrix$] gives a list consisting of the first column of a matrix.
"

FirstColumn[matrix_] := Part[matrix, All, 1];

(**************************************************************************************************)

PackageExport["LastColumn"]

SetUsage @ "
LastColumn[matrix$] gives a list consisting of the last column of a matrix.
"

LastColumn[matrix_] := Part[matrix, All, -1];

(**************************************************************************************************)

PackageExport["MostColumns"]

SetUsage @ "
MostColumns[matrix$] gives a matrix consisting of the all but the last column of matrix$.
"

MostColumns[matrix_] := Part[matrix, All, All ;; -2];

(**************************************************************************************************)

PackageExport["RestColumns"]

SetUsage @ "
RestColumns[matrix$] gives a matrix consisting of the all but the first column of matrix$.
"

RestColumns[matrix_] := Part[matrix, All, 2 ;; All];

(**************************************************************************************************)

PackageExport["PrependColumn"]

SetRelatedSymbolGroup[PrependColumn, AppendColumn];

SetUsage @ "
PrependColumn[matrix$, column$] gives a matrix in which the list column$ has been prepended.
"

PrependColumn[matrix_, column_] := Transpose @ Prepend[Transpose @ matrix, column];
PrependColumn[column_][matrix_] := PrependColumn[matrix, column];

(**************************************************************************************************)

PackageExport["AppendColumn"]

SetUsage @ "
AppendColumn[matrix$, column$] gives a matrix in which the list column$ has been appended.
"

AppendColumn[matrix_, column_] := Transpose @ Append[Transpose @ matrix, column];
AppendColumn[column_][matrix_] := AppendColumn[matrix, column];

(**************************************************************************************************)

PackageExport["FirstRest"]

SetRelatedSymbolGroup[FirstRest, FirstLast, MostLast];

SetUsage @ "
FirstRest[list$] gives the pair {%First[list$], %Rest[list$]}.
"

FirstRest[list_] := {First @ list, Rest @ list};

(**************************************************************************************************)

PackageExport["FirstLast"]

SetUsage @ "
FirstLast[list$] gives the pair {%First[list$], %Last[list$]}.
"

FirstLast[list_] := {First @ list, Last @ list};

(**************************************************************************************************)

PackageExport["MostLast"]

SetUsage @ "
MostLast[list$] gives the pair {%Most[list$], %Last[list$]}.
"

MostLast[list_] := {Most @ list, Last @ list};

(**************************************************************************************************)

PackageExport["AssociationRange"]

SetRelatedSymbolGroup[AssociationRange, RuleRange];

SetUsage @ "
AssociationRange[{key$1, key$2, $$}] gives the association <|$$, key$i -> i$, $$|>.
"

AssociationRange[list_] :=
  AssociationThread[list, Range @ Length @ list];

(**************************************************************************************************)

PackageExport["RuleRange"]

SetUsage @ "
RuleRange[{key$1, key$2, $$}] gives the list {$$, key$i -> i$, $$}.
"

RuleRange[labels_] :=
  MapIndexed[#1 -> First[#2]&, labels];

(**************************************************************************************************)

PackageExport["RuleThread"]

SetRelatedSymbolGroup[RuleThread, AssociationThread];

SetUsage @ "
RuleThread[{key$1, key$2, $$}, {val$1, val$2, $$}] gives the list {$$, key$i -> val$i, $$}.
"

RuleThread[keys_, values_] :=
  MapThread[Rule, {keys, values}];

(**************************************************************************************************)

PackageExport["MinimumIndexBy"]
PackageExport["MaximumIndexBy"]
PackageExport["MinimumIndices"]
PackageExport["MaximumIndices"]
PackageExport["MinimumIndex"]
PackageExport["MaximumIndex"]
PackageExport["MinimumBy"]
PackageExport["MaximumBy"]
PackageExport["Minimum"]
PackageExport["Maximum"]

(* these represent cliques of functions along abstract dimensions.
there are three abstract dimensions: By-ness, Sign, and Index-ness *)

SetRelatedSymbolGroup @@@ {
  (* Sign symmetry: MinimumFoo <-> MaximumFoo *)
  {Minimum,        Maximum},        {MinimumIndex,   MaximumIndex},   {MinimumIndices, MaximumIndices},
  {MinimumBy,      MaximumBy},      {MinimumIndexBy, MaximumIndexBy},

  (* Bi-ness symmetry: FooBy <-> Foo *)
  {MinimumIndexBy, MinimumIndex},   {MaximumIndexBy, MaximumIndex},
  {MinimumBy,      Minimum},        {MaximumBy,      Maximum},

  (* Index symmetry: FooIndex <-> Foo *)
  {MinimumIndex,   MinimumIndices,   Minimum},      {MinimumIndexBy,   MinimumBy},
  {MaximumIndex,   MaximumIndices,   Maximum},      {MaximumIndexBy,   MaximumBy}
};

(**************************************************************************************************)

SetUsage @ "
MinimumIndexBy[{e$1, e$2, $$}, f$] gives the first index i$ for which f$[e$i] is minimal.
"

SetUsage @ "
MaximumIndexBy[{e$1, e$2, $$}, f$] gives the first index i$ for which f$[e$i] is maximal.
"

MinimumIndexBy[list_, f_] :=
  First @ Ordering[f /@ list, 1];


MaximumIndexBy[list_, f_] :=
  First @ Ordering[f /@ list, -1];

(**************************************************************************************************)

SetUsage @ "
MinimumIndices[{e$1, e$2, $$}] gives the list of indices i$ for which e$i is minimal.
"

SetUsage @ "
MaximumIndices[{e$1, e$2, $$}] gives the list of indices i$ for which e$i is maximal.
"

MinimumIndices[list_] :=
  MinimalBy[Range @ Length @ list, Part[list, #]&];

MaximumIndices[list_] :=
  MaximalBy[Range @ Length @ list, Part[list, #]&];

(**************************************************************************************************)

SetUsage @ "
MinimumIndex[{e$1, e$2, $$}] gives the first index i$ for which e$i is minimal.
"

SetUsage @ "
MaximumIndex[{e$1, e$2, $$}] gives the first index i$ for which e$i is maximal.
"

MinimumIndex[list_] :=
  First @ Ordering[list, 1];

MaximumIndex[list_] :=
  First @ Ordering[list, -1];

(**************************************************************************************************)

SetUsage @ "
MinimumBy[{e$1, e$2, $$}, f$] gives the first index i$ for which f$[e$i] is minimal.
"

SetUsage @ "
MaximumBy[{e$1, e$2, $$}, f$] gives the first index i$ for which f$[e$i] is maximal.
"

MinimumBy[list_, f_] :=
  Part[list, First @ Ordering[f /@ list, 1]];

MaximumBy[list_, f_] :=
  Part[list, First @ Ordering[f /@ list, -1]];

(**************************************************************************************************)

SetUsage @ "
Minimum[{e$1, e$2, $$}] gives the maximal e$i.
"

SetUsage @ "
Maximum[{e$1, e$2, $$}] gives the minimal e$i.
"

Minimum[list_] :=
  Part[list, First @ Ordering[list, 1]];

Maximum[list_] :=
  Part[list, First @ Ordering[list, -1]];

(**************************************************************************************************)

PackageExport["FirstIndex"]

SetUsage @ "
FirstIndex[{e$1, e$2, $$}, patt$] gives the first i$ for which e$i matches patt$.
"

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

SetRelatedSymbolGroup[RangeQ, PermutedRangeQ]

SetUsage @ "
RangeQ[list$] gives True if list$ is a permuation of {1, 2, $$, n$}.
"

RangeQ[list_] := PermutedRangeQ[list] && OrderedQ[list];

(**************************************************************************************************)

PackageExport["PermutedRangeQ"]

SetUsage @ "
PermutedRangeQ[list$] gives True if list$ is a permutation of {1, 2, $$, n$}.
"

PermutedRangeQ[list_] := VectorQ[list, IntegerQ] && MinMax[list] == {1, Length @ list};

(**************************************************************************************************)

PackageExport["DropWhile"]

SetUsage @ "
DropWhile[{e$1, e$2, $$}, f$] drops the initial elements e$i that all yield f$[ei$] = True.
"

DropWhile[list_, f_] := Drop[list, LengthWhile[list, f]];

(**************************************************************************************************)

PackageExport["MapStaggered"]

SetUsage @ "
MapStaggered[f$, {e$1, e$2, $$, e$n}] gives {f$[e$1, e$2], f$[e$2, e$3], $$, f$[e$(n-1), e$n]}.
"

MapStaggered[f_, list_] := f @@@ Partition[list, 2, 1];

(**************************************************************************************************)

PackageExport["MapIndices"]

SetUsage @ "
MapIndices[f$, {i$1, i$2, $$},  {e$1, e$2, $$}] applies f$ selectively on elements e$(i$1), e$(i$2), $$.
MapIndices[f$, indices$] is the operator form of MapIndices.
"

MapIndices[f_, {}, list_] := list;

MapIndices[f_, indices_, list_] :=
  MapAt[f, list, List /@ indices];

MapIndices[f_, indices_][list_] := MapIndices[f, indices, list];

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

SetRelatedSymbolGroup[NegatedQ, Negated, StripNegated, NegateReverse]

SetUsage @ "
NegatedQ[e$] returns True if e$ has head Negated.
"

NegatedQ[_Negated] = True;
NegatedQ[_] = False;

(**************************************************************************************************)

PackageExport["Negated"]

SetUsage @ "
Negated[elem$] represents the negation of elem$.
* Negated[a$ \[DirectedEdge] b$] represents the edge a$ \[DirectedEdge] b$ traversed in the reverse direction.
* %DirectedEdge[a$, b$, Negated[c$]] evaluates to %DirectedEdge[b$, a$, c$].
* Negated[Negated[c$]] evaluates to c$.
* Negated[c$] display as %Underbar[c$].
"

Negated[Negated[e_]] := e;
Negated /: DirectedEdge[a_, b_, Negated[c_]] := DirectedEdge[b, a, c];
Negated[CardinalSet[cards_]] := CardinalSet[Negated /@ cards];

declareBoxFormatting[
  Negated[e_] :> NegatedBoxForm[e]
];

(**************************************************************************************************)

PackageExport["NegateReverse"]

SetUsage @ "
NegateReverse[list$] applies Negated to elements of list$, then reverses the list.
"

NegateReverse[e_List] := Reverse @ Map[Negated, e];

(**************************************************************************************************)

PackageExport["StripNegated"]

SetUsage @ "
StripNegated[e$] removes the head Negated if present on e$.
* StripNegated does not map over lists.
"

StripNegated = Case[
  Negated[e_] := e;
  e_          := e;
];

(**************************************************************************************************)

PackageExport["NegatedForm"]

SetUsage @ "
NegatedForm[e$] displays as Underbar[e$].
"

declareBoxFormatting[
  NegatedForm[e_] :> NegatedBoxForm[e]
];

(**************************************************************************************************)

PackageExport["LookupOption"]

SetRelatedSymbolGroup[LookupOption, JoinOptions, DeleteOptions, TakeOptions, ReplaceOptions, UpdateOptions];

SetUsage @ "
LookupOption[object$, option$] looks up the value of option$ in object$.
LookupOption[object$, option$, default$] returns default$ if no value is associated with option$.
* object$ can be any expression that %Options evaluates on.
* By default, Automatic is returned if there is no value for option$.
"

LookupOption[obj_, opt_, default_:Automatic] :=
  Quiet @ Lookup[Options[obj, opt], opt, default];

(**************************************************************************************************)

PackageExport["JoinOptions"]

SetUsage @ "
JoinOptions[options$1, options$2, $$] joins together the options$i to form a single list of rules.
* The option$i can be a rule, list of rules, or a symbol with defined %Options.
* If multiple values are given for a given option symbol, the first is taken.
"

JoinOptions[opts___] := DeleteDuplicatesBy[
  Flatten @ Replace[Flatten @ {opts}, s_Symbol :> Options[s], {1}],
  First
];

(**************************************************************************************************)

PackageExport["DeleteOptions"]

SetUsage @ "
DeleteOptions[options$, symbol$] removes rules with LHS symbol$ from the list of rules given by options$.
DeleteOptions[options$, {sym$, sym$2, $$}] removes multiple rules.
DeleteOptions[spec$] is the operator form of DeleteOptions.
"

DeleteOptions[graph_Graph, keys_] :=
   Graph[VertexList @ graph, EdgeList @ graph, DeleteOptions[Options @ graph, keys]];

DeleteOptions[opts_, keys_List] :=
  DeleteCases[opts, (Alternatives @@ keys) -> _];

DeleteOptions[opts_, key_] :=
  DeleteCases[opts, key -> _];

DeleteOptions[key_][opts_] :=
  DeleteOptions[opts, key];

(**************************************************************************************************)

PackageExport["TakeOptions"]

SetUsage @ "
TakeOptions[options$, symbol$] gives only rules with LHS symbol$ from the list of rules given by options$.
TakeOptions[options$, {sym$, sym$2, $$}] gives rules that match any of the sym$i.
TakeOptions[spec$] is the operator form of TakeOptions.
"

TakeOptions[sym_Symbol, spec_] :=
  TakeOptions[Options @ sym, spec];

TakeOptions[opts_List, keys_List] :=
  Cases[opts, Verbatim[Rule][Alternatives @@ keys, _]];

TakeOptions[opts_List, key_] :=
  Cases[opts, Verbatim[Rule][key, _]];

(**************************************************************************************************)

PackageExport["ReplaceOptions"]

SetUsage @ "
ReplaceOptions[object$, symbol$ -> value$] gives a new version of object$ in which the option has been applied.
ReplaceOptions[object$, {rule$1, rule$2, $$}] applies multiple rule changes.
ReplaceOptions[spec$] is the operator form of ReplaceOptions.
* ReplaceOptions can take a %Graph, %Graphics, etc. as an object, returning the same kind of object.
* ReplaceOptions can take a list of rules as an object, returning a new list of rules.
"

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

SetUsage @ "
UpdateOptions[object$, option$, f$] returns a new object with the old value of option$ replaced with f$[value$].
* If the option was not present initially, the value %Automatic is supplied to f$.
"

UpdateOptions[obj_, option_, func_] :=
  ReplaceOptions[obj, option -> func[LookupOption[obj, option]]];

(**************************************************************************************************)

PackageExport["DeleteNull"]

SetUsage @ "
DeleteNull[list$] removes any elements that are Null from list$.
"

DeleteNull[e_] := DeleteCases[e, Null];

(**************************************************************************************************)

PackageExport["LookupAnnotation"]

SetUsage @ "
LookupAnnotation[object$, key$] gives the value of the annotation associated with key$ in object$.
LookupAnnotation[object$, {key$1, key$2, $$}] gives a list of values.
LookupAnnotation[object$, spec$, default$] evaluates and returns default$ if the key or keys is not present.
* By default, if no value is present, Automatic is returned.
"

SetHoldRest[LookupAnnotation];

LookupAnnotation[obj_, key_, default_:Automatic] :=
  OnFailed[AnnotationValue[obj, key], default];

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

General::noobjprop = "There is no property named \"``\". Valid properties include: ``.";
General::noobjoptprop = "There is no property named \"``\" that accepts options. Such properties include: ``.";

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
toOptionName[_] := Nothing;

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


(********************************************)

PackageExport["ValidPathWordQ"]

SetUsage @ "
ValidPathWordQ[word$] returns True if word$ is a list of cardinals.
ValidPathWordQ[word$, cards$] returns True if the cardinals in word are a subset of cards$.
* Negated cardinal can be present in the form Negated[cardinal$].
"

ValidPathWordQ[word_] := ValidPathWordQ[word, Automatic];

ValidPathWordQ[word_List, Automatic] := True;
ValidPathWordQ[word_List, cards_] := SubsetQ[cards, StripNegated /@ word];
ValidPathWordQ[_, _] := False;

(********************************************)

PackageExport["ToPathWord"]

SetUsage @ "
ToPathWord['word$'] interprets the characters of 'word$' as cardinals and returns a list of them.
ToPathWord['word$', cards$] only allows the cardinals in card$ (or their negations).
ToPathWord['word$', cards$, default$] evaluates and returns default$ is the path is not valid.
ToPathWord[list$, cards$, $$] checks that a list of cardinals is a subset of cards$.
* If a letter 'c$' is uppercased, it is interpreted as Negated['c$'].
* By default, $Failed is returned if the path is not valid.
"

SetHoldRest[ToPathWord];

PackageScope["$pathCancellation"]
$pathCancellation = True;

ToPathWord["" | {}, ___] = {};

ToPathWord[word_, validCardinals_, else_] :=
  OnFailed[
    ToPathWord[word, validCardinals],
    else
  ];

ToPathWord[word_, validCardinals_:Automatic] := Scope[
  $validCardinals = validCardinals;
  cardinals = toCardinalList @ word;
  Which[
    !ValidPathWordQ[cardinals, validCardinals],
      $Failed,
    $pathCancellation,
      cardinals //. $backtrackingRules,
    True,
      cardinals
  ]
];

$backtrackingRules = Dispatch @ {
  {l___, i_, Negated[i_], r___} :> {l, r},
  {l___, Negated[i_], i_, r___} :> {l, r}
};

toCardinalList = Case[
  list_List  := list;
  str_String := Map[toCardinal, Characters @ str];
  _          := $Failed;
];

toCardinal = Case[
  s_ /; MemberQ[$validCardinals, s] := s;
  s_ ? UpperCaseQ := Negated @ ToLowerCase @ s;
  s_ := s
];

(**************************************************************************************************)

PackageScope["UnpackOptionsAs"]

DefineMacro[UnpackOptionsAs,
UnpackOptionsAs[head_Symbol, opts_, syms__Symbol] :=
  mUnpackOptionsAs[head, opts, {syms}]
];

SetHoldAllComplete[mUnpackOptionsAs];
mUnpackOptionsAs[head_, opts_, syms_] :=
  ToQuoted[Set, Quoted[syms],
    ToQuoted[OptionValue, head, List @ opts, symbolsToCapitalizedStrings @ syms]
  ];

(**************************************************************************************************)

SetHoldAllComplete[symbolsToCapitalizedStrings];

symbolsToCapitalizedStrings[syms_] := Map[
  Function[sym, capitalizeFirstLetter @ HoldSymbolName @ sym, HoldAllComplete],
  Unevaluated @ syms
];

capitalizeFirstLetter[str_String] := 
  If[StringStartsQ[str, "$"], capitalizeFirstLetter @ StringDrop[str, 1],
    ToUpperCase[StringTake[str, 1]] <> StringDrop[str, 1]];

(**************************************************************************************************)

PackageScope["UnpackAnonymousOptions"]

DefineMacro[UnpackAnonymousOptions,
UnpackAnonymousOptions[object_, default_, syms__Symbol] :=
  mUnpackAnonymousOptions[object, default, {syms}] 
];

SetHoldAllComplete[mUnpackAnonymousOptions];
mUnpackAnonymousOptions[object_, default_, syms_] :=
  ToQuoted[Set, Quoted[syms],
    ToQuoted[LookupOption,
      Quoted[object],
      findMatchingSymbols[syms],
      Quoted[default]
    ]
  ];

(**************************************************************************************************)

PackageScope["UnpackAnonymousThemedOptions"]

DefineMacro[UnpackAnonymousThemedOptions,
UnpackAnonymousThemedOptions[object_, default_, syms__Symbol] :=
  mUnpackAnonymousThemedOptions[object, default, {syms}] 
];

SetHoldAllComplete[mUnpackAnonymousThemedOptions];
mUnpackAnonymousThemedOptions[object_, default_, syms_] :=
  ToQuoted[Set, Quoted[syms],
    ToQuoted[LookupThemedOption,
      Quoted[object],
      findMatchingSymbols[syms],
      Quoted[default]
    ]
  ];
(**************************************************************************************************)

PackageScope["UnpackExtendedThemedOptions"]

DefineMacro[UnpackExtendedThemedOptions,
UnpackExtendedThemedOptions[graph_, syms___Symbol] :=
  mUnpackExtendedThemedOptions[graph, {syms}] 
];

SetHoldAllComplete[mUnpackExtendedThemedOptions];
mUnpackExtendedThemedOptions[graph_, syms_] := 
  ToQuoted[Set, Quoted[syms],
    ToQuoted[LookupExtendedThemedOption,
      Quoted[graph],
      findMatchingSymbols[syms]
    ]
  ];

(**************************************************************************************************)

PackageScope["UnpackExtendedOptions"]

DefineMacro[UnpackExtendedOptions,
UnpackExtendedOptions[graph_, syms___Symbol] :=
  mUnpackExtendedOptions[graph, {syms}] 
];

SetHoldAllComplete[mUnpackExtendedOptions];
mUnpackExtendedOptions[graph_, syms_] :=
  ToQuoted[Set, Quoted[syms],
    ToQuoted[LookupExtendedOption,
      Quoted[graph],
      findMatchingSymbols[syms]
    ]
  ];

SetHoldAllComplete[findMatchingSymbols];
$lowerCaseSymbolRegExp = RegularExpression["\\b([a-z])(\\w+)\\b"];
findMatchingSymbols[syms_List] := findMatchingSymbols[syms] = Block[
  {$Context = "QuiverGeometry`Private`Dummy`", $ContextPath = {"System`", "QuiverGeometry`", $Context}, str},
  str = ToString[Unevaluated @ syms, InputForm];
  str = StringReplace[str, $lowerCaseSymbolRegExp :> StringJoin[ToUpperCase["$1"], "$2"]];
  ToExpression[str, InputForm, Quoted]
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

PackageExport["CatchMessage"]

DefineMacro[CatchMessage,
CatchMessage[body_] := Quoted[Catch[body, ThrownMessage[_], ThrownMessageHandler[$LHSHead]]]
];

ThrownMessageHandler[msgHead_Symbol][{args___}, ThrownMessage[msgName_String]] :=
  (Message[MessageName[msgHead, msgName], args]; $Failed);

PackageExport["ThrowMessage"]

ThrowMessage[msgName_String, msgArgs___] :=
  Throw[{msgArgs}, ThrownMessage[msgName]];

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

PackageExport["StringReplaceRepeated"]

StringReplaceRepeated[str_String, rules_] := FixedPoint[StringReplace[rules], str];
StringReplaceRepeated[rules_][str_] := StringReplaceRepeated[str, rules];

(**************************************************************************************************)

PackageExport["AbsolutePathQ"]

AbsolutePathQ = Case[
  s_String /; $SystemID === "Windows" := StringStartsQ[s, LetterCharacter ~~ ":\\"];
  s_String := StringStartsQ[s, $PathnameSeparator | "~"];
  _ := False;
];

(**************************************************************************************************)

PackageExport["NormalizePath"]

NormalizePath = Case[
  ""            := "";
  None          := None;
  path_String   := StringReplaceRepeated[path, $pathNormalizationRules];
];

$pathNormalizationRules = {
  StartOfString ~~ "~" :> $HomeDirectory,
  $PathnameSeparator ~~ Except[$PathnameSeparator].. ~~ $PathnameSeparator ~~ ".." ~~ $PathnameSeparator :> $PathnameSeparator,
  $PathnameSeparator ~~ "." :> ""
}

(**************************************************************************************************)

PackageScope["ToFileName"]

ToFileName[""|None, ""|None] :=
  $Failed;

ToFileName[""|None, file_String] :=
  NormalizePath @ file;

ToFileName[base_String, file_String] := 
  NormalizePath @ FileNameJoin[{base, file}];

(**************************************************************************************************)

PackageExport["ExportUTF8"]

ExportUTF8[path_, string_] := 
  Export[path, string, "Text", CharacterEncoding -> "UTF-8"];

  (**************************************************************************************************)

PackageExport["CopyUnicodeToClipboard"]

CopyUnicodeToClipboard[text_] := Scope[
  out = FileNameJoin[{$TemporaryDirectory, "temp_unicode.txt"}];
  Export[out, text, CharacterEncoding -> "UTF-8"];
  Run["osascript -e 'set the clipboard to ( do shell script \"cat " <> out <> "\" )'"];
  DeleteFile[out];
];

(**************************************************************************************************)

PackageExport["BinaryDigits"]

BinaryDigits[n_, len_] := IntegerDigits[n, 2, len];
BinaryDigits[len_][n_] := BinaryDigits[n, len];

(**************************************************************************************************)

PackageExport["BitAndQ"]

BitAndQ[a_, b_] := Total[BitAnd[a, b]] =!= 0;

(**************************************************************************************************)

PackageExport["BitNandQ"]

BitNandQ[a_, b_] := Total[BitAnd[a, b]] === 0;
BitNandQ[a___] := DuplicateFreeQ[{a}, BitAndQ];

(**************************************************************************************************)

PackageExport["RangePartitionIndices"]

RangePartitionIndices[n_] := Scope[
  CollectTo[{$partBag}, rangPartRecurse[{}, Range[n]]];
  $partBag
];

rangPartRecurse[parts_, {}] := Internal`StuffBag[$partBag, parts];
rangPartRecurse[parts_, rem:{_}] := Internal`StuffBag[$partBag, Append[parts, rem]];
rangPartRecurse[parts_, rem_] := Scope @ Scan[
  {first, rest} = TakeDrop[rem, 1];
  rangPartRecurse[
    Append[parts, Join[first, #]], 
    Complement[rest, #]
  ]&,
  Subsets[rest, {1, Infinity}]
];

(**************************************************************************************************)

PackageExport["RangePartitionGraph"]

RangePartitionGraph[n_] := Scope[
  init = List /@ Range[n];
  MultiwaySystem[rangePartitionSuccessors, {init}]
];
  
rangePartitionSuccessors[part_] := Join @@ Table[
  Sort @ Append[
    Delete[part, {{i}, {j}}], 
    Sort[Join @@ Part[part, {i, j}]]
  ],
  {i, Length @ part}, {j, i+1, Length @ part}
];