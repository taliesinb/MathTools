SetUsage @ "
DataFrame[{<|'key$1' -> col$1, 'key$2' -> col$2, $$}] constructs a data frame from a list of assocations.
DataFrame[{'key$1', 'key$2', $$} -> {col$1, col$2, $$}] constructs a data frame from columns and their names.
DataFrame[{'key$1' -> col$1, $$}] as above.
DataFrame[<|pkey$1 -> row$1, pkey$2 -> row$2, $$|>] constructs a data frame with primary keys.
DataFrame[{row$1, row$2, $$}] constucts a dataframe with integer column names.
* selection of rows produces a %DataSelection[$$] object.
* selection of columns produces a %DataColumn[$$] object.
";

General::frameStringKeys = "Keys `` must be strings!";
General::frameNotLists = "Columns should be equal-length lists.";
General::frameBadData = "Bad data with head ``.";
General::frameKeysColsLength = "Number of keys `` =!= number of columns ``."
General::frameRagged = "Length of columns aren't equal: ``."
General::frameEmpty = "DataFrame can't have zero columns.";
General::frameUnknownQuery = "Unknown query element: ``."
General::frameInvalidSpan = "Invalid span: ``.";
General::frameReplacePart = "Invalid ReplacePart spec.";

DataFrame[spec_] := createDataFrame[spec];

(**************************************************************************************************)

createDataFrame[spec_] := CatchMessage[DataFrame, createDataFrame2 @ spec];

createDataFrame2 = Case[

  keys_List -> cols_List :=
    makeDataFrame[keys, cols];

  keysvals:{Rule[_, _List]..} :=
    makeDataFrame[Keys @ keysvals, Values @ keysvals];

  assoc_Association := Scope[
    {keys, vals} = KeysValues @ assoc;
    If[VectorQ[vals, ListQ] && AnyMatrixQ[vals], Return @ makeDataFrame[keys, vals]];
    {keys2, cols} = toKeysCols @ vals;
    makeDataFrame[Pre[keys2, "Key"], Pre[cols, keys]]
  ];

  data_ :=
    makeDataFrame @@ toKeysCols @ data;
];

(**************************************************************************************************)

toKeysCols := Case[

  data_List ? AssociationVectorQ := Scope[
    keys = Union @@ (Keys /@ data);
    If[!StrVecQ[keys], Msg::frameStringKeys[keys]];
    cols = Lookup[data, Key[#], None]& /@ keys;
    {keys, cols}
  ];

  data_List ? ListVectorQ := Scope[
    dims = Dims @ data;
    If[Len[dims] == 1, ThrowMessage["frameNotLists"]];
    keys = IntStr @ Range @ P2 @ dims;
    {keys, Transpose @ data}
  ];

  other_ := Msg::frameBadData[H @ data];
];

(**************************************************************************************************)

makeDataFrame[{}, _] := ThrowMessage["frameEmpty"];

makeDataFrame[keys_, cols_] := Scope[
  SameLenMsg::frameKeysColsLength[keys, cols];
  lens = Len /@ cols;
  If[!AllEqualQ[lens], Msg::frameRagged[RuleThread[keys, lens]]];
  cols = ToPacked /@ cols;
  ConstructNoEntry[DataFrame, keys, cols, F @ lens]
];

makeDataFrameFromRules[rules:(_List | _Association)] := makeDataFrame[Keys @ rules, Values @ rules];

(**************************************************************************************************)

PublicFunction[DataFrameQ]

DataFrameQ[HoldP[DataFrame[_List, _List, _Int] ? HoldNoEntryQ]] := True;
DataFrameQ[_] := False;

(**************************************************************************************************)

PublicFunction[FrameRows, FrameCols, FrameRowAssocs, FrameAssoc]

(**************************************************************************************************)

SetHoldAll[defineDataFrameUpValues, defineDFUV];

defineDataFrameUpValues[CompoundExpression[rules__SetDelayed, Null...]]  :=
  Scan[defineDFUV, Uneval @ {rules}]

defineDFUV[head_Symbol[largs___,  $, rargs___] := rhs_] :=
  TagSetDelayed @@ Hold[
    DataFrame,
    head[largs, HoldP[DataFrame[$K_List, $V_List, $N_Int] ? HoldNoEntryQ], rargs],
    rhs
  ];

defineDFUV[arg_] := PrintIF[Hold[arg]];

defineDataFrameUpValues[

  FrameRows[$]           := Transpose @ $V;
  FrameCols[$]           := $V;
  FrameRowAssocs[$]      := Map[AssocThread[$K, #]&, Transpose @ $V];
  FrameAssoc[$]          := AssocThread[$K, $V];

  (* these are a bit iffy, we should introduce ColumnKeys, ColumnValues *)
  Keys[$]                := $K;
  Values[$]              := $V;
  KeysValues[$]          := {$K, $V};

  KeyTake[$, k_]         := dsDispatch[$K, $V, $N, KeyTake, dsKeyTake, k];
  KeyDrop[$, k_]         := dsDispatch[$K, $V, $N, KeyDrop, dsKeyDrop, k];
  Take[$, n_]            := dsDispatch[$K, $V, $N, Take, dsTake, n];
  Drop[$, n_]            := dsDispatch[$K, $V, $N, Drop, dsDrop, n];

  Sort[$]                := dsDispatch[$K, $V, $N, Sort, dsSortBy, Identity, False];
  ReverseSort[$]         := dsDispatch[$K, $V, $N, ReverseSort, dsSortBy, Identity, True];
  SortBy[$, f_]          := dsDispatch[$K, $V, $N, SortBy, dsSortBy, f, False];
  ReverseSortBy[$, f_]   := dsDispatch[$K, $V, $N, ReverseSortBy, dsSortBy, f, True];

  TakeLargestBy[$, f_, n_]  := dsDispatch[$K, $V, $N, TakeLargestBy, dsLargestBy, f, TakeLargestBy];
  TakeSmallestBy[$, f_, n_] := dsDispatch[$K, $V, $N, TakeSmallestBy, dsLargestBy, f, TakeSmallestBy];

  ReplacePart[$, rule_]  := dsDispatch[$K, $V, $N, ReplacePart, dsReplacePart, rule];
  ReplaceAll[$, rule_]   := dsDispatch[$K, $V, $N, ReplaceAll, dsReplaceAll, rule];
  Dims[$]                := {$N, Len @ $K};
  Len[$]                 := $N;
  Part[$, spec__]        := dsPart[$K, $V, $N, spec];
  Lookup[$, key_Str]     := columnData @ key;

  RandomChoice[$]        := Scope[
    i = RandomInteger[{1, $N}];
    AssocThread[$K, Part[$V, All, i]]
  ];

  RandomSample[$, n_Int] := Scope[
    indices = RandomSample[range @ $N, n];
    If[!ListQ[indices], ReturnFailed[]];
    makeDataFrame[$K, Part[#, indices]& /@ $V]
  ];

  Map[fn_, $]               := dsDispatch[$K, $V, $N, Map, dsMap, fn];
  Scan[fn_, $]              := dsDispatch[$K, $V, $N, Scan, dsScan, fn];
  Select[$, fn_]            := dsDispatch[$K, $V, $N, Select, dsSelect, fn];
  Discard[$, fn_]           := dsDispatch[$K, $V, $N, Discard, dsDiscard, fn];

(*
  SelectFirst[$, fn_]       := dsDispatch[$K, $V, $N, Select, dsSelect, fn];
  SelectFirstIndex[$, fn_]  := dsDispatch[$K, $V, $N, Select, dsSelect, fn];
  SelectIndices

  should maybe map over columns. but would be nice if it ignored values that don't make sense for each function!
  and if no values apply in a column that column doesn't show up!
  Counts
  Histogram
  etc
 *)
  MapColumn[fn_, n_, $]     := dsDispatch[$K, $V, $N, MapColumn, dsMapColumn, fn];
];

SetListable[defineOperatorDispatch];
defineOperatorDispatch[sym_Symbol, n_] := TagSetDelayed[DataFrame, s_sym[df_DataFrame], Insert[s, df, n]];

defineOperatorDispatch[{Map, Scan}, 2]
defineOperatorDispatch[{Select}, 1];

(**************************************************************************************************)

(* TODO: Make a UUID, and cache picked indices because they're cheap!
also abort on messages! *)

(**************************************************************************************************)

df_DataFrame[query_, result_:All] := dfQuery[df, query, result];

dfQuery[df:HoldP[DataFrame[$K_List, $V_List, $N_Int] ? HoldNoEntryQ], query_, result_] :=
  Block[{$lhs = df}, dsDispatch[$K, $V, $N, DataFrame, doQueryResult, query, result]];

doQueryResult[query_, result_] := Scope[
  $indices = doQuery[query];
  doResult @ result
];

doQuery = Case[
  All := All;

  span:(Span[_, _] | Span[_, _, _]) := resolveSpan @ span;

  Verbatim[Pattern][key_Symbol, rhs_] :=
    %[HoldSymbolName[key] -> rhs];

  key_Str -> val_ :=
    pickCol[key, val];

  fn_Function     := If[Count[fn, Slot[_Str], Inf] == 1,
    pickCol[DeepFirstCase[fn, Slot[s_Str] :> s], simplifyFn[fn /. _Slot -> Slot[]]],
    pickIndices @ normalMap @ fn
  ];

  Sampled[n_Int] := RandomSample[range @ $N, n];

  list_List := intersectIndices @ Map[%, list];

  other_ := Msg::frameUnknownQuery[other];
];

(**************************************************************************************************)

intersectIndices = Case[
  {a_}      := a;
  list_List := Intersection @@ VectorReplace[list, s_Span :> Part[range @ $N, s]]
];

resolveSpan = Case[
  Span[All, All, n_?Negative] := Span[$N, 1, n];
  Span[All, r__]              := % @ Span[1, r];
  Span[f_, All, r___]         := % @ Span[f, $N, r];
  Span[i_Int, j_Int]          := Most @ clipSpan[i, j, 1];
  Span[i_Int, j_Int, n_Int]   := clipSpan[i, j, n];
  other_                      := Msg::frameInvalidSpan[other];
];

clipN[i_] := Which[
  i > $N, $N,
  i < 0,  Clip[$N + 1 + i, {0, $N}],
  True,   i
];

clipSpan[i_, j_, n_] := Scope[
  i //= clipN; j //= clipN;
  If[n > 0, i = Min[i, j+1]];
  If[n < 0, i = Max[i, j-1]];
  Span[i, j, n]
];

(**************************************************************************************************)

pickCol[key_, fn:$listableP[__]] := pickIndices @ fn @ columnData @ key;
pickCol[key_, fn_ ? MightEvaluateWhenAppliedQ] := pickIndices @ Map[fn, columnData @ key];
pickCol[key_, patt_] := pickIndices[columnData @ key, patt];

simplifyFn = Case[
  Fn[(h:$lp)[_Slot, s_]] := h[s];
  other_            := other;
,
  $lp -> $listableP
];

(* these fall into transformations that:
* post-process the indices, return a new DS:  All, Sampled, Span, {__Integer}
* select a specific index, return its assoc:  _Integer
* return facts about the indices:             Part, Len
* return a column:                            key
* aggregate the results:                      key -> fn
*)

doResult = Case[

  Part                             := $indices;

  Len                              := If[$indices === All, $N, Len @ $indices];

  All                              := If[$indices === All, $lhs, fromIndices @ $indices];

  Sampled[n_Int]                   := fromIndices @ If[$indices === All, RandomSample[range @ $N, n], RandomSample[$indices, n]];

  part:(_Span | {__Integer})       := If[$indices === All, fromIndices @ part, fromIndices @ SafePart[$indices, part]];

  key_Str                          := Part[columnData @ key, $indices];

  key_Str -> fn_                   := fn[%[key]];

  list_List                        := Map[%, list];

  other_                           := doMultiResult[other];

  i_Integer                        := getPartI @ Which[
    $indices === All,             i,
    i > Len[$indices],            L @ $indices,
    i < -Len[$indices] || i == 0, F @ $indices,
    True,                         Part[$indices, i]
  ];

];

(* TODO: exploit listability here! *)

doMultiResult = Case[

  fn_Function  := MapThread[transformSlots @ fn, Part[$V, All, $indices]];

  fn_ ? MightEvaluateWhenAppliedQ := normify[fn] @ Part[$V, All, $indices];

];

(**************************************************************************************************)

PublicFunction[JoinFrames]

SetUsage @ "
JoinFrames[frame$1, frame$2, $$] joins one or more frames into a single %DataFrame.
* keys in later frames override earlier ones, unless they are %None or %Missing[$$}.
* all frames should have the same length.
* see JoinAcross for keyed joins, which works on %DataFrames.
"

General::mismatchingFrameLengths = "Number of rows in frames doesn't match: ``.";
JoinFrames[dfs__DataFrame] := Scope[
  frames = List[dfs];
  {ks, vs, ns} = Transpose[dataFrameInternals /@ frames];
  If[!AllSameQ[ns], ReturnFailed["mismatchingFrameLengths", ns]];
  uks = Catenate @ ks;
  If[DuplicateFreeQ[uks], Return @ makeDataFrame[Catenate @ ks, Catenate @ vs]];
  makeMergedDataFrame[Map[dataFrameColRules, frames]]
];

makeMergedDataFrame[frameRuleLists_] :=
  makeDataFrameFromRules @ Merge[frameRuleLists, mergeFrameCols];

mergeFrameCols[{c_}] := c;
mergeFrameCols[cols_] := If[FreeQ[cols, None|Missing], L @ cols, MapThread[mergeFrameElems, cols]];

mergeFrameElems[a_, None | _Missing] := a;
mergeFrameElems[_, b_] := b;
mergeFrameElems[many__] := FirstCase[Rev[{many}], Except[None|_Missing]];

(**************************************************************************************************)

PrivateFunction[dataFrameInternals, dataFrameColRules]

dataFrameInternals[HoldP[DataFrame[k_List, v_List, n_Int] ? HoldNoEntryQ]] := {k, v, n};

dataFrameColRules[HoldP[DataFrame[k_List, v_List, n_Int] ? HoldNoEntryQ]] := RuleThread[k, v];

(**************************************************************************************************)

PublicFunction[UpdateFrame]

SetUsage @ "
UpdateFrame[DataFrame[$$], map$] inserts or updates one or more columns of a %DataFrame.
UpdateFrame[$$, filter$] only applies the update to rows matching query$.
* map$ should return a key or list of keys.
* the special value Nothing will remove that column.
* if new columns are introduced to only specific rows, other rows are filled with None.
UpdateFrame[map$] is an operator form of UpdateFrame.
"

UpdateFrame::resultLength = "Length of result of mapping did not match the expected value of ``.";
UpdateFrame::mapResults = "Mapping did not return a rule or list of rules for one or more rows.";

UpdateFrame[update_][df_] := UpdateFrame[df, update];

UpdateFrame[df_ ? DataFrameQ, update_, filter_:None] := Scope @ CatchMessage[
  {$K, $V, $N} = dataFrameInternals[df];
  $indices = doQuery @ SubNone[filter, All];
  $decondValue = None;

  results = doMultiResult @ update;
  If[results === {}, Return @ df];

  mergedResults = Check[Merge[results, Identity], $Failed];
  If[!AssociationQ[mergedResults], ReturnFailed["mapResults"]];

  If[$indices =!= All,
    $blankColumn = Repeat[None, $N];
    mergedResults = insertNoneEntries /@ mergedResults;
  ];
  mergedResults = Discard[mergedResults, allNoneQ];
  oldRules = dataFrameColRules @ df;
  makeMergedDataFrame[{oldRules, mergedResults}]
];

insertNoneEntries[sparseColumn_] := Module[
  {denseColumn = $blankColumn},
  Part[denseColumn, $indices] = sparseColumn;
  denseColumn
];

allNoneQ[list_List] := !FreeQ[list, None] && MatchQ[list, {None..}];

(**************************************************************************************************)

pickIndices[bools_] := Pick[range @ $N, bools];
pickIndices[values_, patt_] := Pick[range @ $N, values, patt];

range[n_] := range[n] = Range @ n;

fromIndices[{}]       := newDataFrame[$K, Repeat[{}, Len @ $K]];
fromIndices[indices_] := newDataFrame[$K, Part[$V, All, indices]];

fromBits[bits_, bit_] := newDataFrame[$K, Pick[#, bits, bit]& /@ $V];

(**************************************************************************************************)

dsMap[fn_Fn] := rawMap @ decondition @ fn;
dsMap[fn_] := normalMap @ fn;

rawMap[fn_Fn] := MapThread[transformSlots @ fn, $V];
normalMap[fn_] := MapThread[(normify @ fn) /. # -> {##}, $V];
normify[fn_] := fn[AssocThread[$K, #]]&;

$decondValue = Nothing;
decondition[f_] := With[{dc = $decondValue}, RepAll[f, Verbatim[Condition][body_, test_] :> If[TrueQ[test], body, dc]]];

(**************************************************************************************************)

transformSlots[f_Fn] := RepAll[f, Slot[k_Str] :> RuleEval[Slot @ getColIndex @ k]];

transformSlots[HoldPattern @ Fn[var_Symbol, body_]]      := substituteFnSymbolsWithColIndices[{var}, body];
transformSlots[HoldPattern @ Fn[vars:{__Symbol}, body_]] := substituteFnSymbolsWithColIndices[vars, body];

SetHoldAllComplete[substituteFnSymbolsWithColIndices];
substituteFnSymbolsWithColIndices[vars_List, body_] :=
  Function[body] /. RuleThread[
    HoldMap[HoldPattern, vars],
    Slot /@ getColIndex /@ HoldMap[HoldSymbolName, vars]
  ];

(**************************************************************************************************)

dsScan[fn_Fn] := rawMap @ nullify @ fn;
dsScan[fn_] := Scan[normify @ fn, Transpose @ $V];

nullify[Fn[c_]] := Fn[c;];
nullify[f_] := f;

(**************************************************************************************************)

General::framePredicate = "Frame predicate function did not return booleans: ``, result may be incorrect.";

dsSelect[spec_] := fromBits[dsEvalPredicates[spec, BitAnd], 1];
dsDiscard[spec_] := fromBits[dsEvalPredicates[spec, BitOr], 0];

dsEvalPredicates[specs_List, combiner_] := checkPredResult @ Apply[combiner, Map[dsEvalOnePredicate, specs]];
dsEvalPredicates[spec_, _] := checkPredResult @ dsEvalOnePredicate @ spec;

checkPredResult[list_ ? IntegerVectorQ] := list;
checkPredResult[list_] := (
  Message[General::framePredicate, SelectFirst[list, IntQ /* Not]];
  ToPacked @ VectorReplace[list, Except[_Int] -> 0]
);

dsEvalOnePredicate := Case[

  Rule[key_, pred_] := ToPacked @ dsEvalKeyPredicate[columnData @ key, pred];

  fn_ := Boole @ dsMap @ fn;

];

dsEvalKeyPredicate[col_] := Case[

  patt:(_Alt | _Blank) := Map[Boole @ MatchQ[#, patt]&, col];

  glob_Str := Boole @ StringMatchQ[col, glob];

  literal:(False | True | _Int | None) := Map[Boole @ SameQ[#, literal]&, col];

  (op:SStartsQ|SEndsQ|SMatchQ|SFreeQ)[patt_] := Check[
    Quiet @ op[col, patt],
    Boole @ Map[op[patt], col]
  ];

  fn_ := Boole @ Map[fn, col];
];

(**************************************************************************************************)

dsExtractor = Case[
  Identity        := Transpose @ $V;
  key_Str         := columnData @ key;
  key_Str -> fn_  := Map[fn, columnData @ key];
  specs_List      := Transpose @ Map[%, specs];
];

dsSortBy[spec_, rev_] := Scope[
  order = Ordering @ dsExtractor @ spec;
  If[rev, order //= Reverse];
  fromIndices[order]
];

dsLargestBy[spec_, UpTo[n_], fn_] := dsLargestBy[spec, n, fn];
dsLargestBy[spec_, n_, fn_] := Scope[
  order = fn[dsExtractor[spec] -> "Index", UpTo[n]];
  fromIndices[order]
];

(**************************************************************************************************)

dsReplacePart[{i_Int, key_} -> value_] :=
  makeDataFrame[$K, ReplacePart[$V, {getColIndex[key], i} -> value]];

dsReplacePart[{part:(All | _Span | {__Int}), key_} -> value_] := Scope[
  If[ListQ[value], ThrowMessage["frameReplacePart"]];
  v2 = $V;
  Part[v2, getMultiColIndex[key], part] = value;
  makeDataFrame[$K, v2]
];

getMultiColIndex = Case[
  All    := All;
  l_List := Map[getColIndex, l];
  key_   := getColIndex[key];
];

dsReplacePart[___] := ThrowMessage["frameReplacePart"];

(**************************************************************************************************)

dsDispatch[k_, v_, n_, fn_, impl_, args___] := Scope @ CatchMessage[fn,
  $K = k; $V = v; $N = n;
  impl[args]
];

(**************************************************************************************************)

dsPart[k_, v_, n_, specs__] := Scope @ CatchMessage[Part,
  $K = k; $V = v; $N = n; part[specs]
];

$multiPart = All | _Span | {__Int};

part[i_Int] := getPartI[i];
part[i_Int, All] := getPartI[i];
getPartI[i_] := If[i == 0 || Abs[i] > $N, $Failed, AssocThread[$K, Part[$V, All, i]]];

part[multi:$multiPart] := newDataFrame[$K, Part[$V, multi]];

part[part_, i_Int] :=
  SafePart[$V, i, part];

part[part_, k_Str] :=
  SafePart[$V, getColIndex @ k, part];

(**************************************************************************************************)

dsKeyTake[k_] := makeDataFrameFromRules @ KeyTake[RuleThread[$K, $V], k];
dsKeyDrop[k_] := makeDataFrameFromRules @ KeyDrop[RuleThread[$K, $V], k];

dsTake[n_]    := makeDataFrame[$K, SafeTake[#, n]& /@ $V];
dsDrop[n_]    := makeDataFrame[$K, SafeDrop[#, n]& /@ $V];

(**************************************************************************************************)

columnData[k_] := Part[$V, getColIndex @ k];

getColIndex[k_] := IndexOf[$K, k, badColumn[k]];

General::noDataFrameColumn = "DataFrame does not have a column called ``, available: ``."
badColumn[k_] := Msg::noDataFrameColumn[k, $K];

(**************************************************************************************************)

General::badPartSpec = "Unsupported part spec: ``."
part[spec__] := Msg::badPartSpec[{spec}];

newDataFrame[_, {}] := ThrowMessage["frameEmpty"];
newDataFrame[k_, c_] := ConstructNoEntry[DataFrame, k, ToPacked /@ c, Len @ F @ c];

getPartI[i_] := If[i == 0 || Abs[i] > $N, $Failed, AssocThread[$K, Part[$V, All, i]]];

(**************************************************************************************************)
(*
DataFrame /: Normal[HoldPattern @ DataFrame[k_List, v_List]] := AssociationThread[k, #]& /@ v;

DataFrame /: Keys[HoldPattern @ DataFrame[k_List, v_List]] := k;

DataFrame /: Values[HoldPattern @ DataFrame[k_List, v_List]] := v;

DataFrame /: KeysValues[HoldPattern @ DataFrame[k_List, v_List]] := {k, v};

DataFrame[k_List, v_List]] := {k, v};

*)

(**************************************************************************************************)

DefineStandardTraditionalForm[
  HoldP[DataFrame[k_List, v_List, n_Integer]] ? HoldNoEntryQ :> dataFrameBoxes[k, v, n]
];

dataFrameBoxes[k_, v_, n_] := ToBoxes @ Labeled[
  NiceGrid[k -> v, ItemSize -> {UpTo @ 600, UpTo @ 100}, MaxRows -> 10, MaxWidth -> 2000, Splits -> None, ItemFunction -> dfItemFunction],
  Row[{n, " rows"}, BaseStyle -> {FontSize -> 12, FontFamily -> "Arial", FontColor -> GrayLevel[0.5]}]
];

dfItemFunction[s_String /; StringStartsQ[s, "/Users/"]] := MsgPath[s];
dfItemFunction[other_] := other;

(**************************************************************************************************)

dataFrameBoxesOld[k_, v_, n_] := Scope[
  $n = n; $countPadding = SLen @ IntStr @ n;
  gridEntries = ZipMap[colSummary, k, v];
  grid = niceGrid @ gridEntries;
  ToBoxes @ Labeled[grid, Row[{n, " rows"}, BaseStyle -> {FontSize -> 12, FontFamily -> "Fira Code"}]]
];

niceGrid[entries_] := Grid[
  entries, Background -> GrayLevel[0.98],
  FrameStyle -> GrayLevel[0.3],
  BaseStyle -> {FontSize -> 12, FontFamily -> "Fira Code"},
  RowSpacings -> 1, ColumnSpacings -> 1.5,
  RowMinHeight -> 1.2,
  Dividers -> All, RowAlignments -> Baseline, ColumnAlignments -> {Right, Left},
  GridFrameMargins -> {{1.5, 1.5}, {1, 1}}
];

$specialHeads = {True|False -> "bool"[], None -> "null"[]};

$toHeadName = {
  Int -> "int", Rational -> "real", Real -> "real", Str -> "str", "bool" -> "bool",
  "null" -> "null", Missing -> "null", List -> "list", Assoc -> "dict", Symbol -> "sym",
  _ -> "?"
};

colSummary[key_, {}] := {Style[key, Bold], "empty"};

colSummary[key_, col_] := Scope[
  $values = col;
  $heads = VectorReplace[H /@ VectorReplace[col, $specialHeads], $toHeadName];
  counts = Counts[$heads];
  If[ContainsExactly[Decases["null"] @ Keys @ counts, {"int", "real"}],
    $heads = VectorReplace[$heads, "int" -> "real"];
    counts = Counts[$heads];
  ];
  counts = Rev @ KSort @ counts;
  entries = KVMap[formatHeadCount, counts];
  {Style[key, Bold], RawBoxes @ RowBox @ Riffle[entries, ","]}
];

formatHeadCount["null", _] := Nothing;
formatHeadCount[head_, count_] := Scope[
  summary = Lookup[$summaryFn, head, None];
  If[summary =!= None,
    summary = summary @ If[count == $n, $values, Pick[$values, $heads, head]];
  ];
  countStr = SPadLeft[If[count == $n, "", IntStr @ count], $countPadding];
  headStr = SPadRight[head, 4];
  color = Lookup[$headToColor, head, Black];
  boxes = SJoin[colorStr[headStr, color], " ", colorStr[countStr, $Gray]];
  If[summary =!= None,
    summaryBoxes = ToBoxes @ niceGrid @ KVMap[{Style[#, Bold], #2}&, summary];
    boxes = NiceTooltipBoxes[boxes, summaryBoxes, {500, 500}]
  ];
  boxes
];

colorStr[s_, c_] := ToLinearSyntax[Style[s, c]];

$headToColor = <|
  "int" -> $DarkGreen,
  "real" -> $DarkBlue,
  "str" -> $DarkRed,
  "bool" -> $DarkPurple,
  "null" -> GrayLevel[0.9]
|>;

$summaryFn = <|
  "int"    -> intSummary,
  "real"   -> realSummary,
  "str"    -> strSummary,
  "bool"   -> boolSummary,
  "list"   -> listSummary
|>;

intSummary[v_] := Scope[
  {min, max} = MinMax @ v;
  counts = Counts @ v;
  distinct = Len @ counts;
  If[distinct > 1024, Return @ PackAssociation[min, max, distinct]];
  values = KVMap[
    Underscript,
    Take[Select[# > 1&] @ ReverseSort[counts], UpTo[5]]
  ];
  PackAssociation[min, max, distinct, values]
];

realSummary[v_] := Scope[
  {min, max} = MinMax @ v;
  mean = Mean @ v;
  PackAssociation[min, max, mean]
];

strSummary[v_] := Scope[
  lens = SLen @ v;
  {minLen, maxLen} = MinMax @ lens;
  shortest = trimStr @ Part[v, IndexOf[lens, minLen]];
  longest = trimStr @ Part[v, IndexOf[lens, maxLen]];
  PackAssociation[minLen, maxLen, shortest, longest]
];

trimStr[str_] := Scope[
  str = ToString[str, InputForm];
  If[SLen[str] > 40, str = InsertLinebreaks[STake[str, UpTo[40 * 6]], 40]];
  str
];

boolSummary[v_] := Scope[
  true = Count[v, True];
  false = $n - true;
  fraction = N[true / $n];
  PackAssociation[true, false, fraction]
]

listSummary[v_] := Scope[
  lens = Len /@ v;
  {minLen, maxLen} = MinMax @ lens;
  shortest = Part[v, IndexOf[v, minLen]];
  longest = Part[v, IndexOf[v, maxLen]];
  PackAssociation[minLen, maxLen, shortest, longest]
];

