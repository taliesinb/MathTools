SetUsage @ "
DataFrame[{<|'key$1' -> col$1, 'key$2' -> col$2, $$}] constructs a data frame from a list of assocations.
DataFrame[{'key$1', 'key$2', $$} -> {col$1, col$2, $$}] constructs a data frame from columns and their names.
DataFrame[<|pkey$1 -> row$1, pkey$2 -> row$2, $$|>] constructs a data frame with primary keys.
DataFrame[{row$1, row$2, $$}] constucts a dataframe with integer column names.
* selection of rows produces a %DataSelection[$$] object.
* selection of columns produces a %DataColumn[$$] object.
";

DataFrame::stringKeys = "Keys `` must be strings!";
DataFrame::notLists = "Columns should be equal-length lists.";
DataFrame::badData = "Bad data with head ``.";
DataFrame::keysColsLength = "Number of keys `` =!= number of columns ``."
DataFrame::ragged = "Length of columns aren't equal: ``."
DataFrame::noColumns = "DataFrame can't have zero columns.";

DataFrame[keys_List -> cols_List] := Scope[
  makeDataFrame[keys, cols]
];

DataFrame[assoc_Association] := Scope @ CatchMessage[
  {primaryKeys, data} = KeysValues @ assoc;
  {keys, cols} = toKeysCols @ data;
  makeDataFrame[Pre["Key", keys], Pre[primaryKeys, cols]]
];

DataFrame[data_] := Scope @ CatchMessage[
  makeDataFrame @@ toKeysCols @ data
];

(**************************************************************************************************)

makeDataFrame[{}, _] := ThrowMessage["noColumns"];

makeDataFrame[keys_, cols_] := Scope[
  If[!LengthEqualOrMessage["keysColsLength", keys, cols], ReturnFailed[]];
  lens = Len /@ cols;
  If[!AllEqualQ[lens], ThrowMessage["ragged", RuleThread[keys, lens]]];
  cols = ToPacked /@ cols;
  ConstructNoEntry[DataFrame, keys, cols, F @ lens]
];

(**************************************************************************************************)

PublicFunction[DataFrameQ]

DataFrameQ[HoldP[DataFrame[_List, _List, _Int] ? HoldNoEntryQ]] := True;
DataFrameQ[_] := False;

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
  Normal[$]              := MapThread[AssocThread[$K, #]&, $V];
  Keys[$]                := $K;
  Values[$]              := $V;
  KeysValues[$]          := {$K, $V};
  Dims[$]          := {$N, Len @ $K};
  Len[$]                 := $N;
  RandomChoice[$]        := Scope[
    i = RandomInteger[{1, $N}];
    AssocThread[$K, Part[$V, All, i]]
  ];
  Part[$, spec__]        := dsPart[$K, $V, $N, spec];
  RandomSample[$, n_Int] := Scope[
    indices = RandomSample[range @ $N, n];
    If[!ListQ[indices], ReturnFailed[]];
    makeDataFrame[$K, Part[#, indices]& /@ $V]
  ];
  Map[fn_, $]               := dsDispatch[$K, $V, $N, Map, dsMap, fn];
  Scan[fn_, $]              := dsDispatch[$K, $V, $N, Scan, dsScan, fn];
  Select[$, fn_]            := dsDispatch[$K, $V, $N, Select, dsSelect, fn];
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

  Verbatim[Pattern][key_Symbol, rhs_] :=
    %[HoldSymbolName[key] -> rhs];

  key_Str -> val_ :=
    pickCol[key, val];

  fn_Function     := If[Count[fn, Slot[_Str], Inf] == 1,
    pickCol[DeepFirstCase[fn, Slot[s_Str] :> s], simplifyFn[fn /. _Slot -> Slot[]]],
    pickIndices @ normalMap @ fn
  ];

  list_List :=
    Inter @@ Map[%, list];
];


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

  Sampled[n_Int]                   := fromIndices @ If[$indices === All, RandomRandom[Range @ $N, n], RandomSample[$indices, n]];

  part:(_Span | {__Integer})       := If[$indices === All, fromIndices @ part, fromIndices @ SafePart[$indices, part]];

  RepPart[key_Str -> new_]     := Part[columnData @ key, $indices];



  key_Str                          := Part[columnData @ key, $indices];

  key_Str -> fn_                   := fn[%[key]];

  list_List                        := Map[%, list];

  fn_Function                      := MapThread[transformSlots @ fn, Part[$V, All, $indices]];

  i_Integer                        := getPartI @ Which[
    $indices === All,             i,
    i > Len[$indices],            L @ $indices,
    i < -Len[$indices] || i == 0, F @ $indices,
    True,                         Part[$indices, i]
  ];

];

(**************************************************************************************************)

pickIndices[bools_] := Pick[range @ $N, bools];
pickIndices[values_, patt_] := Pick[range @ $N, values, patt];
range[n_] := range[n] = Range @ $N;

rawMap[fn_] := MapThread[transformSlots @ fn, $V];
normalMap[fn_] := Map[normify @ fn, $V];
normify[fn_] := fn[AssocThread[$K, #]]&;

(**************************************************************************************************)

dsDispatch[k_, v_, n_, fn_, impl_, args___] := Scope @ CatchMessage[fn,
  $K = k; $V = v; $N = n;
  impl[args]
];

(**************************************************************************************************)

transformSlots[f_] := RepAll[f, Slot[k_Str] :> RuleEval[Slot @ getColIndex @ k]];
decondition[f_] := RepAll[f, Verbatim[Condition][body_, test_] :> If[TrueQ[test], body, Nothing]]

dsMap[fn:Fn[_]] := rawMap @ decondition @ fn;
dsMap[fn_] := normalMap @ fn;

dsScan[fn:Fn[_]] := rawMap @ nullify @ fn;
dsScan[fn_] := Scan[normify @ fn, $V];
nullify[Fn[c_]] := Fn[c;];
nullify[f_] := f;

dsSelect[fn:Fn[_]] := fromIndices @ pickIndices @ rawMap @ fn;
dsSelect[fn_] := newDataFrame[$K, Transpose @ Values @ Select[$V, normify @ fn]];

fromIndices[indices_] := newDataFrame[$K, Part[$V, All, indices]];

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

columnData[k_] := Part[$V, getColIndex @ k];

getColIndex[k_] := IndexOf[$K, k, badColumn[k]];

General::noDataFrameColumn = "DataFrame does not have a column called ``, available: ``."
badColumn[k_] := ThrowMessage["noDataFrameColumn", k, $K];

(**************************************************************************************************)

General::badPartSpec = "Unsupported part spec: ``."
part[spec__] := ThrowMessage["badPartSpec", {spec}];

newDataFrame[_, {}] := ThrowMessage[noColumns];
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

toKeysCols := Case[
  data_List ? AssociationVectorQ := Scope[
    keys = Union @@ (Keys /@ data);
    If[!StrVecQ[keys], ThrowMessage["stringKeys", MsgExpr @ keys]];
    cols = Lookup[data, Key[#], None]& /@ keys;
    {keys, cols}
  ];
  data_List ? ListVectorQ := Scope[
    dims = Dims @ data;
    If[Len[dims] == 1, ThrowMessage["notLists"]];
    keys = IntStr @ Range @ P2 @ dims;
    {keys, Transpose @ data}
  ];
  other_ := ThrowMessage["badData", MsgExpr @ H @ data];
];

(**************************************************************************************************)

DefineStandardTraditionalForm[
  HoldP[DataFrame[k_List, v_List, n_Integer]] ? HoldNoEntryQ :> dataFrameBoxes[k, v, n]
];

dataFrameBoxes[k_, v_, n_] := Scope[
  $n = n; $countPadding = SLen @ IntStr @ n;
  gridEntries = ZipMap[colSummary, k, v];
  grid = niceGrid @ gridEntries;
  ToBoxes @ Labeled[grid, Row[{n, " rows"}, BaseStyle -> {FontSize -> 12, FontFamily -> "Fira Code"}]]
];

niceGrid[entries_] := Grid[
  entries, Background -> GrayLevel[0.95],
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
  {Style[key, Bold], RawBoxes @ RowBox[entries]}
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
  boxes = SJoin[colorStr[headStr, color], " ", colorStr[countStr, $LightGray]];
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
  "bool" -> $DarkPurple
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

