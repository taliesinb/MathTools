PackageExport["MultiplicationForm"]

SetUsage @ "
MultiplicationForm is an option to %AlgebraicRow and %AlgebraicGrid.
"

PackageExport["Headings"]

SetUsage @ "
Headings is an option to %AlgebraicRow and %AlgebraicGrid.
"

(**************************************************************************************************)

PackageExport["AlgebraicColumn"]

SetHoldAllComplete[AlgebraicColumn];

$arColumn = False;
AlgebraicColumn[args___] := Scope[$arColumn = True; AlgebraicRow[args]];

(**************************************************************************************************)

SetHoldAllComplete[AlgebraicRow, AlgebraicGrid, algebraicForm, algebraicGridRow];

PackageExport["AlgebraicRow"]

Options[AlgebraicRow] = {
  MultiplicationForm -> "Dot"
};

$mForm = None;

AlgebraicRow[elements__,  MultiplicationForm -> t:("Dot"|"Times"|None)] := Block[{$mForm = t},
  AlgebraicRow[elements]
];

AlgebraicRow[elements__] :=
  If[$arColumn, SpacedColumn, SpacedRow][
    Map[algebraicForm, Unevaluated @ {elements}],
    LabelStyle -> $MathLabelStyle,
    LabelPosition -> After,
    Alignment -> If[$arColumn, Left, Center]
  ]

(**************************************************************************************************)

PackageExport["AlgebraicGrid"]

Options[AlgebraicGrid] = {
  Headings -> None,
  MultiplicationForm -> "Dot",
  Transposed -> False
};

$headings = None;
$agTransposed = False;

AlgebraicGrid[elements__, Transposed -> t_] := Block[
  {$agTransposed = t},
  AlgebraicGrid[elements]
];


AlgebraicGrid[elements__, Headings -> h_] := Block[
  {$headings = h},
  AlgebraicGrid[elements]
];

AlgebraicGrid[elements__, MultiplicationForm -> t:("Dot"|"Times"|None)] := Block[
  {$mForm = t},
  AlgebraicGrid[elements]
];

AlgebraicGrid[elements__] := Scope[
  entries = Map[algebraicGridRow, Unevaluated @ {elements}];
  If[$headings =!= None, PrependTo[entries, LabelForm[#, Bold]& /@ $headings]];
  Grid[
    If[$agTransposed, Transpose, Identity] @ entries,
    Spacings -> {2, 1.5},
    Alignment -> {Center, Center},
    BaseStyle -> $MathLabelStyle
  ]
];

algebraicGridRow[e_] := algebraicGridRow @ {e};
algebraicGridRow[row_List] := Map[algebraicForm, Unevaluated @ row]

(**************************************************************************************************)

algebraicForm[span:SpanFromAbove|SpanFromLeft] := span;
algebraicForm[s_Spacer] := s;
algebraicForm[Text[t_]] := t;
algebraicForm[s_String] := Labeled[LabelForm[s], ""];
algebraicForm[e_] := Labeled[e, toSymbolicForm @ e];
algebraicForm[Labeled[e_, l_]] := Labeled[e, toSymbolicForm @ l];
algebraicForm[Style[e_, s__]] := Labeled[e, Style[toSymbolicForm @ e, s]];

$mDot = Style["\[CenterDot]", $LightGray];
$mTimes = Style["\[Times]", $LightGray];

$mSymbol := Switch[$mForm,
  "Dot",   $mDot,
  "Times", $mTimes,
  None,    "\[ThinSpace]"
];

$symbolicHeadToSymbol = {
  CenterDot | PathCompose -> "\[CenterDot]",
  PathTranslate | UpArrow -> "\[UpArrow]",
  PathReverseTranslate | DownArrow -> "\[DownArrow]",
  TranslateAdd | CirclePlus -> "\[CircleTimes]",
  TranslateSubtract | CircleMinus -> "\[CircleMinus]",
  Plus -> "+",
  Times :> $mSymbol,
  Minus -> "\[Minus]"
};

$symbolicHeadsP = Apply[Alternatives, Cases[Keys[$symbolicHeadToSymbol], _Symbol, {1,2}]];

(**************************************************************************************************)

SetHoldAllComplete[Parentheses, Grouped];

SetHoldAllComplete[toSymbolicForm];
toSymbolicForm[e_] :=
  Apply[symbolicForm, HoldComplete[e] //. $preEvaluate] //. $postEvaluate;

$preEvaluate = {
  (f_Function)[arg_] :> RuleCondition @ evalFuncHold[f, arg],
  Hold[h_] :> h
};

SetHoldAllComplete[evalFuncHold];
evalFuncHold[Verbatim[Function][x_, body_], arg_] :=
  Function[x, Hold @ body, {HoldFirst}] @ Unevaluated[arg];

evalFuncHold[Verbatim[Function][body_], arg_] :=
  Function[Null, Hold @ body, {HoldFirst}] @ Unevaluated[arg];

$postEvaluate = {
  Superscript[Subscript[z_, sub_], sup_] :> Subsuperscript[z, sub, sup],
  Subscript[Superscript[z_, sup_], sub_] :> Subsuperscript[z, sub, sup]
};

parenForm[e_] := Row[{$parenL, e, $parenR}];
$parenL = Style["(", $Gray];
$parenR = Style[")", $Gray];

SetHoldAllComplete[headForm]
headForm[head_, args_] :=
  Row[
    Map[possiblyParenSymbolicForm, Unevaluated @ args],
    Replace[head, $symbolicHeadToSymbol]
  ];

$heavyHeadP = PathForwardDifference | PathBackwardDifference | PathCentralDifference;
SetHoldAllComplete[possiblyParenSymbolicForm]
possiblyParenSymbolicForm = Case[
  sym_Symbol      := symbolicForm @ sym;
  t:Verbatim[Times][__Symbol] := symbolicForm @ t;
(*   p_Plus        := symbolicForm @ Parentheses @ p;
  p:(_[_, _])   := symbolicForm @ Parentheses @ p;
  p:($symbolicHeadsP[___]) := symbolicForm @ Parentheses @ p;
 *)
 other_        := symbolicForm @ Parentheses @ other
];

SetHoldAllComplete[symbolicForm];
symbolicForm = Case[
  (a_ = b_) := Row[{% @ a, " = ", % @ b}];

  a_Symbol := symbolForm[HoldSymbolName @ a];

  Times[-1, a_] := Row[{"\[Minus]", % @ a}];

  (* we never use an explicit times for unambiguous multiplication *)
  Times[a_Symbol, b_Symbol, c_Symbol] := Row[{% @ a, % @ b, % @ c}, "\[VeryThinSpace]"];
  Times[a_Symbol, b_Symbol] := Row[{% @ a, % @ b}, "\[VeryThinSpace]"];

  (* Verbatim[Times][args__] := timesForm[{args}]; *)
  Plus[args___] := sumForm[{args}];
  Parentheses[e_] := parenForm @ %[e];
  Grouped[e_] := %[e];
  PathReverse[t_] := SuperDagger[% @ t];

  Subsuperscript[a_, b_, c_] := Subsuperscript[% @ a, % @ b, % @ c];
  Subscript[a_, b_] := Subscript[% @ a, % @ b];

  WordVector[e_, type_String:"Forward"] := Subsuperscript["e", fmtWord @ e, fmtType @ type];

  PathForwardDifference[t_, p_] := differenceForm[t, "+", p];
  PathBackwardDifference[t_, p_] := differenceForm[t, "\[Minus]", p];
  PathCentralDifference[t_, p_] := differenceForm[t, None, p];

  PathGradient[p_] := Row[{"\[Gradient]", "\[VeryThinSpace]", % @ p}];

  (head:symbolicHeads)[t_ ? isSuppressed, a_] :=
    Row[{Replace[head, $symbolicHeadToSymbol], possiblyParenSymbolicForm @ a}];

  (head:symbolicHeads)[args___] := headForm[head, {args}];
  PathHeadVector[a_] := Superscript[% @ a, "\[FilledSmallCircle]"];
  PathTailVector[a_] := Subscript[% @ a, "\[FilledSmallCircle]"];
  {symbolicHeads -> $symbolicHeadsP}
];

SetHoldAllComplete[differenceForm];
differenceForm[a:{(_ ? isSuppressed)..}, type_, p_] :=
  Row[{supscript[Subscript[$Delta, HoldLength @ a], type], symbolicForm @ p}];

differenceForm[t_ ? isSuppressed, type_, p_] :=
  Row[{supscript[$Delta, type], symbolicForm @ p}];

differenceForm[t_Symbol | t_List, type_, p_] :=
  Row[{supscript[Subscript[$Delta, subscriptForm @ t], type], symbolicForm @ p}];

differenceForm[t_, type_, p_] :=
  Row[{symbolicForm @ t, supscript[$Delta, type], symbolicForm @ p}];

supscript[e_, None] := e;
supscript[e_, s_] := Superscript[e, s];
supscript[Subscript[e_, sub1_], sub2_] := Subsuperscript[e, sub1, sub2];

fmtWord = Case[
  s_String        := toRow[Map[If[UpperCaseQ[#], Negated @ ToLowerCase @ #, #]&, Characters @ s]];
  s_String -> -1  := Row[{"\[Minus]", fmtWord @ s}];
  list_List       := Row[fmtWord /@ list, "+"];
  _               := "?"
];

toRow[{e_}] := e;
toRow[list_List] := Row[list];

fmtType = Case[
  "Forward" := "f";
  "Backward" := "b";
  "Symmetric" := "s";
  "Antisymmetric" := "a";
];

$Delta = Style["\[DifferenceDelta]", FontFamily -> "Times"];

SetHoldAllComplete[isSuppressed];
isSuppressed[e_Symbol] := StringEndsQ[HoldSymbolName @ e, "$"];
isSuppressed[_] := False;

SetHoldAllComplete[heavyQ];
heavyQ[s_Symbol] := False;
heavyQ[_] := True;

SetHoldAllComplete[subscriptForm];
subscriptForm = Case[
  a_Symbol := symbolForm[HoldSymbolName @ a];
  s_String := s;
  list_List := Row[Map[%, Unevaluated @ list], ","];
  e_ := symbolicForm @ e;
];

(**************************************************************************************************)

PackageExport["Parentheses"]

Parentheses[e_] := e;

(**************************************************************************************************)

PackageExport["Grouped"]

Grouped[e_] := e;

(**************************************************************************************************)

Clear[sumTermForm];
SetHoldAll[sumForm, sumTermForm, HoldNumericQ];
sumForm[args_] := Scope[
  res = Flatten @ Map[sumTermForm, Unevaluated[args]];
  If[First[res] === "\[ThinSpace]+\[ThinSpace]", res //= Rest];
  Row[res, "\[ThinSpace]"]\[ThinSpace]
];

HoldNumericQ[n_] := NumericQ[Unevaluated @ n];

sumTermForm[Times[-1, a_]] := {"\[ThinSpace]\[Minus]\[ThinSpace]", symbolicForm @ a};
sumTermForm[Times[n_ ? HoldNumericQ, a_]] := {n, symbolicForm @ a};
sumTermForm[a_] := {"\[ThinSpace]+\[ThinSpace]", symbolicForm @ a};

symbolForm[name_String] := Which[
  StringEndsQ[name, DigitCharacter], Subscript[StringDrop[name, -1], StringTake[name, -1]],
  StringMatchQ[name, __ ~~ "$" ~~ __], Subscript @@ StringSplit[name, "$"],
  True, name
];