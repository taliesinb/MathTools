
SetHoldAllComplete[AlgebraicRow, AlgebraicGrid, algebraicForm, algebraicGridRow];

PackageExport["AlgebraicRow"]

AlgebraicRow[elements___] := Scope[
  SpacedRow @ Map[algebraicForm, Unevaluated @ {elements}]
];

(**************************************************************************************************)

PackageExport["AlgebraicGrid"]

AlgebraicGrid[elements___] := Scope[
  Grid[
    Map[algebraicGridRow, Unevaluated @ {elements}],
    Spacings -> {2, 1.5}
  ]
];

algebraicGridRow[e_] := algebraicGridRow @ {e};
algebraicGridRow[row_List] := Map[algebraicForm, Unevaluated @ row]

(**************************************************************************************************)

algebraicForm[e_] := Labeled[e, symbolicForm[e]];

$symbolicHeadToSymbol = {
  CenterDot | PathCompose -> "\[CenterDot]",
  PathTranslate | UpArrow -> "\[UpArrow]",
  PathReverseTranslate | DownArrow -> "\[DownArrow]",
  TranslateAdd | CirclePlus -> "\[CircleTimes]",
  TranslateSubtract | CircleMinus -> "\[CircleMinus]",
  Plus -> "+",
  Times -> "\[Times]",
  Minus -> "\[Minus]"
};

(**************************************************************************************************)

SetHoldAllComplete[symbolicForm, Parentheses, Grouped];
Clear[symbolicForm];

$Delta = "\[CapitalDelta]";

symbolicForm = Case[
  (a_Symbol = expr_) := symbolForm[HoldSymbolName @ a];
  a_Symbol := symbolForm[HoldSymbolName @ a];
  Times[-1, a_] := Row[{"\[Minus]", % @ a}];
  Times[n_ ? HoldNumericQ, a_] := Row[{n, % @ a}];
  Plus[args___] := sumForm[{args}];
  Parentheses[e_] := Row[{"(", %[e], ")"}];
  Grouped[e_] := %[e];
  PathForwardDifference[t_, p_] := Row[{Subsuperscript[$Delta, % @ t, "+"], % @ p}];
  PathBackwardDifference[t_, p_] := Row[{Subsuperscript[$Delta, % @ t, "\[Minus]"], % @ p}];
  PathCentralDifference[t_, p_] := Row[{Subscript[$Delta, % @ t], % @ p}];
  (head:symbolicHeads)[args___] := Row[Map[%, Unevaluated[{args}]], Replace[head, $symbolicHeadToSymbol]];
  PathHeadVector[a_] := "head"[% @ a];
  PathTailVector[a_] := "tail"[% @ a];,
  {symbolicHeads -> Apply[Alternatives, Cases[Keys[$symbolicHeadToSymbol], _Symbol, {1,2}]]}
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