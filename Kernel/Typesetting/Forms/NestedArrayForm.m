PublicForm[NestedArrayForm]

(* PublicOption[AxisDirection, AxisDelimiters]

PublicHead[RepeatedSpec]

Options[NestedArrayForm] = {
  AxisDirection -> Automatic,
  AxisDelimiters -> Automatic,
  AxisPadding -> None
}
*)

DefineStandardTraditionalForm[
  na_NestedArrayForm :> nestedArrayBoxes[na]
];

(* TODO: make DefineStringBlockForm that automatically sets this up *)
NestedArrayForm /: CopyToClipboard[na_NestedArrayForm] :=
  CopyUnicodeToClipboard @ nestedArrayString @ na;

NestedArrayForm /: Normal[na_NestedArrayForm] := nestedArrayRender[na];

nestedArrayBoxes[na_NestedArrayForm] :=
  ToBoxes @ StringBlockForm @ nestedArrayRender @ na;

(**************************************************************************************************)

PublicVariable[$AxisPalette, $NestedArrayFrame]

SetInitialValue[$NestedArrayFrame, "["];

NestedArrayForm::badaxiscolor = "`` is not a valid axis color."
toAxisColor = Case[
  n_Integer       := Part[ReplaceAutomatic[$axisPaletteSpec, Range[9]], n];
  color_ ? ColorQ := color;
  None            := None;
  other_          := ThrowMessage["badaxiscolor", other];
];

(**************************************************************************************************)

PrivateFunction[nestedArrayRender]

$defaultSpanning = True;
$axisPaletteSpec = Automatic;
$itemFunction = defaultItemFunction;

defaultItemFunction = Case[
  False  := "F";
  True   := "T";
  r_Real := TextString[NumberForm[r, 2]];
  other_ := other;
];

nestedArrayRender[NestedArrayForm[array_, axisSpec___, Frame -> spec_]] := Scope[
  $NestedArrayFrame = spec;
  nestedArrayRender @ NestedArrayForm[array, axisSpec]
];

nestedArrayRender[NestedArrayForm[array_, axisSpec___, AxisPalette -> spec_]] := Scope[
  $axisPaletteSpec = spec;
  nestedArrayRender @ NestedArrayForm[array, axisSpec]
];

nestedArrayRender[NestedArrayForm[array_, axisSpec___, SpanningFrame -> spec_]] := Scope[
  $defaultSpanning = TrueQ[spec];
  nestedArrayRender @ NestedArrayForm[array, axisSpec]
];

nestedArrayRender[NestedArrayForm[array_, axisSpec___, ItemFunction -> itemFn_]] := Scope[
  $itemFunction = itemFn;
  nestedArrayRender @ NestedArrayForm[array, axisSpec]
];

nestedArrayRender[NestedArrayForm[array_]] := Scope[
  spec = Switch[ArrayDepth @ array,
    1, {"Row" -> 1},
    2, {"Grid" -> {1, 2}},
    3, {"Column" -> 1, "Grid" -> {2, 3}},
    4, {"Grid" -> {1, 2}, "Grid" -> {3, 4}}
  ];
  nestedArrayRender @ NestedArrayForm[array, Sequence @@ spec]
];

nestedArrayRender[NestedArrayForm[array_, axisSpec___]] := CatchMessage[
  NestedArrayForm,
  Block[{$dims = {}, $pos = {}, $hor = {}}, procNA[axisSpec] @ array]
];

ClearAll[procNA];

procNA[Row | "Row", rest___][array_] := If[!ListQ[array], array, Row[procNA[rest] /@ array, " "]];

procNA[Column | "Column", rest___][array_] := If[!ListQ[array], array, Column[procNA[rest] /@ array]];

$headP = ListForm | TupleForm | SetForm;

procNA[(head:$headP)[spec_], rest___][array_] :=
  head @ procNA[spec, rest] @ array;

procNA[(styleHead_Symbol ? StyleFormHeadQ)[(head:$headP)[inner_]], rest___][array_] :=
  StyleDecorated[StyleFormData @ styleHead, head][procNA[inner, rest] @ array];

shouldSpanQ[s_String] := Which[
  StringStartsQ[s, "Spanning"], True,
  StringStartsQ[s, "Normal"], False,
  True, $defaultSpanning
];

procNA[(t:"Row"|"SpanningRow"|"NormalRow"|"Column"|"NormalColumn"|"SpanningColumn"), rest___] :=
  procNA[t -> None, rest];

procNA[(t:"Grid"|"NormalGrid"|"SpanningGrid"), rest___] :=
  procNA[t -> {None, None}, rest];

procNA["Parentheses"|"Paren", rest___] :=
  procNA["Parentheses" -> None, rest];

procNA["LeftParentheses"|"LeftParen", rest___] :=
  procNA["LeftParentheses" -> None, rest];

procNA["Parentheses"|"Paren" -> col_, rest___][array_] :=
  StyleDecorated[col, ParenthesesForm] @ procNA[rest] @ array;

procNA["LeftParentheses"|"LeftParen" -> col_, rest___][array_] :=
  StringFrame[procNA[rest] @ array, Frame -> "(", FrameStyle -> col];

procNA[StringForm[str_, args___], rest___][array_] :=
  StringForm[str, args, procNA[rest] @ array];

NestedArrayForm::notlist = "`` is not a list, but `` was specified.";

autoRightPadding[] := If[$NestedArrayFrame =!= "[", 0, If[And @@ Pick[ThreadEqual[$pos, $dims], $hor], 0, 1]];
autoRightPadding[___] := 0;

procNA[(t:"Row"|"SpanningRow"|"NormalRow") -> col_, rest___][array_] :=
  StringRow[
    If[!ListQ[array], ThrowMessage["notlist", array, t]];
    vectorMapNA[procNA[rest], array, True],
    ColumnSpacings -> third[col, If[SeqLength[rest] == 0, 1, 0]],
    Frame -> $NestedArrayFrame, FrameStyle -> toAxisColor @ first[col, col],
    FramePadding -> {Left -> second[col, 0], Right -> second[col, autoRightPadding[rest]]},
    SpanningFrame -> shouldSpanQ[t],
    Background -> fourth[col, None]
  ];

procNA[(t:"Column"|"SpanningColumn"|"NormalColumn") -> col_, rest___][array_] :=
  StringColumn[
    If[!ListQ[array], ThrowMessage["notlist", array, t]];
    vectorMapNA[procNA[rest], array, False],
    Frame -> $NestedArrayFrame, FrameStyle -> toAxisColor @ first[col, col],
    RowSpacings -> third[col, 0],
    FramePadding -> {Left -> second[col, 0], Right -> second[col, autoRightPadding[rest]]},
    SpanningFrame -> shouldSpanQ[t],
    Background -> fourth[col, None]
  ];

NestedArrayForm::notmatrix = "`` is not a matrix, but `` was specified.";

procNA[(t:"ReverseGrid"|"ReverseSpanningGrid"|"ReverseNormalGrid") -> spec_, rest___][array_] :=
  procNA[StringTrimLeft[t, "Reverse"] -> spec, rest] @ Transpose @ array;

procNA[(t:"Grid"|"SpanningGrid"|"NormalGrid") -> {col1_, col2_, col3_:None}, rest___][array_] :=
  StringMatrix[
    If[!AnyMatrixQ[array], ThrowMessage["notmatrix", array, t]];
    matrixMapNA[procNA[rest], array],
    RowSpacings -> third[col1, 0],
    ColumnSpacings -> third[col2, If[SeqLength[rest] == 0, 1, 0]],
    Frame -> $NestedArrayFrame, FrameStyle -> toAxisColor @ first[col1, col1],
    RowFrames -> $NestedArrayFrame, RowFrameStyle -> toAxisColor @ first[col2, col2],
    FramePadding -> {Left -> second[col1, 0], Right -> second[col1, autoRightPadding[rest]]},
    RowFramePadding -> second[col2, 0],
    SpanningFrame -> shouldSpanQ[t],
    Background -> col3
  ];

first[{a_, ___}, _] := a;         first[_, e_] := e;
second[{a_, b_, ___}, _] := b;    second[_, e_] := e;
third[{a_, b_, c_}, _] := c;      third[_, e_] := e;
fourth[{a_, b_, c_, d_}, _] := d; fourth[_, e_] := e;

procNA[][e_] := $itemFunction @ e;

NestedArrayForm::badspec = "Unknown spec ``";
procNA[spec_, ___][array_] := (Message[NestedArrayForm::badspec, MsgExpr @ spec]; "?");

vectorMapNA[f_, array_, isH_] := InheritedBlock[
  {$dims = Append[$dims, Length @ array], $pos = Append[$pos, 0], $hor = Append[$hor, isH]},
  MapIndex1[{a, i} |-> (Part[$pos, -1] = i; f[a]), array]
];

matrixMapNA[f_, array_] := InheritedBlock[
  {$dims = Join[$dims, Dimensions[array, 2]], $pos = Join[$pos, {0, 0}], $hor = Join[$hor, {False, True}]},
  MapIndexed[{a, i} |-> (Part[$pos, {-2, -1}] = i; f[a]), array, {2}]
];

(* unpackSpec[d_][list_List] := PadRight[list, d, Last @ list];
unpackSpec[d_][RepeatedSpec[elems__]] := Take[Catenate @ ConstantArray[{elems}, d], d];
unpackSpec[d_][e_] := ConstantArray[e, d];
 *)