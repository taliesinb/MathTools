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

PublicVariable[$AxisPalette]

$AxisPalette = {1, 2, 3, 4, 5, 6};

NestedArrayForm::badaxiscolor = "`` is not a valid axis color."
toAxisColor = Case[
  n_Integer       := Part[ReplaceAutomatic[$axisPaletteSpec, $AxisPalette], n];
  color_ ? ColorQ := color;
  None            := None;
  other_          := ThrowMessage["badaxiscolor", other];
];

(**************************************************************************************************)

PrivateFunction[nestedArrayRender]

$defaultSpanning = True;
$axisPaletteSpec = Automatic;

nestedArrayRender[NestedArrayForm[array_, axisSpec___, AxisPalette -> spec_]] := Scope[
  $axisPaletteSpec = spec;
  nestedArrayRender @ NestedArrayForm[array, axisSpec]
];

nestedArrayRender[NestedArrayForm[array_, axisSpec___, SpanningFrame -> spec_]] := Scope[
  $defaultSpanning = TrueQ[spec];
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
  procNA[axisSpec] @ array
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

NestedArrayForm::notlist = "`` is not a list, but `` was specified.";

procNA[(t:"Row"|"SpanningRow"|"NormalRow") -> col_, rest___][array_] :=
  StringRow[
    If[!ListQ[array], ThrowMessage["notlist", array, t]];
    Map[procNA[rest], array],
    ColumnSpacings -> third[col, If[SeqLength[rest] == 0, 1, 0]],
    Frame -> "[]", FrameStyle -> toAxisColor @ first[col, col],
    FramePadding -> {Horizontal -> second[col, 0]},
    SpanningFrame -> shouldSpanQ[t]
  ];

procNA[(t:"Column"|"SpanningColumn"|"NormalColumn") -> col_, rest___][array_] :=
  StringColumn[
    If[!ListQ[array], ThrowMessage["notlist", array, t]];
    Map[procNA[rest], array],
    Frame -> "[]", FrameStyle -> toAxisColor @ first[col, col],
    RowSpacings -> third[col, 0],
    FramePadding -> {Horizontal -> second[col, 0]},
    SpanningFrame -> shouldSpanQ[t]
  ];

NestedArrayForm::notmatrix = "`` is not a matrix, but `` was specified.";

procNA[(t:"Grid"|"SpanningGrid"|"NormalGrid") -> {col1_, col2_}, rest___][array_] :=
  StringMatrix[
    If[!MatrixQ[array, True&], ThrowMessage["notmatrix", array, t]];
    MatrixMap[procNA[rest], array],
    RowSpacings -> third[col1, 0],
    ColumnSpacings -> third[col2, If[SeqLength[rest] == 0, 1, 0]],
    Frame -> "[]", FrameStyle -> toAxisColor @ first[col1, col1],
    RowFrames -> "[]", RowFrameStyle -> toAxisColor @ first[col2, col2],
    FramePadding -> {Horizontal -> second[col1, 0]},
    RowFramePadding -> second[col2, 0],
    SpanningFrame -> shouldSpanQ[t]
  ];

first[{a_, ___}, _] := a;      first[_, e_] := e;
second[{a_, b_, ___}, _] := b; second[_, e_] := e;
third[{a_, b_, c_}, _] := c;   third[_, e_] := e;

procNA[] := Identity;

NestedArrayForm::badspec = "Unknown spec ``";
procNA[spec_, ___][array_] := (Message[NestedArrayForm::badspec, MsgExpr @ spec]; "?");

(* unpackSpec[d_][list_List] := PadRight[list, d, Last @ list];
unpackSpec[d_][RepeatedSpec[elems__]] := Take[Catenate @ ConstantArray[{elems}, d], d];
unpackSpec[d_][e_] := ConstantArray[e, d];
 *)