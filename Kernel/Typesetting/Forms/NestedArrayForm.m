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

PrivateFunction[nestedArrayRender]

nestedArrayRender[NestedArrayForm[array_, axisSpec___]] :=
  procNA[axisSpec] @ array;

ClearAll[procNA];

procNA[Row | "Row", rest___][array_] := If[!ListQ[array], array, Row[procNA[rest] /@ array, " "]];

procNA[Column | "Column", rest___][array_] := If[!ListQ[array], array, Column[procNA[rest] /@ array]];

$headP = ListForm | TupleForm | SetForm;

procNA[(head:$headP)[spec_], rest___][array_] :=
  head @ procNA[spec, rest] @ array;

procNA[(styleHead_Symbol ? StyleFormHeadQ)[(head:$headP)[inner_]], rest___][array_] :=
  StyleDecorated[StyleFormData @ styleHead, head][procNA[inner, rest] @ array];

procNA[Grid | "Grid", rest___] := procNA[{Grid, RowSpacings -> 1}, rest];
procNA[Grid | "Grid"] := procNA[{Grid}];

procNA[{Grid |"Grid", opts___Rule}, rest___][array_] := If[!MatrixQ[array, True&], procNA["Row"] @ array,
  StringMatrix[MatrixMap[procNA[rest], array], opts, RowSpacings -> 0, ColumnSpacings -> 1]
];

procNA[(t:"Row"|"SpanningRow") -> col_, rest___][array_] :=
  StringRow[
    Map[procNA[rest], array],
    ColumnSpacings -> If[SeqLength[rest] == 0, 1, 0],
    Frame -> "[]", FrameStyle -> col,
    SpanningFrame -> (t == "SpanningRow")
  ];

procNA[(t:"Column"|"SpanningColumn") -> col_, rest___][array_] :=
  StringColumn[
    Map[procNA[rest], array],
    RowSpacings -> If[SeqLength[rest] == 0, 0, If[(t == "SpanningColumn"), 1, 0]],
    Frame -> "[]", FrameStyle -> col,
    SpanningFrame -> (t == "SpanningColumn")
  ];

procNA[(t:"Grid"|"SpanningGrid") -> {col1_, col2_}, rest___][array_] :=
  StringMatrix[
    MatrixMap[procNA[rest], array],
    ColumnSpacings -> If[SeqLength[rest] == 0, 1, 0],
    RowSpacings ->    If[SeqLength[rest] == 0 && (t == "SpanningGrid"), 1, 0],
    Frame -> "[]", FrameStyle -> col1,
    RowFrames -> "[]", RowFrameStyle -> col2,
    SpanningFrame -> (t == "SpanningGrid")
  ];

procNA[] := Identity;

NestedArrayForm::badspec = "Unknown spec ``";
procNA[spec_, ___][array_] := (Message[NestedArrayForm::badspec, MsgExpr @ spec]; "?");

(* unpackSpec[d_][list_List] := PadRight[list, d, Last @ list];
unpackSpec[d_][RepeatedSpec[elems__]] := Take[Catenate @ ConstantArray[{elems}, d], d];
unpackSpec[d_][e_] := ConstantArray[e, d];
 *)