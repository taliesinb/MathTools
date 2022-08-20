PublicFunction[MatrixGrid]
PublicFunction[ScalarGrid]
PublicFunction[ColumnVectorGrid]
PublicFunction[RowVectorGrid]

$axisStyle = {FontSize -> 13, FontFamily -> "Avenir", FontColor -> Black};
$rowsText = "rows";
$colsText = "cols";

$arrayFrameStyle = {AbsoluteThickness[2], $LightGray};

arrayGrid[array_, opts___] := MeshGrid[{0,0}, array, opts, ItemSize -> {2,2}, FrameStyle -> $arrayFrameStyle, MeshStyle -> $arrayFrameStyle]

MatrixGrid[array_, opts___] := arrayGrid[array, opts, AxesLabel -> {$rowsText, $colsText}, LabelStyle -> $axisStyle];

RowVectorGrid[vector_, opts___] := arrayGrid[List @ vector, opts, AxesLabel -> {None, $colsText}, LabelStyle -> $axisStyle];
ColumnVectorGrid[vector_, opts___] := arrayGrid[List /@ vector, opts, AxesLabel -> {$rowsText, None}, LabelStyle -> $axisStyle];

ScalarGrid[n_, opts___] := MeshGrid[{{n}}, FrameStyle -> Append[$arrayFrameStyle, Dotted], opts, ItemSize -> {2, 2}];

(**************************************************************************************************)

PublicFunction[SplitSquareBracketList]

SplitSquareBracketList[n_, fs_:16][list_] := Block[
  {$fs = fs}, toCol[toRows @ Partition[If[n > 1, padStr, Identity] @ list, UpTo[n]]]];

$comma = Style[",", $Gray];
$lbrac = DarkGrayForm @ RaiseForm["[",.09];
$rbrac = DarkGrayForm @ RaiseForm["]",.09];
$lbrac = Style["[", $DarkGray, Bold];
$rbrac = Style["]", $DarkGray, Bold];

padStr[a_List] := StringPadLeft[TextString /@ a];
toCol[{e_}] := Style[e, {FontSize -> $fs, FontFamily -> "Source Code Pro"}];
toCol[list_] := Column[list,
  Spacings -> 0.15, Alignment -> Left, BaselinePosition -> Scaled[0.5],
  BaseStyle -> {FontSize -> $fs, FontFamily -> "Source Code Pro"}
]
toRows[{a_}] := List @ Row @ Flatten @ {$lbrac, Riffle[a, $comma], $rbrac};
toRows[{a_, mid___, b_}] := Flatten @ {
  Row[Flatten @ {$lbrac, {#, $comma}& /@ a}],
  Row[Flatten @ {" ", {#, $comma}& /@ #}]& /@ {mid},
  Row[Flatten @ {" ", Riffle[b, $comma], $rbrac}]
};

PublicFunction[SquareBracketList]

SquareBracketList[list_, fs_:16] := Row[{$lbrac, Row[list, $comma], $rbrac}, BaseStyle -> {FontSize -> fs, FontFamily -> "Source Code Pro"}];


(**************************************************************************************************)

PublicForm[SymbolicPlus, SymbolicTimes]

SetListable[SymbolicPlus, SymbolTimes, SymbolicVerticalPlus, SymbolVerticalTimes, SymbolicDiagonalPlus, SymbolVerticalTimes];

MakeBoxes[SymbolicPlus[a___], form_] :=
  ToBoxes[Row[{a}, "\[NegativeThinSpace]+\[NegativeThinSpace]"], form];

MakeBoxes[SymbolicPlus[a_, b_ ? minusQ], form_] :=
  ToBoxes[Row[{a, abs @ b}, "\[NegativeThinSpace]-\[NegativeThinSpace]"], form];

minusQ[n_Integer ? Negative] := True;
minusQ[Style[_ ? minusQ, ___]] := True;
minusQ[_] := False;

abs[n_ ? NumberQ] := Abs[n];
abs[Style[a_, r___]] := Style[abs @ a, r];

MakeBoxes[SymbolicTimes[a___], form_] :=
  ToBoxes[Row[{a}, "\[Times]"], form];

PublicForm[SymbolicVerticalPlus, SymbolVerticalTimes]

gridOp[elems_, op_] :=
  Grid[ReplacePart[{op, #} & /@ elems, {1, 1} -> ""],
   RowSpacings -> 0.4, ColumnSpacings -> {0.1},
   ColumnAlignments -> Right];
MakeBoxes[SymbolicVerticalPlus[a___], form_] :=
  ToBoxes[gridOp[{a}, "+"], form];
MakeBoxes[SymbolVerticalTimes[a___], form_] :=
  ToBoxes[gridOp[{a}, "\[Times]"], form];

PublicForm[SymbolicDiagonalPlus, SymbolVerticalTimes]

SetListable[SymbolicDiagonalPlus, SymbolVerticalTimes];
MakeBoxes[SymbolicDiagonalPlus[a_, b_], form_] :=
  ToBoxes[Grid[{{a, "+"}, {"", b}}, RowSpacings -> 0.4,
    ColumnSpacings -> {0.1}, ColumnAlignments -> Right], form];
