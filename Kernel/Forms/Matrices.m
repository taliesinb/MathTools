PublicTypesettingForm[TupleSymbol]

DefineTaggedForm[TupleSymbol];

(**************************************************************************************************)

PublicTypesettingForm[TuplePartForm]

PublicFunction[PartBox]

PartBox[a_, rest___] := RBox[a, DoubleSquareBracketBox[rest]];

DefineBinaryForm[TuplePartForm, PartBox[$1, $2]];

(**************************************************************************************************)

PublicTypesettingForm[MatrixPartForm, SubMatrixPartForm]

DefineTernaryForm[MatrixPartForm, PartBox[$1, $2, $3]];
DefineTernaryForm[SubMatrixPartForm, PartBox[$1, $2, $3]];

(**************************************************************************************************)

PublicTypesettingForm[MatrixDotForm, MatrixPlusForm]

DefineInfixForm[MatrixDotForm, OpBox["\[CenterDot]"]];
DefineInfixForm[MatrixPlusForm, OpBox["+"]];

(**************************************************************************************************)

PublicTypesettingForm[MatrixSymbol]

DefineTaggedForm[MatrixSymbol];

(**************************************************************************************************)

PublicTypesettingForm[SmallMatrixForm, NormalMatrixForm, PMatrixForm, BMatrixForm, VMatrixForm]

DefineStandardTraditionalForm[{
  SmallMatrixForm[array_List]  :> makeMatrixGrid[array, "SmallMatrixForm", FontSize -> Smaller],
  NormalMatrixForm[array_List] :> makeMatrixGrid[array, "NormalMatrixForm"],
  PMatrixForm[array_List]      :> makeMatrixGrid[array, "PMatrixForm"],
  BMatrixForm[array_List]      :> makeMatrixGrid[array, "BMatrixForm"],
  VMatrixForm[array_List]      :> makeMatrixGrid[array, "VMatrixForm"]
}];

SetHoldAllComplete[matrixBoxes, matrixRowBoxes, makeMatrixGrid]
matrixRowBoxes[row_List] := MapUnevaluated[MakeMathBoxes, row];

makeMatrixGrid[array_, templateName_, style___] := TBox[GridBox[
  MapUnevaluated[matrixRowBoxes, array], BaseStyle -> {style, "MathFont"},
  ColumnsEqual -> True, RowsEqual -> True,
  GridBoxSpacings -> {"Columns" -> {{0.3}}, "Rows" -> {{0.3}}}
], templateName];

DefineNotebookDisplayFunction["SmallMatrixForm", #1&];
DefineNotebookDisplayFunction["NormalMatrixForm", #1&];
DefineNotebookDisplayFunction["PMatrixForm", RowBox[{"(", #1, ")"}]&];
DefineNotebookDisplayFunction["BMatrixForm", RowBox[{"[", #1, "]"}]&];
DefineNotebookDisplayFunction["VMatrixForm", RowBox[{"\[LeftBracketingBar]", #1, "\[RightBracketingBar]"}]&];

DefineKatexDisplayFunction["SmallMatrixForm", makeKatexMatrix["smallmatrix", #]&];
DefineKatexDisplayFunction["NormalMatrixForm", makeKatexMatrix["matrix", #]&];
DefineKatexDisplayFunction["PMatrixForm", makeKatexMatrix["pmatrix", #]&];
DefineKatexDisplayFunction["BMatrixForm", makeKatexMatrix["bmatrix", #]&];
DefineKatexDisplayFunction["VMatrixForm", makeKatexMatrix["vmatrix", #]&];

makeKatexMatrix[name_, GridBox[rows_, ___]] := {"{\\begin{" <> name <> "}", Riffle[rowKatex /@ rows, "\\\\"], "\\end{" <> name <> "}}"};
rowKatex[row_List] := Riffle[row, "&"];

(**************************************************************************************************)

PublicTypesettingForm[ArraySymbol]

DefineTaggedForm[ArraySymbol];

(**************************************************************************************************)

PublicTypesettingForm[ArrayPlusForm, ArrayMinusForm, ArraySubtractForm, ArrayTimesForm, ArrayDivideForm]

DefineInfixForm[ArrayPlusForm,  OpBox @ "+"]
DefineInfixForm[ArrayTimesForm, OpBox @ "\[Times]"]
DefineInfixBinaryForm[ArraySubtractForm, OpBox @ "-"];
DefineUnaryForm[ArrayMinusForm, RBox["\[Minus]\!", $1]];
DefineBinaryForm[ArrayDivideForm,  RBox[$1, KBox[OpBox @ "/", "/"], $2]];

(**************************************************************************************************)

PublicTypesettingForm[ArrayPartForm]

DefineRestCommaForm[ArrayPartForm, PartBox[$1, $2]];

(**************************************************************************************************)

PublicTypesettingForm[LiftedForm, MappedForm]

DefineUnaryForm[LiftedForm, SuperscriptBox[$1, "\[UpArrow]"]];
DefineUnaryForm[MappedForm, SuperscriptBox[$1, "\[UpArrow]"]];



