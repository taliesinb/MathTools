PublicForm[TupleSymbol]

declareSymbolForm[TupleSymbol] // usingCustomKatex["tupleSym"];

(**************************************************************************************************)

PublicForm[TuplePartForm]

declareBoxFormatting[
  TuplePartForm[t_, i_] :> makeHintedTemplateBox[t -> maybeParen[TupleSymbol], i, "TuplePartForm"]
]

$TemplateKatexFunction["TuplePartForm"] = "tuplePart";

(**************************************************************************************************)

PublicForm[MatrixPartForm, SubMatrixPartForm]

declareBoxFormatting[
  MatrixPartForm[m_, i_, j_] :> makeHintedTemplateBox[m -> maybeParen[MatrixSymbol], i -> MatrixRowPartForm, j -> MatrixColumnPartForm, "MatrixPartForm"],
  SubMatrixPartForm[m_, i_, j_] :> makeHintedTemplateBox[m -> maybeParen[MatrixSymbol], i -> MatrixRowPartForm, j -> MatrixColumnPartForm, "SubMatrixPartForm"]
]

$TemplateKatexFunction["MatrixPartForm"] = "matrixPart";
$TemplateKatexFunction["SubMatrixPartForm"] = "subMatrixPart";

(**************************************************************************************************)

PublicForm[MatrixRowPartForm, MatrixColumnPartForm]

declareUnaryWrapperForm[MatrixRowPartForm];
declareUnaryWrapperForm[MatrixColumnPartForm];

(**************************************************************************************************)

PublicForm[MatrixDotForm, MatrixPlusForm]

declareInfixSymbol[MatrixDotForm, maybeParen[MatrixSymbol|TranslationVectorForm|TupleSymbol]]
declareInfixSymbol[MatrixPlusForm, maybeParen[MatrixSymbol|TranslationVectorForm|TupleSymbol]]

(**************************************************************************************************)

PublicForm[MatrixSymbol]

MatrixSymbol[] := MatrixSymbol["M"];

declareSymbolForm[MatrixSymbol];

(**************************************************************************************************)

PublicForm[SmallMatrixForm, NormalMatrixForm]

declareBoxFormatting[
  SmallMatrixForm[array_] :>
    TemplateBox[MapUnevaluated[matrixRowBoxes, array], "SmallMatrixForm"],

  NormalMatrixForm[array_] :>
    TemplateBox[MapUnevaluated[matrixRowBoxes, array], "NormalMatrixForm"]
];

SetHoldAllComplete[matrixRowBoxes]

matrixRowBoxes[row_List] :=
  TemplateBox[MapUnevaluated[makeQGBoxes, row], "MatrixRowForm"];

$TemplateKatexFunction["SmallMatrixForm"] = smallMatrixKatex;
$TemplateKatexFunction["NormalMatrixForm"] = normalMatrixKatex;
$TemplateKatexFunction["MatrixRowForm"] = matrixRowKatex;

smallMatrixKatex[rows___] := {"{\\begin{smallmatrix}", Riffle[{rows}, "\\\\"], "\\end{smallmatrix}}"};
normalMatrixKatex[rows___] := {"{\\begin{pmatrix}", Riffle[{rows}, "\\\\"], "\\end{pmatrix}}"};

matrixRowKatex[cols___] := Riffle[{cols}, "&"];
