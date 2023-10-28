PublicFunction[QuadraticForm]

QuadraticForm::badspec =
  "First argument should be a positive integer, a list, or a matrix of coefficients."

QuadraticForm[spec_, OptionsPattern[]] := Scope[
  matrix = Which[
    PositiveIntegerQ[spec], IdentityMatrix[spec],
    RealVectorQ[spec], DiagonalMatrix[spec],
    RealMatrixQ[spec], spec,
    True, ReturnFailed["badspec"]
  ];
  ConstructNoEntry[QuadraticFormObject, matrix]
];

matrixTranspose[matrix_ ? MatrixQ] := Transpose[matrix];
matrixTranspose[vector_] := vector;

(**************************************************************************************************)

PublicObject[QuadraticFormObject]

(QuadraticFormObject[matrix_] ? HoldNoEntryQ)[arg_] := Dot[matrixTranspose @ arg, matrix, arg];

declareFormatting[
  qf_QuadraticFormObject ? HoldNoEntryQ :> formatQuadraticForm @ qf
];

(* formatQuadraticForm[QuadraticFormObject[matrix_]] :=
  Row[{QuadraticForm, "[", formatQuadraticFormMatrix[matrix], "]"}];
 *)
formatQuadraticForm[QuadraticFormObject[matrix_]] :=
  formatQuadraticFormMatrix[matrix];

formatQuadraticFormMatrix[matrix_] :=
  renderRepresentationMatrix[matrix, False, FrameStyle -> $LightBlue]

(**************************************************************************************************)

PublicFunction[QuadraticFormQ]

QuadraticFormQ[_QuadraticFormObject ? HoldNoEntryQ] := True;
QuadraticFormQ[_] := False;
