Package["GraphTools`"]


PackageImport["GeneralUtilities`"]


(**************************************************************************************************)

PackageExport["QuadraticForm"]
PackageExport["QuadraticFormObject"]

QuadraticForm::badspec =
  "First argument should be a positive integer, a list, or a matrix of coefficients."

QuadraticForm[spec_, OptionsPattern[]] := Scope[
  matrix = Which[
    Internal`PositiveIntegerQ[spec], IdentityMatrix[spec],
    RealVectorQ[spec], DiagonalMatrix[spec],
    RealMatrixQ[spec], spec,
    True, ReturnFailed["badspec"]
  ];
  System`Private`ConstructNoEntry[QuadraticFormObject, matrix]
];

(QuadraticFormObject[matrix_] ? System`Private`HoldNoEntryQ)[arg_] := Dot[Conjugate @ arg, matrix, arg];

declareFormatting[
  qf_QuadraticFormObject ? System`Private`HoldNoEntryQ :> formatQuadraticForm @ qf
];

(* formatQuadraticForm[QuadraticFormObject[matrix_]] :=
  Row[{QuadraticForm, "[", formatQuadraticFormMatrix[matrix], "]"}];
 *)
formatQuadraticForm[QuadraticFormObject[matrix_]] :=
  formatQuadraticFormMatrix[matrix];

formatQuadraticFormMatrix[matrix_] :=
  renderRepresentationMatrix[matrix, False, FrameStyle -> $LightBlue]


PackageExport["QuadraticFormQ"]

QuadraticFormQ[_QuadraticFormObject ? System`Private`HoldNoEntryQ] := True;
QuadraticFormQ[_] := False;
