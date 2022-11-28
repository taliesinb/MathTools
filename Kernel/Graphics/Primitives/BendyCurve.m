PublicHead[BendyCurve]
PublicOption[BendRadius, BendShape]

Options[BendyCurve] = {BendRadius -> 0.1, BendShape -> "Arc"};

AssociateTo[$MakeBoxesStyleData, Options[BendyCurve]];

declareGraphicsFormatting[BendyCurve[p:$CoordMat3P, opts___Rule] :> bendyCurveBoxes[True, p, opts], Graphics3D];
declareGraphicsFormatting[BendyCurve[p:$CoordMat2P, opts___Rule] :> bendyCurveBoxes[False, p, opts], Graphics];

BendyCurve::interr = "Internal error."
bendyCurveBoxes[is3d_, points_, opts___Rule] := Scope[
  UnpackAssociationSymbols[
    {opts} -> $MakeBoxesStyleData,
    bendRadius, bendShape
  ];
  points = BendyCurvePoints[points, bendRadius, bendShape];
  If[!NumericMatrixQ[points], Message[BendyCurve::interr]; Return @ {}];
  Construct[If[is3d, Line3DBox, LineBox], points]
]
