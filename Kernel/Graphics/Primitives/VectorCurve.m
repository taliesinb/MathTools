PublicHead[VectorCurve]

declareGraphicsFormatting[vc:VectorCurve[$Coord3P, $Coord3P...] :> Construct[Line3DBox, vectorCurvePoints @ vc], Graphics3D];
declareGraphicsFormatting[vc:VectorCurve[$Coord2P, $Coord2P...] :> Construct[LineBox, vectorCurvePoints @ vc], Graphics];

(**************************************************************************************************)

PrivateFunction[vectorCurvePoints]

vectorCurvePoints = Case[
  VectorCurve[dir_] := ToPackedReal @ {Zeros @ Length @ dir, dir};
  VectorCurve[pos_, dir_] := ToPackedReal @ {pos, pos + dir};
]
