PublicGraphicsPrimitive[VectorCurve]

DeclareAtomicCurvePrimitive[VectorCurve, vectorCurvePoints];

SignPrimitive["Vector | Vector,Delta", VectorCurve];

(**************************************************************************************************)

vectorCurvePoints = Case[
  VectorCurve[dir:$CoordP] := ToPackedReal @ {Zeros @ Len @ dir, dir};
  VectorCurve[pos:$CoordP, dir:$CoordP] := ToPackedReal @ {pos, pos + dir};
]
