PublicFunction[AffineTransformPrimitives]

SetUsage @ "
AffineTransformPrimitives[g$, mx$, b$] transforms all graphics primitive coordinates by Dot[x$, mx$] + b$.
"

AffineTransformPrimitives[g_, None | {{1., 0.}, {0., 1.}} | {{1, 0}, {0, 1}}, t_] :=
  MapPrimitiveCoordinates[ThreadedPlusOperator[t], g];

AffineTransformPrimitives[g_, m_, None | {0., 0.} | {0, 0}] :=
  MapPrimitiveCoordinates[AffineOperator[m], g];

AffineTransformPrimitives[g_, m_, t_] :=
  MapPrimitiveCoordinates[AffineOperator[m, t], g];

(**************************************************************************************************)

PublicFunction[GeometricTransformPrimitives]

SetUsage @ "
GeometricTransformPrimitives[t$, spec$] transforms all graphics primitive coordinates by symbolic transformation spec$.
* spec$ is a spec that matches one of the forms supported by GeometricTransformation.
"

GeometricTransformPrimitives[g_, {m:$CoordMatP, t:$CoordP}] :=
  AffineTransformPrimitives[g, m, t];

GeometricTransformPrimitives[g_, m:$CoordMatP] :=
  AffineTransformPrimitives[g, m, None];

GeometricTransformPrimitives[g_, fn_ ? MightEvaluateWhenAppliedQ] :=
  MapPrimitiveCoordinates[onCoords[fn], g];

onCoords[fn_][e_ ? CoordinateMatricesQ] := Map[fn, e];
onCoords[fn_][e_] := fn[e];

(**************************************************************************************************)

PublicFunction[TranslatePrimitives, TranslatePrimitiveBoxes]

SetUsage @ "TranslatePrimitives[g$, vec$] transates all primitive coordinates by vec$."
SetUsage @ "TranslatePrimitiveBoxes[g$, vec$] transates all primitive box coordinates by vec$."

TranslatePrimitives[prims_, t_] := MapPrimitiveCoordinates[ThreadedPlusOperator[t], prims];
TranslatePrimitiveBoxes[prims_, t_] := MapPrimitiveBoxCoordinates[ThreadedPlusOperator[t], prims];

(**************************************************************************************************)

PublicFunction[ScalePrimitives]

SetUsage @ "ScalePrimitives[g$, vec$] scales all primitive coordinates by vec$."

ScalePrimitives[prims_, s_] := MapPrimitiveCoordinates[ThreadedTimesOperator[s], prims];

(**************************************************************************************************)

PublicFunction[RotatePrimitives]

SetUsage @ "RotatePrimitives[g$, theta$] rotates all primitive coordinates by theta$."

RotatePrimitives[prims_, theta_] := AffineTransformPrimitives[prims, RotationMatrix[theta], None];
