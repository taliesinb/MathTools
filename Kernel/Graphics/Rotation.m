PublicFunction[FromSpherical, ToSpherical]

FromSpherical[{r_, a_, b_}] := {r Cos[b] Sin[a],r Sin[a] Sin[b],r Cos[a]};
ToSpherical[{x_, y_, z_}] := {Sqrt[x^2 + y^2 + z^2], ArcTan[z, Sqrt[x^2 + y^2]], ArcTan[x, y]};

(**************************************************************************************************)

PublicFunction[SphericalRotateVector]

SphericalRotateVector[vecs:{___List}, t_] :=
  Map[SphericalRotateVector[#, t]&, vecs];

SphericalRotateVector[vec_List, t_] :=
  Dot[{{Cos[t], -Sin[t], 0}, {Sin[t], Cos[t], 0}, {0, 0, 1}}, vec];

SphericalRotateVector[t_][vec_] := SphericalRotateVector[vec, t];

(**************************************************************************************************)

PublicFunction[RotateVector]

setVectorListableOperator[RotateVector];

RotateVector[vec_List ? CoordinateVectorOrMatrix2DQ, t_] :=
  Dot[vec, {{Cos[t], Sin[t]}, {-Sin[t], Cos[t]}}];

RotateVector[vecs_List | vecs_Assoc, t_] :=
  Map[RotateVector[#, t]&, vecs];

RotateVector[t_][vec_] := RotateVector[vec, t];

(**************************************************************************************************)

PublicFunction[RotateVectorTo]

setVectorListableOperator[RotateVectorTo];

RotateVectorTo[vec_List ? CoordinateVectorOrMatrix2DQ, to_] :=
  Dot[vec, rotToTrans @ to];

RotateVectorTo[vecs_List | vecs_Assoc, to_] :=
  Map[RotateVectorTo[to], vecs];

RotateVectorTo[t_] := DotRightOperator @ rotToTrans @ t;

rotToTrans[dirx_] := ToPackedReal @ List[dirx, VectorRotate90 @ dirx];

(**************************************************************************************************)

(* TODO: make these properly listable! *)

PublicFunction[ScaleRotateTranslateVector]

ScaleRotateTranslateVector[scale_, angle_, trans_List, points_List] :=
  TranslateVector[trans, ScaleRotateVector[scale, angle, points]];

ScaleRotateTranslateVector[s_, a_, t_][points_] := ScaleRotateTranslateVector[s, a, t, points];

(**************************************************************************************************)

PublicFunction[ScaleRotateVector]

ScaleRotateVector[scale_, angle_List, points_List] :=
  ScaleRotateVector[scale, PairAngle @ angle, points];

ScaleRotateVector[scale_, angle_, points_List] :=
  scale * Dot[points, rotationMatrix @ angle];

ScaleRotateVector[s_, a_][points_] := ScaleRotateVector[s, a, points];

(**************************************************************************************************)

$pi = N[Pi];
$tau = N[Tau];

rotationMatrix[0|0.|Tau|$tau|(-Tau)|(-$tau)] := {{1, 0}, {0, 1}};
rotationMatrix[Pi|$pi|(-Pi)|(-$pi)] := {{-1, 0}, {0, -1}};
rotationMatrix[angle_] := Transpose @ ToPacked @ Chop @ RotationMatrix @ N @ angle;

(**************************************************************************************************)

PublicFunction[TranslateVector]

setVectorListableOperator[TranslateVector];

TranslateVector[trans_List, points_List] := Threaded[trans] + points;

TranslateVector[t_][points_] := TranslateVector[t, points];

(**************************************************************************************************)

PublicFunction[RotateToMatrix]

RotateToMatrix[dirx_] :=
  ToPackedReal @ Transpose[{dirx, VectorRotate90 @ dirx}];

RotateToMatrix[dirx_, {sx_, sy_}] :=
  ToPackedReal @ Transpose[{dirx * sx, VectorRotate90 @ dirx * sy}];
