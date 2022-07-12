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

RotateVector[vecs:{___List} | vecs_Association, t_] :=
  Map[RotateVector[#, t]&, vecs];

RotateVector[vec_List, t_] :=
  Dot[{{Cos[t], -Sin[t]}, {Sin[t], Cos[t]}}, vec];

RotateVector[t_][vec_] := RotateVector[vec, t];