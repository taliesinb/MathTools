PublicHead[VectorArrow]

declareGraphicsFormatting[

  VectorArrow[pos:$CoordP, dir:$CoordP, rest___] :> vectorArrowBoxes[pos, dir, rest]

];

vectorArrowBoxes[pos1_, dir_, sz_:Automatic] := Scope[
  pos3 = pos1 + dir;
  SetAutomatic[sz, Norm[dir]/4];
  pos2 = PointAlongLine[{pos3, pos1}, sz];
  {AbsoluteThickness[3], Construct[If[Length[pos1] === 2, LineBox, Line3DBox], {pos1, pos2}],
   arrowheadBoxes[pos2, pos3 - pos2]}
];
