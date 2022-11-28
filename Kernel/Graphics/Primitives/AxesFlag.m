PublicHead[AxesFlag]

declareGraphicsFormatting[AxesFlag[pos:$CoordP, rest___]  :> axesFlagBoxes[pos, rest]];

axesFlagBoxes[o:{_, _, _}, r_:1] := make3DBoxes[
  {Tooltip[Style[VectorArrow[o, r * {1,0,0}], $Red], "X"],
   Tooltip[Style[VectorArrow[o, r * {0,1,0}], $Green], "Y"],
   Tooltip[Style[VectorArrow[o, r * {0,0,1}], $Blue], "Z"]}
];

axesFlagBoxes[o:{_, _}, r_:1] := make2DBoxes[
  {Tooltip[Style[VectorArrow[o, r * {1,0,0}], $Red], "X"],
   Tooltip[Style[VectorArrow[o, r * {0,1,0}], $Green], "Y"]}
];
