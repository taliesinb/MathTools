PublicGraphicsPrimitive[AxesFlag]

DeclareGraphicsPrimitive[AxesFlag, "Vector?Radius", axesFlagBoxes, {2, 3}];

axesFlagBoxes[AxesFlag[o:$Coord3P, r:$NumberP:1]] := MapApply[
  styleTooltip[vectorArrowBoxes @ VectorArrow[o, r * #1], #2, #3]&,
  {{{1, 0, 0}, $Red,   "X"},
   {{0, 1, 0}, $Green, "Y"},
   {{0, 0, 1}, $Blue,  "Z"}}
];

axesFlagBoxes[AxesFlag[o:$Coord2P, r:$NumberP:1]] := MapApply[
  styleTooltip[vectorArrowBoxes @ VectorArrow[o, r * #1], #2, #3]&,
  {{{1, 0}, $Red,   "X"},
   {{0, 1}, $Green, "Y"}}
];

styleTooltip[box_, s_, t_] := TooltipBox[StyleBox[box, s], t];

(**************************************************************************************************)

PublicGraphicsPrimitive[VectorArrow]

DeclareGraphicsPrimitive[VectorArrow, "Vector,Delta", vectorArrowBoxes, {2, 3}];

vectorArrowBoxes[VectorArrow[p:$Coord2P, d:$Coord2P]] := Scope[
  p0 = p + d;
  p1 = p0 - VectorRotate45[d] * 1/5;
  p2 = p0 - VectorRotate45CW[d] * 1/5;
  {AbsoluteThickness[2], Construct[LineBox, {{p, p0}, {p1, p0, p2}}]}
];

vectorArrowBoxes[VectorArrow[p:$Coord3P, d:$Coord3P]] := With[{m = p + 7/8*d},
  List[
    Construct[TubeBox, {p, m}, Norm[d]/64],
    Construct[ConeBox, {m, p + d}, Norm[d]/14]
  ]
];
