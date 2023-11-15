PublicFunction[ToHorizontalLine, ToVerticalLine, ToDepthLine]

ToHorizontalLine[{xmin_, xmax_}, rest__] := Line[tupled[{{xmin, ##}, {xmax, ##}}&][rest]];

ToVerticalLine[x_, {ymin_, ymax_}, rest___] := Line[tupled[{{#1, ymin, ##2}, {#1, ymax, ##2}}&][x, rest]];

ToDepthLine[x_, y_, {zmin_, zmax_}] := Line[tupled[{{##, zmin}, {##, zmax}}&][x, y]];

tupled[f_][args:(Except[_List]..)] := f[args];
tupled[f_][args__] := ApplyTuples[f, ToList /@ {args}];

(**************************************************************************************************)

PublicGraphicsPrimitive[HorizontalCurve, VerticalCurve]

DeclareAtomicCurvePrimitive[HorizontalCurve, axisCurvePoints];
DeclareAtomicCurvePrimitive[VerticalCurve, axisCurvePoints];

SignPrimitive["Vector|Vector,Radius", HorizontalCurve | VerticalCurve];

(**************************************************************************************************)

axisCurvePoints = Case[
  HorizontalCurve[p:$Coord2P|$NumberP]                      := % @ HorizontalCurve[{0, 0}, p];
  HorizontalCurve[pos:$Coord2P, p:$NumberP]                 := ToPackedReal @ {pos, pos + {p, 0}};
  HorizontalCurve[pos:$Coord2P, {p1:$NumberP, p2:$NumberP}] := ToPackedReal @ {pos + {p1, 0}, pos + {p2, 0}};
  VerticalCurve[p:$Coord2P|$NumberP]                        := % @ VerticalCurve[{0, 0}, p];
  VerticalCurve[pos:$Coord2P, p:$NumberP]                   := ToPackedReal @ {pos, pos + {0, p}};
  VerticalCurve[pos:$Coord2P, {p1:$NumberP, p2:$NumberP}]   := ToPackedReal @ {pos + {0, p1}, pos + {0, p2}};
  _                                                         := $Failed;
];
