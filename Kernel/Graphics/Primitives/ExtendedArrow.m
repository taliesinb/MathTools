PublicHead[ExtendedArrow]

declareGraphicsFormatting[ExtendedArrow[path:($CoordMat3P | $GArrowIntP), rest___Rule] :> extendedArrowBoxes[True, path, rest], Graphics3D];
declareGraphicsFormatting[ExtendedArrow[path:($CoordMat2P | $GArrowIntP), rest___Rule] :> extendedArrowBoxes[False, path, rest], Graphics];

Options[ExtendedArrow] = $extendedArrowOptions;

(*
ArrowShaftTruncation
ArrowPathShrinking
ArrowPathBlowout
ArrowheadPosition -> Every[dist] will repeat it every dist!
ArrowheadExtrusion -> 0.1
*)

PrivateFunction[extendedArrowBoxes]

extendedArrowBoxes[is3d_, points_, opts___] := Scope[
  UnpackAssociationSymbols[
    {opts} -> $MakeBoxesStyleData,
    arrowheadPosition, arrowheadLength,
    arrowShaftColor, arrowShaftOpacity, arrowShaftThickness, arrowShaftDashing,
    arrowPathShrinking, arrowPathSetback, arrowPathOffset
  ];
  UnpackAssociationSymbols[{opts} -> {}, arrowheadAnchor];
  SetAutomatic[arrowheadPosition, 1.0];
  points //= toCurvePoints;
  points = SetbackCoordinates[points, arrowPathSetback];
  If[arrowPathOffset =!= None, points += Threaded[arrowPathOffset]];
  If[MatchQ[arrowheadPosition, Offset[_ ? NumericQ, _ ? NumericQ]],
    arrowheadPosition = Last[arrowheadPosition] + (First[arrowheadPosition] / LineLength[points])];
  SetAutomatic[arrowheadAnchor, arrowheadPosition];
  SetAutomatic[arrowheadLength, LineLength[points] * 0.1];
  If[NumericQ[arrowPathShrinking], points = ShrinkPolygon[points, arrowPathShrinking]];
  line = Construct[If[is3d, Line3DBox, LineBox], points];
  styler = makeShaftStyler[arrowShaftColor, arrowShaftOpacity, arrowShaftThickness, arrowShaftDashing];
  {pos, dir} = VectorAlongLine[points, Scaled[arrowheadPosition]];
  center = Mean[points];
  arrowhead = arrowheadBoxes[pos, dir,
    ArrowheadPlane -> PlaneRightTowards[center],
    ArrowheadLength -> arrowheadLength, ArrowheadAnchor -> arrowheadAnchor, opts];
  {styler @ line, arrowhead}
]

