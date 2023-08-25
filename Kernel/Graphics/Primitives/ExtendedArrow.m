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
  points = SetbackCoordinates[points, ReplaceAutomatic[arrowPathSetback, 0]];
  boxFn = If[is3d, Line3DBox, LineBox];

  lineLength = LineLength[points];
  points = applyPathOffset[opoints = points, arrowPathOffset];

  If[MatchQ[arrowheadPosition, Offset[_ ? NumericQ, _ ? NumericQ]],
    arrowheadPosition = Last[arrowheadPosition] + (First[arrowheadPosition] / lineLength)];
  SetAutomatic[arrowheadAnchor, arrowheadPosition];
  SetAutomatic[arrowheadLength, lineLength * 0.1];
  If[NumericQ[arrowPathShrinking], points = ShrinkPolygon[points, arrowPathShrinking]];
  line = Construct[boxFn, points];

  If[CoordinateMatricesQ[points], points = opoints];
  {pos, dir} = VectorAlongLine[points, Scaled[arrowheadPosition]];
  center = Mean[points];
  arrowhead = arrowheadBoxes[pos, dir,
    ArrowheadPlane -> PlaneRightTowards[center],
    ArrowheadLength -> arrowheadLength, ArrowheadAnchor -> arrowheadAnchor, opts];

  styler = makeShaftStyler[arrowShaftColor, arrowShaftOpacity, arrowShaftThickness, arrowShaftDashing];
  {styler @ line, arrowhead}
]

(**************************************************************************************************)

PrivateFunction[applyPathOffset]

applyPathOffset[points_, None] := points;

applyPathOffset[points_, coord:$CoordP] := points + Threaded[coord];

applyPathOffset[points_, Offset[coord:$CoordP]] := SimplifyOffsets[Offset[coord, #]& /@ points];

applyPathOffset[points_, "Perpendicular" -> p_] := applyPathOffset[points, p * pathOrthogonalVector[points]];

applyPathOffset[points_, "Perpendicular" -> Offset[p_]] := applyPathOffset[points, Offset[p * pathOrthogonalVector[points]]];

ExtendedArrow::badoffset = "Bad specification ArrowPathOffset -> ``.";
applyPathOffset[points_, other_] := (Message[ExtendedArrow::badoffset, other]; points);

applyPathOffset[points_, list_List] := Map[applyPathOffset[points, #]&, list];

pathOrthogonalVector[points_ ? ContainsOffsetsQ] := pathOrthogonalVector[RemoveOffsets @ points];
pathOrthogonalVector[points_] := VectorRotate90CW @ N @ Normalize[(Last[points] - First[points])];


