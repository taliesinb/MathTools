PublicHead[ExtendedArrow]

declareGraphicsFormatting[ExtendedArrow[path:$CoordMat3P, rest___Rule] :> extendedArrowBoxes[True, path, rest], Graphics3D];
declareGraphicsFormatting[ExtendedArrow[path:$CoordMat2P, rest___Rule] :> extendedArrowBoxes[False, path, rest], Graphics];

Options[ExtendedArrow] = $extendedArrowOptions;

extendedArrowBoxes[is3d_, points_, opts___] := Scope[
  UnpackAssociationSymbols[
    {opts} -> $MakeBoxesStyleData,
    arrowheadPosition, arrowheadLength,
    arrowShaftThickness, arrowShaftColor, arrowShaftOpacity, arrowPathShrinking
  ];
  UnpackAssociationSymbols[{opts} -> {}, arrowheadAnchor];
  SetAutomatic[arrowheadPosition, 0.5];
  If[MatchQ[arrowheadPosition, Offset[_ ? NumericQ, _ ? NumericQ]],
    arrowheadPosition = Last[arrowheadPosition] + (First[arrowheadPosition] / LineLength[points])];
  SetAutomatic[arrowheadAnchor, arrowheadPosition];
  SetAutomatic[arrowheadLength, LineLength[points] * 0.1];
  If[NumericQ[arrowPathShrinking], points = ShrinkPolygon[points, arrowPathShrinking]];
  line = StyleBox[Construct[If[is3d, Line3DBox, LineBox], points], AbsoluteThickness[arrowShaftThickness]];
  {pos, dir} = VectorAlongLine[points, Scaled[arrowheadPosition]];
  center = Mean[points];
  arrowhead = arrowheadBoxes[pos, dir,
    ArrowheadPlane -> PlaneRightTowards[center],
    ArrowheadLength -> arrowheadLength, ArrowheadAnchor -> arrowheadAnchor, opts];
  {line, arrowhead}
]

