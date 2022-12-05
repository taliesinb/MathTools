PublicHead[BendyArrow]

PublicOption[ArrowShaftThickness, ArrowShaftColor, ArrowShaftOpacity, ArrowPathShrinking]

declareGraphicsFormatting[BendyArrow[p:$CoordMat3P, opts___Rule] :> bendyArrowBoxes[True, p, opts], Graphics3D];
declareGraphicsFormatting[BendyArrow[p:$CoordMat2P, opts___Rule] :> bendyArrowBoxes[False, p, opts], Graphics];

(*
ArrowShaftTruncation
ArrowPathShrinking
ArrowPathBlowout
ArrowheadPosition -> Every[dist] will repeat it every dist!
ArrowheadExtrusion -> 0.1
*)

Options[BendyArrow] = JoinOptions[
  Arrowhead,
  ArrowheadAnchor -> Automatic,
  ArrowheadPosition -> Automatic,
  ArrowShaftThickness -> 2,
  ArrowShaftColor -> Black,
  ArrowShaftOpacity -> 1,
  ArrowPathShrinking -> None,
  BendRadius -> 0.1,
  BendShape -> "Arc"
];

(* ArrowShaftColor -> Inherited will use the current stroke color etc *)

AssociateTo[$MakeBoxesStyleData, KeyTake[Options[BendyArrow], {ArrowheadPosition, ArrowShaftThickness, ArrowShaftColor, ArrowShaftOpacity, ArrowPathShrinking}]];

PrivateFunction[bendyArrowBoxes]

bendyArrowBoxes[is3d_, points_, opts___Rule] := Scope[
  UnpackAssociationSymbols[
    {opts} -> $MakeBoxesStyleData,
    bendRadius, bendShape, arrowheadPosition, arrowheadLength,
    arrowShaftThickness, arrowShaftColor, arrowShaftOpacity, arrowPathShrinking
  ];
  UnpackAssociationSymbols[{opts} -> {}, arrowheadAnchor];
  SetAutomatic[arrowheadPosition, 0.5];
  SetAutomatic[arrowheadAnchor, arrowheadPosition];
  SetAutomatic[arrowheadLength, LineLength[points] * 0.1];
  If[NumericQ[arrowPathShrinking], points = ShrinkPolygon[points, arrowPathShrinking]];
  points = BendyCurvePoints[points, bendRadius, bendShape];
  line = StyleBox[Construct[If[is3d, Line3DBox, LineBox], points], AbsoluteThickness[arrowShaftThickness]];
  {pos, dir} = VectorAlongLine[points, Scaled[arrowheadPosition]];
  center = Mean[points];
  arrowhead = arrowheadBoxes[pos, dir,
    ArrowheadPlane -> PlaneRightTowards[center],
    ArrowheadLength -> arrowheadLength, ArrowheadAnchor -> arrowheadAnchor, opts];
  {line, TooltipBox[arrowhead, ToBoxes @ arrowheadAnchor]}
]

