PublicGraphicsPrimitive[FramedPrimitive]

PublicOption[FrameOpacity, FrameDashing]

Options[FramedPrimitive] = {
  FrameColor -> Black,
  Background -> None,
  FrameThickness -> 1,
  FrameDashing -> None,
  RoundingRadius -> 0,
  FrameMargins -> 0
};

DeclareGraphicsPrimitive[FramedPrimitive, "Primitives", framedPrimitiveBoxes]

framedPrimitiveBoxes[FramedPrimitive[prims_, opts___Rule]] := Scope[
  UnpackOptionsAs[FramedPrimitive, {opts},
    frameColor, frameThickness, frameDashing, background, roundingRadius, frameMargins];
  boxes = ToGraphicsBoxes @ prims;
  bounds = PrimitiveBoxesBounds @ boxes;
  bounds += {{-1, 1}, {-1, 1}} * StandardizePadding[frameMargins];
  {bl, tr} = Transpose @ bounds;

  thick = AbsoluteThickness @ frameThickness;
  If[frameDashing === None,
    frameBoxes = Construct[RectangleBox, bl, tr, RoundingRadius -> roundingRadius];
    frameBoxes //= StyleBoxOperator[FaceForm @ background, EdgeForm[{frameColor, thick}]];
  ,
    (* Note: dashing precludes rounding radius without extra work to discretize the curve !*)
    frameBoxes = StyleBox[EmptyRectangleBox[bl, tr], frameColor, thick, Dashing @ frameDashing];
    If[ColorQ[background],
      frameBoxes = {frameBoxes, StyleBox[Construct[RectangleBox, bl, tr], FaceForm @ background, EdgeForm @ None]}];
  ];
  List[frameBoxes, boxes]
]
