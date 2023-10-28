PublicTypesettingForm[TextIcon]

Options[TextIcon] = {
  FontColor -> Inherited,
  FontWeight -> Inherited,
  FontSize -> Inherited,
  FontFamily -> Inherited
}

DefineStandardTraditionalForm[ti:TextIcon[_String, ___Rule] :> textIconBoxes[ti]];

TextIcon::nostrpoly = "Could not form a Polygon for `` using FontSize -> ``, FontWeight -> ``, FontFamily -> ``.";

textIconBoxes[TextIcon[str_String, opts___Rule]] := Scope[

  UnpackOptionsAs[TextIcon, opts, fontColor, fontWeight, fontSize, fontFamily];

  SetInherited[fontColor, Black];
  SetInherited[fontWeight, "Regular"];
  SetInherited[fontFamily, "KaTeX_Main"];

  result = TextToPolygon[str, 20, fontFamily, fontWeight];

  If[FailureQ[result],
    Message[TextIcon::nostrpoly, MsgExpr @ s, MsgExpr @ fontSize, MsgExpr @ fontWeight, MsgExpr @ fontFamily];
    Return @ {};
  ];

  {polygons, bounds} = result;
  baseImageSize = BoundsToSize[bounds] / 20;
  primBoxes = ToGraphicsBoxes @ polygons;

  primBoxes = Which[
    Head[fontColor] === ColorGradient || PairQ[fontColor], ColorGradiateBoxes[primBoxes, bounds, fontColor],
    ColorQ[fontColor],                                     StyleBox[primBoxes, FaceForm @ fontColor],
    True,                                                  primBoxes
  ];

  gboxConstructor = makeTextIconGraphicsBox[primBoxes, bounds, baseImageSize];

  If[fontSize === Inherited,
    Function[Null, DynamicBox[With[{System`\[FormalF] = CurrentValue[FontSize]}, #]], HoldFirst] @@ gboxConstructor,
    First[gboxConstructor /. System`\[FormalF] -> fontSize]
  ]
];

(**************************************************************************************************)

(* the internal Construct has to be in there, apparently the FE thinks the expression doesn't need dynamic evaluation otherwise *)
makeTextIconGraphicsBox[boxes_, bounds_, baseImageSize_] :=
  Hold @ AdjustmentBox[
    Construct[
      GraphicsBox,
      boxes,
      PlotRange -> bounds, PlotRangePadding -> 0, AspectRatio -> Full, PlotRangeClipping -> False,
      ImageSize -> Round[baseImageSize * System`\[FormalF] + 2, .5], ImagePadding -> {{1, 1}, {1, 1}},
      BaselinePosition -> Axis
    ],
    -2 / System`\[FormalF]
  ];

