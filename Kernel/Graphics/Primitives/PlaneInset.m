PublicHead[PlaneInset]

PublicOption[FlipX, FlipY, FlipZ, InsetScale]

Options[PlaneInset] = {
  ViewVector -> -{-2, -1.5, 2.5},
  FlipX -> Automatic,
  FlipY -> False,
  InsetScale -> 1/144,
  BaseStyle -> {},
  TextAlignment -> Left,
  LineSpacing -> 0.1,
  FontSize -> Inherited,
  FontColor -> Inherited,
  FontFamily -> Inherited
};

DeclareGraphicsPrimitive[PlaneInset, "Opaque,Opaque,Pair", planeInsetBoxes, {3}];

planeInsetBoxes[PlaneInset[object_, origin_, vectors:({_List, _List}|_String), offset:_List:{0, 0}, rule___Rule]] :=
  rawPlaneInsetBoxes[object, origin, vectors, offset, rule]

(**************************************************************************************************)

rawPlaneInsetBoxes[object_, origin_, orient_String, f_, opts:OptionsPattern[PlaneInset]] := Scope[
  UnpackOptions[viewVector];
  viewVector //= Normalize;
  Switch[orient,
    "Screen",
      vx = Chop @ Cross[viewVector, Normalize @ ReplacePart[viewVector, 3 -> 0]];
      vy = Chop @ Cross[vx, viewVector],
    "XZ",
      vx = {1, 0, 0}; vy = {0, 0, 1},
    "XY",
      vx = {1, 0, 0}; vy = {0, 1, 0},
    "YZ",
      vx = {0, 1, 0}; vy = {0, 0, 1}
  ];
  rawPlaneInsetBoxes[object, origin, {vx, vy}, f, opts]
];

fracPair[f_] := f/2 + {-0.5, 0.5};
rawPlaneInsetBoxes[object_, origin_, {vx_, vy_}, {fx_, fy_}, OptionsPattern[PlaneInset]] := Scope[
  UnpackOptions[viewVector, flipX, flipY, insetScale, baseStyle, textAlignment, lineSpacing, fontSize, fontColor, fontFamily];
  baseStyle = ToList[baseStyle, FontSize -> fontSize, FontFamily -> fontFamily, FontColor -> fontColor, LineSpacing -> lineSpacing, TextAlignment -> textAlignment];
  baseStyle //= DeleteCases[_ -> Inherited];
  If[textAlignment =!= Left,
    object = Framed[object, FrameStyle -> None, ContentPadding -> False]];
  styledObject = addStyle[object, Seq @@ baseStyle];
  {texture, dims} = cachedTextureBoxAndSize @ styledObject;
  {w, h} = dims * insetScale;
  {mx, px} = Outer[Times, fracPair[-fx], Normalize[vx] * w];
  {my, py} = Outer[Times, fracPair[-fy], Normalize[vy] * h];
  coords = Threaded[origin] + {mx + my, mx + py, px + py, px + my};
  SetAutomatic[flipX, Dot[Cross[vx, vy], viewVector] > 0];
  StyleBox[
    Construct[Polygon3DBox, coords, VertexTextureCoordinates -> $textureCoords[{flipX, flipY}]],
    texture, EdgeForm[None]
  ]
];

addStyle[Text[t_, rest___], style___] := Text[Style[t, style], rest];
addStyle[other_, style___] := Style[other, style];

_rawPlaneInsetBoxes := BadArguments[];

(**************************************************************************************************)

$textureCoords = Association[
  {False, False} -> ToPacked[{{0,0}, {0,1}, {1,1}, {1,0}}],
  {False, True}  -> ToPacked[{{0,1}, {0,0}, {1,0}, {1,1}}],
  {True, False}  -> ToPacked[{{1,0}, {1,1}, {0,1}, {0,0}}],
  {True, True}   -> ToPacked[{{1,1}, {1,0}, {0,0}, {0,1}}]
];

(**************************************************************************************************)

cachedTextureBoxAndSize[object_] :=
  CacheTo[QuiverGeometryCaches`$TextureBoxCache, Hash @ object, textureBoxesAndSize @ object];

textureBoxesAndSize[object_] := Scope[
  img = FastRasterize[Style[object, LineSpacing -> 0.1], Background -> Transparent];
  dims = ImageDimensions @ img;
  texture = Construct[Typeset`MakeBoxes, Texture @ img, StandardForm, Graphics3D];
  {texture, dims}
]