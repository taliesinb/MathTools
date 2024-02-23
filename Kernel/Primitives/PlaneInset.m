PublicGraphicsPrimitive[PlaneInset]

PublicOption[FlipX, FlipY, FlipZ, InsetScale]

SetUsage @ "
PlaneInset[label$, origin$, orientation$] insets a label at origin$ with given orientation and alignment.
PlaneInset[$$, offset$] applies a fractional offset, like %Inset.

* orientation$ can be one of the following:
| 'Screen' | orient so that we are in the same plane as the screen |
| 'XY', 'XZ', 'YZ' | specific axes |

The following options are supported:
| ViewVector | the view vector to assume to implement 'Screen' orientation |
| FlipX | whether to flip the label in the X axis |
| FlipY | whether to flip the label in the Y axis |
| InsetScale | how to scale the label to achieve a given inset size |
| BaseStyle | the style to apply before rasterizing the label |
| TextAlignment | how to align multi-line text |
| LineSpacing | how to space multi-line text |
| FontSize | default font size to use |
| FontFamily | default font family to use |
| FontColor | defauilt font color to use |
"

Options[PlaneInset] = {
  ViewVector -> -{-2, -1.5, 2.5},
  FlipX -> Auto,
  FlipY -> False,
  InsetScale -> 1/144,
  BaseStyle -> {},
  TextAlignment -> Left,
  LineSpacing -> 0.1,
  FontSize -> Inherited,
  FontColor -> Inherited,
  FontFamily -> Inherited
};

DeclareGraphicsPrimitive[PlaneInset, "Opaque,Vector,Pair", planeInsetBoxes, {3}];

planeInsetBoxes[PlaneInset[object_, origin:$Coord3P, vectors:({$Coord3P, $Coord3P}|_Str), offset:$Coord2P:{0, 0}, rule___Rule]] :=
  rawPlaneInsetBoxes[object, origin, vectors, offset, rule]

(**************************************************************************************************)

PlaneInset::badOrient = "Named orientation `` is not recognized."
rawPlaneInsetBoxes[object_, origin_, orient_Str, f_, opts:OptionsPattern[PlaneInset]] := Scope[
  UnpackOptions[viewVector];
  viewVector //= Normalize;
  Switch[orient,
    "Screen",
      vx = Chop @ Cross[viewVector, Normalize @ RepPart[viewVector, 3 -> 0]];
      vy = Chop @ Cross[vx, viewVector],
    "XZ", vx = {1, 0, 0}; vy = {0, 0, 1},
    "XY", vx = {1, 0, 0}; vy = {0, 1, 0},
    "YZ", vx = {0, 1, 0}; vy = {0, 0, 1},
    _,    Message[PlaneInset::badOrient, orient]; Return @ {};
  ];
  rawPlaneInsetBoxes[object, origin, {vx, vy}, f, opts]
];

fracPair[f_] := f/2 + {-0.5, 0.5};
rawPlaneInsetBoxes[object_, origin_, {vx_, vy_}, {fx_, fy_}, OptionsPattern[PlaneInset]] := Scope[
  UnpackOptions[viewVector, flipX, flipY, insetScale, baseStyle, textAlignment, lineSpacing, fontSize, fontColor, fontFamily];
  baseStyle = ToList[baseStyle, FontSize -> fontSize, FontFamily -> fontFamily, FontColor -> fontColor, LineSpacing -> lineSpacing, TextAlignment -> textAlignment];
  baseStyle //= Decases[_ -> Inherited];
  If[textAlignment =!= Left,
    object = Framed[object, FrameStyle -> None, ContentPadding -> False]];
  styledObject = addStyle[object, Seq @@ baseStyle];
  {texture, dims} = cachedTextureBoxAndSize @ styledObject;
  {w, h} = dims * insetScale;
  {mx, px} = Outer[Times, fracPair[-fx], Normalize[vx] * w];
  {my, py} = Outer[Times, fracPair[-fy], Normalize[vy] * h];
  coords = Threaded[origin] + {mx + my, mx + py, px + py, px + my};
  SetAuto[flipX, Dot[Cross[vx, vy], viewVector] > 0];
  TagBox[StyleBox[
    Construct[Polygon3DBox, coords, VertexTextureCoordinates -> $textureCoords[{flipX, flipY}]],
    texture, EdgeForm[None]
  ], "PlaneInset"]
];

addStyle[Text[t_, rest___], style___] := Text[Style[t, style], rest];
addStyle[other_, style___] := Style[other, style];

_rawPlaneInsetBoxes := BadArguments[];

(**************************************************************************************************)

$textureCoords = Assoc[
  {False, False} -> ToPacked[{{0,0}, {0,1}, {1,1}, {1,0}}],
  {False, True}  -> ToPacked[{{0,1}, {0,0}, {1,0}, {1,1}}],
  {True, False}  -> ToPacked[{{1,0}, {1,1}, {0,1}, {0,0}}],
  {True, True}   -> ToPacked[{{1,1}, {1,0}, {0,0}, {0,1}}]
];

(**************************************************************************************************)

CacheVariable[$TextureBoxCache]

cachedTextureBoxAndSize[object_] :=
  CachedInto[$TextureBoxCache, Hash @ object, textureBoxesAndSize @ object];

textureBoxesAndSize[object_] := Scope[
  img = MakeImage[Style[object, LineSpacing -> 0.1], Transparent];
  dims = ImageDimensions[img];
  texture = Construct[Typeset`MakeBoxes, Texture @ img, StandardForm, Graphics3D];
  {texture, dims}
]