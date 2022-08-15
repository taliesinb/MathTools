PublicHead[CenteredCuboid, CenteredRectangle]

CenteredCuboid[pos_] := CenteredCuboid[pos, 1];
CenteredRectangle[pos_] := CenteredRectangle[pos, 1];

declareGraphicsFormatting[
  CenteredCuboid[pos:$Coord3P, sz:$Coord3P|$NumberP] :> Construct[CuboidBox, pos - sz/2, pos + sz/2],
  Graphics3D
]

declareGraphicsFormatting[
  CenteredRectangle[pos:$Coord2P, sz:$Coord2P|$NumberP] :> Construct[RectangleBox, pos - sz/2, pos + sz/2],
  Graphics
];

(**************************************************************************************************)

PublicHead[VectorArrow]

declareGraphicsFormatting[

  VectorArrow[pos:$CoordP, dir:$CoordP, rest___] :> vectorArrowBoxes[pos, dir, rest]

];

vectorArrowBoxes[pos1_, dir_, sz_:Automatic] := Scope[
  pos3 = pos1 + dir;
  SetAutomatic[sz, Norm[dir]/4];
  pos2 = PointAlongLine[{pos3, pos1}, sz];
  {AbsoluteThickness[2], Construct[If[Length[pos1] === 2, LineBox, Line3DBox], {pos1, pos2}],
   arrowheadBoxes[pos2, pos3 - pos2]}
];

(**************************************************************************************************)

PublicHead[Arrowhead]

declareGraphicsFormatting[

  Arrowhead[pos:$CoordP, dx:$CoordP, rest___]  :> arrowheadBoxes[pos, dx, rest]

];

arrowheadBoxes[x:{_, _}, dx_] := With[
  {dx2 = VectorRotate90[dx] * 0.57735},
  Construct[PolygonBox, ToPacked @ {x - dx2, x + dx, x + dx2}]
]

arrowheadBoxes[x:{_, _, _}, dx_] := With[
  {r = Norm[dx] * 0.57735},
  StyleBox[Construct[ConeBox, ToPacked @ {x, x + dx}, r], EdgeForm[None]]
];

(**************************************************************************************************)

PublicHead[AxesFlag]

declareGraphicsFormatting[AxesFlag[pos:$CoordP, rest___]  :> axesFlagBoxes[pos, rest]];

axesFlagBoxes[o:{_, _, _}, r_:1] := make3DBoxes[
  {Tooltip[Style[VectorArrow[o, r * {1,0,0}], $Red], "X"],
   Tooltip[Style[VectorArrow[o, r * {0,1,0}], $Green], "Y"],
   Tooltip[Style[VectorArrow[o, r * {0,0,1}], $Blue], "Z"]}
];

axesFlagBoxes[o:{_, _}, r_:1] := make2DBoxes[
  {Tooltip[Style[VectorArrow[o, r * {1,0,0}], $Red], "X"],
   Tooltip[Style[VectorArrow[o, r * {0,1,0}], $Green], "Y"]}
];

(**************************************************************************************************)

PublicHead[PlaneInset]

declareGraphicsFormatting[

  PlaneInset[object_, origin_, vectors:{_List, _List}, offset:_List:{0, 0}, rule___Rule] :> planeInsetBoxes[object, origin, vectors, offset, rule]

];

PublicOption[FlipX, FlipY, InsetScale]

Options[PlaneInset] = {
  ViewVector -> -{-2, -1.5, 2.5},
  FlipX -> Automatic,
  FlipY -> False,
  InsetScale -> 1/144,
  BaseStyle -> {}
};

fracPair[f_] := f/2 + {-0.5, 0.5};
planeInsetBoxes[object_, origin_, {vx_, vy_}, {fx_, fy_}, OptionsPattern[PlaneInset]] := Scope[
  UnpackOptions[viewVector, flipX, flipY, insetScale, baseStyle];
  {texture, dims} = cachedTextureBoxAndSize @ If[baseStyle =!= {}, Style[object, Seq @@ baseStyle], object];
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

planeInsetBoxes[args___] := Print[{args}];

$textureCoords = Association[
  {False, False} -> ToPacked[{{0,0}, {0,1}, {1,1}, {1,0}}],
  {False, True}  -> ToPacked[{{0,1}, {0,0}, {1,0}, {1,1}}],
  {True, False}  -> ToPacked[{{1,0}, {1,1}, {0,1}, {0,0}}],
  {True, True}   -> ToPacked[{{1,1}, {1,0}, {0,0}, {0,1}}]
];

If[!AssociationQ[QuiverGeometryLoader`$TextureBoxCache],
  QuiverGeometryLoader`$TextureBoxCache = UAssociation[]];

cachedTextureBoxAndSize[object_] :=
  CacheTo[QuiverGeometryLoader`$TextureBoxCache, Hash @ object, textureBoxesAndSize @ object];

textureBoxesAndSize[object_] := Scope[
  img = FastRasterize[object, Background -> Transparent];
  dims = ImageDimensions @ img;
  texture = Construct[Typeset`MakeBoxes, Texture @ img, StandardForm, Graphics3D];
  {texture, dims}
]

(**************************************************************************************************)

PublicFunction[HorizontalLine, VerticalLine, DepthLine]

HorizontalLine[{xmin_, xmax_}, rest__] := Line[tupled[{{xmin, ##}, {xmax, ##}}&][rest]];

VerticalLine[x_, {ymin_, ymax_}, rest___] := Line[tupled[{{#1, ymin, ##2}, {#1, ymax, ##2}}&][x, rest]];

DepthLine[x_, y_, {zmin_, zmax_}] := Line[tupled[{{##, zmin}, {##, zmax}}&][x, y]];

tupled[f_][args:(Except[_List]..)] := f[args];
tupled[f_][args__] := ApplyTuples[f, ToList /@ {args}];

(**************************************************************************************************)

(* CoordinateComplex is the generalization of Point, HorizontalLine, VerticalLine, etc. It's n'th argument
represents the n'th coordinate, and any lists are implicitly treated as a set of elements whose
cartesian product will be taken. Any CoordinateSegment[begin, end] will be treated as a segment on that
axis. *)

PublicFunction[CoordinateComplex]
PublicHead[CoordinateSegment]

CoordinateComplex[{___, {}, ___}] := {};

CoordinateComplex[coords_] := KeyValueMap[ccMake, KeySort @ GroupPairs @ ApplyTuples[ccElement, ToList /@ coords]];

ccMake[0, p_] := Point[p];
ccElement[p__] :=
  0 -> {p};

ccMake[1, p_] := Line[p];
ccElement[p___, CoordinateSegment[l_, h_], q___] :=
  1 -> {
    {p, l, q},
    {p, h, q}
  };

ccMake[2, {p_}] := Rectangle @@ p;
ccMake[2, p_] := Rectangle @@@ p;
ccElement[CoordinateSegment[al_, ah_], CoordinateSegment[bl_, bh_]] :=
  2 -> {
    {al, bl},
    {ah, bh}
  };

ccMake[2., p_] := Polygon @ p;
ccElement[p___, CoordinateSegment[al_, ah_], q___, CoordinateSegment[bl_, bh_], r___] :=
  2. -> {
    {p, al, q, bl, r},
    {p, al, q, bh, r},
    {p, ah, q, bh, r},
    {p, ah, q, bl, r}
  };

ccMake[3, {p_}] := Cuboid @@ p;
ccMake[3, p_] := Cuboid @@@ p;
ccElement[p___, CoordinateSegment[al_, ah_], q___, CoordinateSegment[bl_, bh_], r___, CoordinateSegment[cl_, ch_], s___] :=
  3 -> {
    {p, al, q, bl, r, cl, s},
    {p, ah, q, bh, r, ch, s}
  };
