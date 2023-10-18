PublicFunction[GraphicsZSort]

SetUsage @ "
GraphicsZSort[prims$, zfn$] sorts the 3D graphics primitives prims$ by their average z$ position, \
as calculated by applying zfn$ to individual coordinates.
GraphicsZSort[%Graphics3D[$$], zfn$] returns a %Graphics3D object of sorted primitives.
GraphicsZSort[%Graphics3D[$$]] uses the view parameters in the %Graphics3D object to calculate z$ positions.
* zfn$ can be a function, a %GraphicsViewTransform[$$] object, or an integer that gives the component to use as a z$ position.
* the tree of primitives is rewritten so as to put lower z-value primitives first, but styles are otherwise preserved.
"

(* TODO: Allow ZSorting of 2D graphics, use it in ExtendedGraphics *)
GraphicsZSort[g_Graphics3D] :=
  GraphicsZSort[g, ConstructGraphicsViewTransform @ g];

GraphicsZSort::badtrans = "Provided transform returned `` when supplied origin, rather than a single Z value."

GraphicsZSort[Graphics3D[prims_, opts___], trans_] :=
  Graphics3D[GraphicsZSort[prims, trans], opts];

GraphicsZSort[prims_, trans_] := Scope[
  trans = Switch[trans,
    _Integer,               PartOperator[trans],
    _GraphicsViewTransform, ToZFunction @ trans,
    _,                      trans
  ];
  res = trans[{0, 0, 0}];
  $zfn = Switch[res,
    _ ? NumberQ,          trans,
    {_, _, _ ? NumberQ},  trans /* Last,
    _,                    ReturnFailed["badtrans", res]
  ];
  $zstyle = {Black, Opacity[1]};
  $zindex = Bag[];
  zsort @ prims;
  $zindex = Association @ BagPart[$zindex, All];
  KeyValueMap[fromZandStyle, KeySort @ $zindex]
];

fromZandStyle[{z_, {}}, prim_] := prim;
fromZandStyle[{z_, style_}, prim_] := Style[prim, Seq @@ style];

(* TODO: use the new primitive registry for this! *)
$primitiveP = _Point | _Line | _InfiniteLine | _HalfLine | _Arrow | _FilledCurve | _BezierCurve | _BSplineCurve | _Sphere | _Ball | _Tube |
  _Cone | _Polyhedron | _Polygon | _Torus | _CapsuleShape;

(* TODO: use signatures for this dispatch *)
zsort = Case[

  list_List                                   := InheritedBlock[{$zstyle}, Scan[zsort, list]];
  Style[e_, s___]                             := InheritedBlock[{$zstyle}, Scan[zstyle, {s}]; zsort @ e];

  (* TODO: handle GraphicsComplex, GraphicsTransform / Translate / Rotate etc *)

  (Annotation|Tooltip)[e_, ___]                         := % @ a;
  (* TODO: handle all wrappers *)

  Point[vecs_ ? CoordinateMatrixQ]            := Map[%[Point[#]]&, vecs];
  Line[mats_ ? CoordinateMatricesQ]           := Map[%[Line[#]]&, mats];
  Arrow[mats_ ? CoordinateMatricesQ, opts___] := Map[%[Arrow[#, opts]]&, mats];
  Polygon[mats_ ? CoordinateMatricesQ]        := Map[%[Polygon[#]]&, mats];
  Polygon[mats_ ? CoordinateMatricesQ, arr_]  := Map[%[Polygon[#, arr]]&, mats];

  p:Cuboid[l_, h_]                            := zinsert[{l, h}, p];
  p:$primitiveP                               := zinsert[First @ p, p];

  e_                                          := zstyle[e];
];

zinsert[coords_, prim_] := Scope[
  z = If[CoordinateVectorQ[coords], $zfn @ coords, Mean[$zfn /@ coords]];
  StuffBag[$zindex, {-z, DeleteCases[$zstyle, Black | Opacity[1]]} -> prim]
];

zstyle := Case[
  c_ ? ColorQ  := Set[Part[$zstyle, 1], c];
  o:Opacity[_] := Set[Part[$zstyle, 2], o];
  Opacity[o_, c_] := % @ SetColorOpacity[c, o];
  Directive[{d___}] := Scan[%, d];
  Directive[d___] := Scan[%, d];
  s_ ? GraphicsDirectiveQ := AppendTo[$zstyle, s];
];
