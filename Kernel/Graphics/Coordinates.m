PublicFunction[ConstructGraphicsViewTransform]

(* adapted from https://mathematica.stackexchange.com/questions/3528/extract-values-for-viewmatrix-from-a-graphics3d *)

ConstructGraphicsViewTransform[viewAssoc_Association] := Scope[

  viewAssoc = Association[$defaultViewOpts, viewAssoc];
  {plotRange, viewPoint, viewVector, viewRotation, viewMatrix, viewCenter, viewVertical, viewAngle, viewProjection} =
    Lookup[viewAssoc, {PlotRange, ViewPoint, ViewVector, ViewRotation, ViewMatrix, ViewCenter, ViewVertical, ViewAngle, ViewProjection}, Automatic];

  If[Dimensions[viewMatrix] === {2, 4, 4},
    Return @ GraphicsViewTransform[Dot @@ viewMatrix, viewMatrix]];

  SetAutomatic[plotRange, {{-1, 1}, {-1, 1}, {-1, 1}}];
  plotSize = EuclideanDistance @@@ plotRange;
  If[Total[plotSize] == 0, Return[Indeterminate]];

  plotRangeLower = Part[plotRange, All, 1];
  SetAutomatic[viewPoint, {1.3, -2.4, 2}];
  SetAutomatic[viewProjection, If[FreeQ[viewPoint, Infinity], "Perspective", "Orthographic"]];
  viewPoint //= Replace[$symbolicViewpointRules];
  viewPoint = Clip[viewPoint, {-1*^5, 1*^5}];

  SetAutomatic[viewCenter, {0.5, 0.5, 0.5}];
  If[MatchQ[viewCenter, {{_, _, _}, {_, _}}], viewCenter //= First];
  (* ^ Don't yet support translation of view *)

  viewCenter = plotRangeLower + viewCenter * plotSize;

  SetAutomatic[viewVector, viewPoint * Max[plotSize] + viewCenter];
  If[MatrixQ[viewVector],
    {cameraPoint, lookAtPoint} = viewVector
  ,
    cameraPoint = viewVector; lookAtPoint = viewCenter;
    viewVector = {cameraPoint, lookAtPoint};
  ];
  transVec = cameraPoint - lookAtPoint;

  If[viewRotation != 0,
    transVec //= SphericalRotateVector[viewRotation * Pi];
  ];

  SetAutomatic[viewVertical, {0, 0, 1}];
  SetAutomatic[viewAngle, $defaultViewAngle];

  viewAngle /= 2;
  isOrtho = viewProjection === "Orthographic";
  scaling = If[isOrtho, 1, Cot[viewAngle] / Norm[transVec]];
  boxScale = 1 / plotSize;

  trans = Dot[
    RotationTransform[-alpha[viewVertical/boxScale, transVec], {0, 0, 1}],
    RotationTransform[-theta[transVec], {0, 1, 0}],
    RotationTransform[-phi[transVec], {0, 0, 1}],
    ScalingTransform[scaling * {1, 1, 1}],
    TranslationTransform[-lookAtPoint]
  ];

  transformMatrix = ToPackedReal @ TransformationMatrix @ trans;

(*   transformMatrix = ToPackedReal @ BlockDiagonalMatrix[{
    ToPackedReal @ trans["AffineMatrix"], Ones[{1,1}]
  }];
 *)
  projectionMatrix = If[isOrtho, IdentityMatrix[4], makeProjMatrix @ Tan[viewAngle]];

  finalMatrix = ToPackedReal @ Chop @ Dot[projectionMatrix, transformMatrix];

  assoc = PackAssociation[transformMatrix, projectionMatrix, viewVector, viewProjection];

  GraphicsViewTransform[finalMatrix, assoc]
];

makeProjMatrix[tanAngle_] := ToPackedReal @ {
  {1, 0, -1 * tanAngle, 1},
  {0, 1, -1 * tanAngle, 1},
  {0, 0, -1 * tanAngle, 0},
  {0, 0, -2 * tanAngle, 2}
};

theta[{x_, y_, z_}] := ArcTan[z, Norm[{x, y}]]

phi[{x_, y_, _}] := ArcTan2[x, y];

alpha[viewVertical_, viewVector_] := Scope[
  p = phi[viewVector];
  v = {-Sin[p], Cos[p], 0};
  ArcTan2[v . viewVertical, Cross[viewVector / Norm[viewVector], v] . viewVertical]
];

$symbolicViewpointRules = {
  Above|Top    -> { 0,  0,  2},
  Below|Bottom -> { 0,  0, -2},
  Front        -> { 0, -2,  0},
  Back         -> { 0,  2,  0},
  Left         -> {-2,  0,  0},
  Right        -> { 2,  0,  0}
};

$defaultViewAngle = 35. * Degree;

$viewOptionSymbols = {ViewPoint, ViewVector, ViewRotation, ViewMatrix, ViewCenter, ViewVertical, ViewAngle, ViewProjection};
$defaultViewOpts = Thread[$viewOptionSymbols -> Automatic] // ReplaceOptions[ViewRotation -> 0];

PrivateVariable[$automaticViewOptions, $defaultViewPoint]

$defaultViewPoint = {-0.2, -2, 0.5};
$automaticViewOptions := {ViewProjection -> "Orthographic", ViewPoint -> $defaultViewPoint};

ConstructGraphicsViewTransform[g_Graphics3D] := Scope[
  viewOpts = Options[g, DeleteCases[$viewOptionSymbols, ViewRotation]];
  plotRange = GraphicsPlotRange @ g;
  viewAssoc = Association[viewOpts, PlotRange -> plotRange];
  ConstructGraphicsViewTransform[viewAssoc]
];

(**************************************************************************************************)

PublicFunction[GraphicsViewTransform]

GraphicsViewTransform::badinput = "Received input of dimensions ``: ``."

GraphicsViewTransform[tmatrix_, _][input_] :=
  Which[
    VectorQ[input], xyVector[tmatrix, input],
    MatrixQ[input], xyMatrix[tmatrix, input],
    True, Message[GraphicsViewTransform::badinput, Dimensions @ input, input]; input
  ];

xyVector[tmatrix_, input_] := imageXY @ Dot[tmatrix, Append[input, 1]];

xyMatrix[tmatrix_, input_] := Transpose @ imageXY @ Dot[tmatrix, AppendConstantRow[1] @ Transpose @ input];

imageXY[a_] := ToPacked[Take[a, 2] / maybeThreaded[Part[a, 4]]];

maybeThreaded[e_List] := Threaded[e];
maybeThreaded[e_] := e;

(**************************************************************************************************)

PrivateFunction[ToXYFunction]

ToXYFunction[GraphicsViewTransform[tmatrix_, _]] := Function[
  input,
  Which[
    VectorQ[input], xyVector[tmatrix, input],
    MatrixQ[input], xyMatrix[tmatrix, input],
    True, $Failed
  ]
];

(**************************************************************************************************)

PrivateFunction[ToZFunction]

ToZFunction[GraphicsViewTransform[tmatrix_, _]] := Function[
  input,
  Which[
    VectorQ[input], zVector[tmatrix, input],
    MatrixQ[input], zMatrix[tmatrix, input],
    True, $Failed
  ]
];

zVector[tmatrix_, input_] := imageZ @ Dot[tmatrix, Append[input, 1]];

zMatrix[tmatrix_, input_] := Transpose @ imageZ @ Dot[tmatrix, AppendConstnatRow[1] @ Transpose @ input];

imageZ[a_] := ToPacked[Part[a, 3] / maybeThreaded[Part[a, 4]]];

(**************************************************************************************************)

PrivateFunction[ToXYZFunction]

ToXYZFunction[GraphicsViewTransform[tmatrix_, _]] := Function[
  input,
  Which[
    VectorQ[input], xyzVector[tmatrix, input],
    MatrixQ[input], xyzMatrix[tmatrix, input],
    True, $Failed
  ]
];

xyzVector[tmatrix_, input_] := imageXYZ @ Dot[tmatrix, Append[input, 1]];

xyzMatrix[tmatrix_, input_] := Transpose @ imageXYZ @ Dot[tmatrix, AppendConstantRow[1] @ Transpose @ input]

imageXYZ[e_] := Transpose[Take[e, 3] / maybeThreaded[Part[e, 4]]];

(**************************************************************************************************)

PublicFunction[GraphicsTransformCoordinates]

SetUsage @ "
GraphicsTransformCoordinates[trans$, primitives$] transforms all graphics primitives by the \
coordinate transform trans$, which should be a function that operates on a coordinate tuple \
or list of these.
GraphicsTransformCoordinates[{x$, y$}, primitives$] takes projects the unit z$ basis into \
the vector {x$, y$}.
"

(* TODO: correctly transform radii, etc. Sphere should become ellipsoid etc.
TODO: check for coordinates  first to handle things like Arrow[Line[...]] *)

$ctfDispatch = Dispatch @ {
  Line[c_]                   :> RuleCondition @ Line[$ctf @ c],
  InfiniteLine[c_]           :> RuleCondition @ InfiniteLine[$ctf @ c],
  InfiniteLine[c_, p_]       :> RuleCondition @ InfiniteLine[$ctf @ c, $ctf @ p],
  HalfLine[c_]               :> RuleCondition @ HalfLine[$ctf @ c],
  HalfLine[c_, p_]           :> RuleCondition @ HalfLine[$ctf @ c, $ctf @ p],
  Arrow[c_, r___]            :> RuleCondition @ Arrow[$ctf @ c, r],
  Point[c_]                  :> RuleCondition @ Point[$ctf @ c],
  Polygon[c_]                :> RuleCondition @ Polygon[$ctf @ c],
  Polygon[c_, d_]            :> RuleCondition @ Polygon[$ctf @ c, d],
  Polyhedron[c_]             :> RuleCondition @ Polyhedron[$ctf @ c], (* TODO: take care of -> etc *)
  Polyhedron[c_, d_]         :> RuleCondition @ Polyhedron[$ctf @ c, d],
  FilledCurve[c_, i_]        :> RuleCondition @ FilledCurve[$ctf @ c, i],
  FilledCurve[c_]            :> RuleCondition @ FilledCurve[$ctf /@ c],
  BezierCurve[c_, r___]      :> RuleCondition @ BezierCurve[$ctf /@ c, r],
  BSplineCurve[c_, r___]     :> RuleCondition @ BSplineCurve[$ctf /@ c, r],
  Tube[c_, r___]             :> RuleCondition @ Tube[$ctf @ c, r],
  Cone[c_, r___]             :> RuleCondition @ Cone[$ctf @ c, r],
  Disk[c_, r___]             :> RuleCondition @ Disk[$ctf @ c, r],
  Circle[c_, r___]           :> RuleCondition @ Circle[$ctf @ c, r],
  Ball[c_, r___]             :> RuleCondition @ Ball[$ctf @ c, r],
  Sphere[c_, r___]           :> RuleCondition @ Sphere[$ctf @ c, r]
  Polyhedron[c_, d_]         :> RuleCondition @ Polyhedron[$ctf @ c, d],
  Rectangle[a_]              :> RuleCondition @ Rectangle[$ctf @ a],
  Rectangle[a_, b_, r___]    :> RuleCondition @ Rectangle[$ctf @ a, $ctf @ b, r],
  Cuboid[a_]                 :> RuleCondition @ Cuboid[$ctf @ a, $ctf @ b],
  Cuboid[a_, b_]             :> RuleCondition @ Cuboid[$ctf @ a, $ctf @ b],
  CenteredCuboid[a_, r_]     :> RuleCondition @ CenteredCuboid[$ctf @ a, r],
  CenteredRectangle[a_, r_]  :> RuleCondition @ CenteredCuboid[$ctf @ a, r],
  GraphicsComplex[c_, g_]    :> RuleCondition @ GraphicsComplex[$ctf @ c, g],
  Inset[e_, a_, r___]        :> Inset[e, $ctf @ a, r],
  Text[e_, a_, r___]         :> Text[e, $ctf @ a, r],
  a_Arrowheads               :> a
};

GraphicsTransformCoordinates[ctf_, expr_] := Scope[
  $ctf = ctf;
  ReplaceAll[expr, $ctfDispatch]
];

GraphicsTransformCoordinates[{x_, y_} ? CoordinateVectorQ, expr_] :=
  GraphicsTransformCoordinates[
    DotRightOperator @ ToPacked @ Transpose @ {{1, 0, x}, {0, 1, y}},
    expr
  ];

(**************************************************************************************************)

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
  $zindex = Internal`Bag[];
  zsort @ prims;
  $zindex = Association @ Internal`BagPart[$zindex, All];
  KeyValueMap[fromZandStyle, KeySort @ $zindex]
];

fromZandStyle[{z_, {}}, prim_] := prim;
fromZandStyle[{z_, style_}, prim_] := Style[prim, Seq @@ style];

$primitiveP = _Point | _Line | _InfiniteLine | _HalfLine | _Arrow | _FilledCurve | _BezierCurve | _BSplineCurve | _Sphere | _Ball | _Tube |
  _Cone | _Polyhedron | _Polygon | _Torus | _CapsuleShape;

zsort = Case[

  list_List                                   := Internal`InheritedBlock[{$zstyle}, Scan[zsort, list]];
  Style[e_, s___]                             := Internal`InheritedBlock[{$zstyle}, Scan[zstyle, {s}]; zsort @ e];

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
  Internal`StuffBag[$zindex, {-z, DeleteCases[$zstyle, Black | Opacity[1]]} -> prim]
];

zstyle := Case[
  c_ ? ColorQ  := Set[Part[$zstyle, 1], c];
  o:Opacity[_] := Set[Part[$zstyle, 2], o];
  Opacity[o_, c_] := % @ SetColorOpacity[c, o];
  Directive[{d___}] := Scan[%, d];
  Directive[d___] := Scan[%, d];
  s_ ? GraphicsDirectiveQ := AppendTo[$zstyle, s];
];
