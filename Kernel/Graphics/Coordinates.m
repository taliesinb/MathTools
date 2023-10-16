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

(*   transformMatrix = ToPackedReal @ BlockDiagonalMatrix2[{
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

zMatrix[tmatrix_, input_] := Transpose @ imageZ @ Dot[tmatrix, AppendConstantRow[1] @ Transpose @ input];

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

PublicFunction[MapPrimitiveCoordinates]

SetUsage @ "
MapPrimitiveCoordinates[fn$, primitives$] transforms all graphics primitives by the \
coordinate transform fn$, which should be a function that operates on a coordinate vector \
or matrix.
MapPrimitiveCoordinates[{fn$, matrixFn$}, $$] applies fn$ to vectors and matrixFn$ to matrices.
* primitives like %Line which can accept a list of coordinate matrices will have \
the function applied to each matrix.
* listability properties of fn$ will be used to ensure it is called the minimum number of times.
* %Threaded[fn$] can be used to indicate that fn$ can be passed matrices.
* %Circle, %Disk etc. do not have proper affine transformations applied to them, or their radii adjusted.
* %Text, etc. do not have their contents rotated if their coordinates are rotated, but if they have a direction it will be changed.
* Objects inside %Translate, %Rotate, etc. are not touched, since they have their own coordinate system.
* Use %MapBoxCoordinates to do the same for boxes.
"

(* we set up this dispatch so that we know (and test) whether to call
$vec or $mat or $matList
for heads that are dual-use, (Line is $mat OR $matList, Point is $vec or $mat), they will
get dispatched to the right case based on a pattern test -- order is important to test
the more specific cases first
*)

$mpcDispatch := $mpcDispatch0;

$mpcDispatch0 := $mpcDispatch0 = Dispatch @ With[{
  $vecvec    = Alternatives @@ PrimitiveSignatureLookup["Vector,Vector"],
  $vecdelta  = Alternatives @@ PrimitiveSignatureLookup["Vector,Delta"],
  $vec       = Alternatives @@ PrimitiveSignatureLookup["Vector?Radius"],
  $matrix    = Alternatives @@ PrimitiveSignatureLookup["Matrix?Radius | Pair?Radius | Curve?Radius"],
  $matrices  = Alternatives @@ PrimitiveSignatureLookup["Matrices?Radius"],
  $opvec     = Alternatives @@ PrimitiveSignatureLookup["Opaque,Vector|Primitives,Vector"],
  $op        = Alternatives @@ PrimitiveSignatureLookup["Opaque"],
  $rules     = Alternatives @@ PrimitiveSignatureLookup["Rules,Primitives"],
  vecP       = $CoordP,
  matP       = {__List} ? CoordinateMatrixQ,
  matListP   = {__List} ? CoordinateMatricesQ}, {
  e:($op)[___]                            :> e,
  (h:$vecvec)[v:vecP, w:vecP, a___]       :> RuleCondition @ h[$vectorF @ v, $vectorF @ w, a],
  (h:$vecdelta)[v:vecP, d:vecP, a___]     :> RuleCondition @ h[Seq @@ $vecDelta[v, d], a],
  (h:$vec)[v:vecP, a___]                  :> RuleCondition @ h[$vectorF @ v, a],
  (h:$matrix)[m:matP, a___]               :> RuleCondition @ h[$matrixF @ m, a],
  (h:$matrices)[v:matListP, a___]         :> RuleCondition @ h[$matrixF /@ v, a],
  (h:$rules)[r_List, p_, a___]            :> RuleCondition @ h[$rulesF @ r, p /. $mpcDispatch, a],
  Text[x_, v:vecP, y_, d:vecP, a___]      :> RuleCondition @ With[{p = $vecDelta[v, d]}, Text[x, First @ p, y, Last @ p, a]],
  Inset[x_, v:vecP, y_, z_, d:vecP, a___] :> RuleCondition @ With[{p = $vecDelta[v, d]}, Inset[x, First @ p, y, z, Last @ p, a]],
  (h:$opvec)[f_, v:vecP, a___]            :> RuleCondition @ h[f, $vectorF @ v, a]
}];

$mbcDispatch := $mbcDispatch = Dispatch @ ReplaceAll[
  Normal @ $mpc0Dispatch,
  $primitiveToBoxReplacements
];

$rulesF[e_] := VectorReplace[e, Rule[c:$CoordP, o_] :> Rule[$vectorF[c], o]];
$vecDelta[a_, d_] := With[{a1 = $vectorF[a]}, {a1, $vectorF[a + d] - a1}];

MapPrimitiveCoordinates[{fn_, matrixFn_}, expr_] := Scope[
  $vectorF = fn; $matrixF = matrixFn;
  ReplaceAll[expr, $mpcDispatch]
]

MapPrimitiveCoordinates[Threaded[fn_] | (fn_ ? VectorListableQ), expr_] := Scope[
  $vectorF = $matrixF = fn;
  ReplaceAll[expr, $mpcDispatch]
]

MapPrimitiveCoordinates[fn_, expr_] := Scope[
  $vectorF = fn;
  $matrixF = Map[fn];
  ReplaceAll[expr, $mpcDispatch]
];

MapPrimitiveCoordinates[{x_, y_} ? CoordinateVectorQ, expr_] :=
  MapPrimitiveCoordinates[
    DotRightOperator @ ToPacked @ Transpose @ {{1, 0, x}, {0, 1, y}},
    expr
  ];

(**************************************************************************************************)

PublicFunction[MapBoxCoordinates]

SetUsage @ "
MapBoxCoordinates is like MapBoxCoordinates but operates on boxes rather than primitives.
"

MapBoxCoordinates[f_, boxes_] := Block[
  {$mpcDispatch = $mbcDispatch},
  MapPrimitiveCoordinates[f, boxes]
]

(**************************************************************************************************)

PublicFunction[AffineTransformPrimitives]

SetUsage @ "
AffineTransformPrimitives[g$, mx$, b$] transforms all graphics primitive coordinates by Dot[x$, mx$] + b$.
"

AffineTransformPrimitives[g_, None | {{1., 0.}, {0., 1.}} | {{1, 0}, {0, 1}}, t_] :=
  MapPrimitiveCoordinates[ThreadedPlusOperator[t], g];

AffineTransformPrimitives[g_, m_, None | {0., 0.} | {0, 0}] :=
  MapPrimitiveCoordinates[AffineOperator[m], g];

AffineTransformPrimitives[g_, m_, t_] :=
  MapPrimitiveCoordinates[AffineOperator[m, t], g];

(**************************************************************************************************)

PublicFunction[GeometricTransformPrimitives]

SetUsage @ "
GeometricTransformPrimitives[t$, spec$] transforms all graphics primitive coordinates by symbolic transformation spec$.
* spec$ is a spec that matches one of the forms supported by GeometricTransformation.
"

GeometricTransformPrimitives[g_, {m:$CoordMatP, t:$CoordP}] :=
  AffineTransformPrimitives[g, m, t];

GeometricTransformPrimitives[g_, m:$CoordMatP] :=
  AffineTransformPrimitives[g, m, None];

GeometricTransformPrimitives[g_, fn_ ? MightEvaluateWhenAppliedQ] :=
  MapPrimitiveCoordinates[onCoords[fn], g];

onCoords[fn_][e_ ? CoordinateMatricesQ] := Map[fn, e];
onCoords[fn_][e_] := fn[e];

(**************************************************************************************************)

PublicFunction[TranslatePrimitives]

SetUsage @ "TranslatePrimitives[g$, vec$] transates all primitive coordinates by vec$."

TranslatePrimitives[prims_, t_] := MapPrimitiveCoordinates[ThreadedPlusOperator[t], prims];

(**************************************************************************************************)

PublicFunction[ScalePrimitives]

SetUsage @ "ScalePrimitives[g$, vec$] scales all primitive coordinates by vec$."

ScalePrimitives[prims_, s_] := MapPrimitiveCoordinates[ThreadedTimesOperator[s], prims];

(**************************************************************************************************)

PublicFunction[RotatePrimitives]

SetUsage @ "RotatePrimitives[g$, theta$] rotates all primitive coordinates by theta$."

RotatePrimitives[prims_, theta_] := AffineTransformPrimitives[prims, RotationMatrix[theta], None];

(**************************************************************************************************)

PublicFunction[ExtractPrimitiveCoordinates]

SetUsage @ "ExtractPrimitiveCoordinates[g$] returns a list of primitive coordinates.";

ExtractPrimitiveCoordinates[prims_] := Scope[
  points = Bag[];
  MapPrimitiveCoordinates[{StuffBag[points, #]&, StuffBag[points, #, 1]&}, prims];
  BagPart[points, All]
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
  $zindex = Bag[];
  zsort @ prims;
  $zindex = Association @ BagPart[$zindex, All];
  KeyValueMap[fromZandStyle, KeySort @ $zindex]
];

fromZandStyle[{z_, {}}, prim_] := prim;
fromZandStyle[{z_, style_}, prim_] := Style[prim, Seq @@ style];

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
