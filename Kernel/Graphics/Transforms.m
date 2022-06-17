PublicFunction[ConstructGraphicsViewTransform]

(* adapted from https://mathematica.stackexchange.com/questions/3528/extract-values-for-viewmatrix-from-a-graphics3d *)

ConstructGraphicsViewTransform[viewAssoc_Association] := Scope[

  viewAssoc = Association[$defaultViewOpts, viewAssoc];
  {plotRange, viewPoint, viewVector, viewRotation, viewMatrix, viewCenter, viewVertical, viewAngle, viewProjection} =
    Lookup[viewAssoc, {PlotRange, ViewPoint, ViewVector, ViewRotation, ViewMatrix, ViewCenter, ViewVertical, ViewAngle, ViewProjection}];

  If[Dimensions[viewMatrix] === {2, 4, 4},
    Return @ GraphicsViewTransform[Dot @@ viewMatrix, viewMatrix]];

  plotSize = EuclideanDistance @@@ plotRange;
  If[Total[plotSize] == 0, Return[Indeterminate]];

  plotRangeLower = Part[plotRange, All, 1];
  SetAutomatic[viewPoint, {1.3, -2.4, 2}];
  SetAutomatic[viewProjection, If[FreeQ[viewPoint, Infinity], "Perspective", "Orthographic"]];
  viewPoint //= Replace[$symbolicViewpointRules];
  viewPoint = Clip[viewPoint, {-1*^5, 1*^5}];

  SetAutomatic[viewCenter, {0.5, 0.5, 0.5}];
  viewCenter = plotRangeLower + viewCenter * plotSize;

  SetAutomatic[viewVector, viewPoint * Max[plotSize] + viewCenter];
  If[MatrixQ[viewVector],
    {cameraPoint, lookAtPoint} = viewVector,
    cameraPoint = viewVector; lookAtPoint = viewCenter
  ];
  transVec = cameraPoint - lookAtPoint;

  If[viewRotation != 0,
    transVec //= SphericalRotateVector[viewRotation * Pi];
  ];

  SetAutomatic[viewVertical, {0, 0, 1}];
  SetAutomatic[viewAngle, $defaultViewAngle];

  viewAngle /= 2;
  isOrtho = viewProjection === "Orthographic";
  scaling = If[False && isOrtho, 1, Cot[viewAngle] / Norm[transVec]];
  boxScale = 1 / plotSize;

  trans = Dot[
    RotationTransform[-alpha[viewVertical/boxScale, transVec], {0, 0, 1}],
    RotationTransform[-theta[transVec], {0, 1, 0}],
    RotationTransform[-phi[transVec], {0, 0, 1}],
    ScalingTransform[scaling * {1, 1, 1}],
    TranslationTransform[-lookAtPoint]
  ];

  transMatrix = ToPackedReal @ TransformationMatrix @ trans;

(*   transMatrix = ToPackedReal @ BlockDiagonalMatrix[{
    ToPackedReal @ trans["AffineMatrix"], Ones[{1,1}]
  }];
 *)
  projMatrix = If[isOrtho, IdentityMatrix[4], makeProjMatrix @ Tan[viewAngle]];

  finalMatrix = ToPackedReal @ Dot[projMatrix, transMatrix];

  GraphicsViewTransform[finalMatrix, {transMatrix, projMatrix}]
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
  Above|Top -> { 0,  0,  2},
  Below|Bottom -> { 0,  0, -2},
  Front -> { 0, -2,  0},
  Back ->  { 0,  2,  0},
  Left ->  {-2,  0,  0},
  Right -> { 2,  0,  0}
};

$defaultViewAngle = 35. * Degree;

$viewOptionSymbols = {ViewPoint, ViewVector, ViewRotation, ViewMatrix, ViewCenter, ViewVertical, ViewAngle, ViewProjection};
$defaultViewOpts = Thread[$viewOptionSymbols -> Automatic] // ReplaceOptions[ViewRotation -> 0];

PrivateVariable[$automaticViewOptions, $defaultViewPoint]

$defaultViewPoint = {-0.2, -2, 0.5};
$automaticViewOptions := {ViewProjection -> "Orthographic", ViewPoint -> $defaultViewPoint};

ConstructGraphicsViewTransform[g_Graphics3D] := Scope[
  viewOpts = Options[g, $viewOptionSymbols];
  plotRange = GraphicsPlotRange @ g;
  viewAssoc = Association[viewOpts, PlotRange -> plotRange];
  ConstructGraphicsViewTransform[Echo @ viewAssoc]
];

(**************************************************************************************************)

PublicFunction[GraphicsViewTransform]

GraphicsViewTransform::badinput = "Received input of dimensions ``: ``."

GraphicsViewTransform[tmatrix_, _][input_] :=
  Which[
    VectorQ[input], gvtVector[tmatrix, input],
    MatrixQ[input], gvtMatrix[tmatrix, input],
    True, Message[GraphicsViewTransform::badinput, Dimensions @ input, input]; input
  ];

gvtVector[tmatrix_, input_] :=
  toImageSpaceVec @ Dot[tmatrix, AppendOnes @ input];

toImageSpaceVec[vec_] :=
  ToPacked[Take[vec, 2] / Part[vec, 4]];

gvtMatrix[tmatrix_, input_] :=
  toImageSpaceMatrix @ Transpose @ Dot[tmatrix, Transpose @ AppendOnes @ N @ input];

toImageSpaceMatrix[matrix_] :=
  ToPacked[Take[matrix, All, 2] / Part[matrix, All, 4]];

(**************************************************************************************************)

PublicFunction[Graphics3DProjection]

$canonicalizeShapes = {
  c_Cuboid :> CanonicalizePolyhedron @ c
};

$to2DShapes = {
  Polyhedron -> Polygon,
  Sphere -> Circle (* todo: Cone, Tube, etc *)
};

Graphics3DProjection[g:Graphics3D[primitives_, ___]] := Scope[
  trans = ConstructGraphicsViewTransform[g];
  primitives //= ReplaceAll[$canonicalizeShapes];
  transformedPrimitives = GraphicsTransformCoordinates[trans, primitives];
  transformedPrimitives //= ReplaceAll[$to2DShapes];
  elements = {EdgeForm[$DarkRed], FaceForm[Opacity[0.2, $Red]], transformedPrimitives};
  Graphics[
    elements, ImageSize -> 400,
    Frame -> True, PlotRange -> All
  ]
];


(**************************************************************************************************)

PublicFunction[GraphicsTransformCoordinates]

$ctfDispatch = Dispatch @ {
  Line[c_] :> Line[$ctf @ c],
  InfiniteLine[c_] :> InfiniteLine[$ctf @ c],
  InfiniteLine[c_, p_] :> InfiniteLine[$ctf @ c, $ctf @ p],
  HalfLine[c_] :> HalfLine[$ctf @ c],
  HalfLine[c_, p_] :> HalfLine[$ctf @ c, $ctf @ p],
  Arrow[c_, opts___] :> Arrow[$ctf @ c, opts],
  Point[c_] :> Point[$ctf @ c],
  Polygon[c_] :> Polygon[$ctf @ c],
  Polygon[c_, d_] :> Polygon[$ctf @ c, d],
  FilledCurve[c_, i_] :> FilledCurve[$ctf @ c, i],
  FilledCurve[c_] :> FilledCurve[$ctf /@ c],
  BezierCurve[c_] :> BezierCurve[$ctf /@ c],
  BSplineCurve[c_] :> BSplineCurve[$ctf /@ c],
  Tube[c_, r___] :> Tube[$ctf @ c, r],
  Cone[c_, r___] :> Cone[$ctf @ c, r],
  Disk[c_, r___] :> Disk[$ctf @ c, r],
  Sphere[c_, r___] :> Sphere[$ctf @ c, r],
  Circle[c_, r___] :> Circle[$ctf @ c, r],
  Polyhedron[c_, d_] :> Polyhedron[$ctf @ c, d],
  Rectangle[c_] :> Rectangle[$ctf @ c],
  GraphicsComplex[c_, g_] :> RuleCondition @ GraphicsComplex[$ctf @ c, g],
  i_Inset :> i,
  a_Arrowheads :> a
};

GraphicsTransformCoordinates[ctf_, expr_] := Scope[
  $ctf = ctf;
  ReplaceAll[expr, $ctfDispatch]
];