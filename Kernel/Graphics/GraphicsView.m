PublicFunction[ConstructGraphicsViewTransform]

(* adapted from https://mathematica.stackexchange.com/questions/3528/extract-values-for-viewmatrix-from-a-graphics3d *)

ConstructGraphicsViewTransform[viewAssoc_Assoc] := Scope[

  viewAssoc = Assoc[$defaultViewOpts, viewAssoc];
  {plotRange, viewPoint, viewVector, viewRotation, viewMatrix, viewCenter, viewVertical, viewAngle, viewProjection} =
    Lookup[viewAssoc, {PlotRange, ViewPoint, ViewVector, ViewRotation, ViewMatrix, ViewCenter, ViewVertical, ViewAngle, ViewProjection}, Auto];

  If[Dims[viewMatrix] === {2, 4, 4},
    Return @ GraphicsViewTransform[Dot @@ viewMatrix, viewMatrix]];

  SetAuto[plotRange, {{-1, 1}, {-1, 1}, {-1, 1}}];
  plotSize = Dist @@@ plotRange;
  If[Total[plotSize] == 0, Return[Indeterminate]];

  plotRangeLower = Col1[plotRange];
  SetAuto[viewPoint, {1.3, -2.4, 2}];
  SetAuto[viewProjection, If[FreeQ[viewPoint, Inf], "Perspective", "Orthographic"]];
  viewPoint //= Rep[$symbolicViewpointRules];
  viewPoint = Clip[viewPoint, {-1*^5, 1*^5}];

  SetAuto[viewCenter, {0.5, 0.5, 0.5}];
  If[MatchQ[viewCenter, {{_, _, _}, {_, _}}], viewCenter //= F];
  (* ^ Don't yet support translation of view *)

  viewCenter = plotRangeLower + viewCenter * plotSize;

  SetAuto[viewVector, viewPoint * Max[plotSize] + viewCenter];
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

  SetAuto[viewVertical, {0, 0, 1}];
  SetAuto[viewAngle, $defaultViewAngle];

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
$defaultViewOpts = Thread[$viewOptionSymbols -> Auto] // ReplaceOptions[ViewRotation -> 0];

PrivateVariable[$automaticViewOptions, $defaultViewPoint]

$defaultViewPoint = {-0.2, -2, 0.5};
$automaticViewOptions := {ViewProjection -> "Orthographic", ViewPoint -> $defaultViewPoint};

ConstructGraphicsViewTransform[g_Graphics3D] := Scope[
  viewOpts = Options[g, Decases[$viewOptionSymbols, ViewRotation]];
  plotRange = GraphicsPlotRange @ g;
  viewAssoc = Assoc[viewOpts, PlotRange -> plotRange];
  ConstructGraphicsViewTransform[viewAssoc]
];

(**************************************************************************************************)

PublicFunction[GraphicsViewTransform]

GraphicsViewTransform::badinput = "Received input of dimensions ``: ``."

GraphicsViewTransform[tmatrix_, _][input_] :=
  Which[
    VecQ[input], xyVector[tmatrix, input],
    MatrixQ[input], xyMatrix[tmatrix, input],
    True, Message[GraphicsViewTransform::badinput, Dims @ input, input]; input
  ];

xyVector[tmatrix_, input_] := imageXY @ Dot[tmatrix, App[input, 1]];

xyMatrix[tmatrix_, input_] := Transpose @ imageXY @ Dot[tmatrix, AppendConstantRow[1] @ Transpose @ input];

imageXY[a_] := ToPacked[Take[a, 2] / maybeThreaded[P4[a]]];

maybeThreaded[e_List] := Threaded[e];
maybeThreaded[e_] := e;

(**************************************************************************************************)

PrivateFunction[ToXYFunction]

ToXYFunction[GraphicsViewTransform[tmatrix_, _]] := Fn[
  input,
  Which[
    VecQ[input], xyVector[tmatrix, input],
    MatrixQ[input], xyMatrix[tmatrix, input],
    True, $Failed
  ]
];

(**************************************************************************************************)

PrivateFunction[ToZFunction]

ToZFunction[GraphicsViewTransform[tmatrix_, _]] := Fn[
  input,
  Which[
    VecQ[input], zVector[tmatrix, input],
    MatrixQ[input], zMatrix[tmatrix, input],
    True, $Failed
  ]
];

zVector[tmatrix_, input_] := imageZ @ Dot[tmatrix, App[input, 1]];

zMatrix[tmatrix_, input_] := Transpose @ imageZ @ Dot[tmatrix, AppendConstantRow[1] @ Transpose @ input];

imageZ[a_] := ToPacked[P3[a] / maybeThreaded[P4[a]]];

(**************************************************************************************************)

PrivateFunction[ToXYZFunction]

ToXYZFunction[GraphicsViewTransform[tmatrix_, _]] := Fn[
  input,
  Which[
    VecQ[input], xyzVector[tmatrix, input],
    MatrixQ[input], xyzMatrix[tmatrix, input],
    True, $Failed
  ]
];

xyzVector[tmatrix_, input_] := imageXYZ @ Dot[tmatrix, App[input, 1]];

xyzMatrix[tmatrix_, input_] := Transpose @ imageXYZ @ Dot[tmatrix, AppendConstantRow[1] @ Transpose @ input]

imageXYZ[e_] := Transpose[Take[e, 3] / maybeThreaded[P4[e]]];