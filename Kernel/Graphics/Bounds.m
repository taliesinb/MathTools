PublicFunction[PrimitiveBoxesBounds]

SetUsage @ "
PrimitiveBoxesBounds[boxes$] returns the bounds {{x$min, x$max}, {y$min, y$max}} of primitive boxes.
PrimitiveBoxesBounds[boxes$, scale$] assumes that one plot range unit corresponds to scale$ pixels.
* if no scale is provided:
* %Offset expressions are ignored
* %TextBox and %InsetBox contents are treated as a single point at their placement position.
"

PrimitiveBoxesBounds[boxes_, gs_:None] := Scope[
  setupPrimBoxesBoundDefs[];
  $aps = 5/2; $gs = gs; $fs = $ff = $fw = $fsl = $ft = Inherited;
  iBounds @ ResolveOffsets[boxes, gs]
];

(* used internally to compute the center for Rotate *)
iBounds[boxes_] := Scope[
  $p = Bag[]; $t = Identity;
  boxBound @ boxes;
  $p = BagPart[$p, All];
  ToPackedReal @ If[$p === {}, $emptyBounds, fatten /@ CoordinateBounds[$p]]
];

$eps = 10.^-6;
fatten[{z_, z_}] := {z - $eps, z + $eps};
fatten[other_] := other;

$emptyBounds = {{-$eps, $eps}, {-$eps, $eps}};

(**************************************************************************************************)

(* boxBound is set up lazily so that the primitive box registery will have been populated *)

toAlt[a_] := a;
toAlt[a__] := Alternatives[a];
toBoxHeadPatt[str_] := toAlt @@ PrimitiveBoxSignatureLookup[str];

(* TODO: make this work in 3D, handle radii that are specified as Offset *)
setupPrimBoxesBoundDefs[] := With[{
  $prims     = toBoxHeadPatt["Primitives"],
  $vec       = toBoxHeadPatt["Vector!Radius"],
  $vecvec    = toBoxHeadPatt["Vector,Vector"],
  $vecrad    = toBoxHeadPatt["Vector,Radius"],
  $mat       = toBoxHeadPatt["Matrix!Radius"],
  $matrad    = toBoxHeadPatt["Matrix,Radius"],
  $mats      = toBoxHeadPatt["Matrices!Radius"],
  $matsrad   = toBoxHeadPatt["Matrices,Radius"],
  $dirP      = _Directive | _AbsolutePointSize | Rule[FontSize | FontWeight | FontFamily | FontSlant | FontTracking, _],
  vecP       = $CoordP,
  matP       = {__List} ? CoordinateMatrixQ,
  matListP   = {__List} ? CoordinateMatricesQ},

  Clear[boxBound];

  boxBound[PointBox[p_]] /; $gs =!= None       := boxBound @ Construct[DiskBox, p, $aps / $gs];
  boxBound[ib:(_TextBox | _InsetBox)]          := insetBounds @ ib;
  boxBound[$prims[p_, ___]]                    := boxBound @ p;
  boxBound[$vec[v:vecP]]                       := StuffBag[$p, $t @ v];
  boxBound[$vecvec[v:vecP, w:vecP, ___]]       := (StuffBag[$p, $t @ v]; StuffBag[$p, $t @ w]);
  boxBound[$vecrad[v:vecP, r_:1, ___]]         := StuffBag[$p, $t @ vecCirc8[v, r]];
  boxBound[$mat[m:matP, ___]]                  := StuffBag[$p, $t @ m, 1];
  boxBound[$matrad[m:matP, r_:1, ___]]         := StuffBag[$p, $t @ matCirc8[m, r], 1];
  boxBound[$mats[ms:matListP, ___]]            := StuffBag[$p, $t /@ ms, 2];
  boxBound[$matsrad[ms:matListP, r_:1, ___]]   := StuffBag[$p, $t[matCirc8[#, r]& /@ ms], 2];
  boxBound[list_List]                          := styleBlock @ Scan[boxBound, list];
  boxBound[d:$dirP]                            := applyDir[d];
  boxBound[StyleBox[p_, opts___]]              := styleBlock[Scan[applyDir, {opts}]; boxBound @ p];
  boxBound[(JoinedCurveBox|FilledCurveBox)[curves_List, ___]] := multiCurveBound[curves];
  boxBound[GeometricTransformationBox[p_, t_]] := transBoxBound[p, t];
  (* boxBound[e_]                              := Message[PrimitiveBoxesBounds::unrecogBox, MsgExpr @ e]; *)
  boxBound[_]                                  := Null;

  Clear[setupPrimBoxesBoundDefs];
];

PrimitiveBoxesBounds::unrecogBox = "Unrecognized element ``.";

(**************************************************************************************************)

Clear[transBoxBound, multiCurveBound, applyDir, composeTransform, applyTrans];

applyDir = Case[
  AbsolutePointSize[p:$NumberP]      := Set[$aps, p/2];
  AbsolutePointSize[s_Symbol]        := % @ AbsolutePointSize  @ Lookup[$SymbolicPointSizes, s];
  e_List                             := Scan[%, e];
  d_Directive                        := Scan[%, d];
  FontSize -> fs_                    := Set[$fs, fs];
  FontFamily -> ff_                  := Set[$ff, ff];
  FontWeight -> fw_                  := Set[$fw, fw];
  FontSlant -> fs_                   := Set[$fsl, fs];
  FontTracking -> ft_                := Set[$ft, ft];
  _                                  := Null
];

(**************************************************************************************************)

(* TODO: join endpoints, which can effect bounds of BezierCurve *)
multiCurveBound[curves_] := boxBound @ ToGraphicsBoxes @ curves;

(**************************************************************************************************)

transBoxBound = Case[
  Seq[p_, v:vecP]           := applyTrans[p, ThreadedPlusOperator[v]];
  Seq[p_, m:matP]           := applyTrans[p, AffineOperator[m]];
  Seq[p_, {m:matP, v:vecP}] := applyTrans[p, AffineOperator[m, v]];
  Seq[p_, {m:matP, Center}] := With[
    {v = N[Mean /@ iBounds[p]]},
    applyTrans[p, ThreadedPlusOperator[-v] /* AffineOperator[m] /* ThreadedPlusOperator[v]]
  ];
  Seq[_, t_] := Message[PrimitiveBoxesBounds::unrecogTrans, MsgExpr @ t],
  {vecP -> $CoordP, matP -> {__List} ? CoordinateMatrixQ}
];

PrimitiveBoxesBounds::unrecogTrans = "Unrecognized geometric transform spec ``.";

(**************************************************************************************************)

SetHoldFirst[styleBlock];
styleBlock[e_] := InheritedBlock[{$aps, $fs, $ff, $fw, $fsl, $ft}, e];

$circ8 := $circ8 = ToPackedReal @ N @ ClockwiseCirclePoints[8];
vecCirc8[v_, r_] := Threaded[v] + r * $circ8;
matCirc8[m_, r_] := vecCirc8[#, r]& /@ m;

(**************************************************************************************************)

applyTrans[p_, new_] := Block[{$t = composeTransform[$t, new]}, boxBound @ p];

(* TODO: this won't simplify cases that are already involving Composition *)
composeTransform[Identity, new_] := new;
composeTransform[old_, new_] := Composition[old, new];
composeTransform[DotRightOperator[old_], DotRightOperator[new_]] := DotRightOperator[Dot[new, old]];
composeTransform[ThreadedPlusOperator[old_], ThreadedPlusOperator[new_]] := ThreadedPlusOperator[old + new];

(**************************************************************************************************)

$baseStyle := {FontSize -> $fs, FontFamily -> $ff, FontWeight -> $fw, FontSlant -> $fsl, FontTracking -> $ft};

PrimitiveBoxesBounds::unsuppInset = "Unsupported InsetBox ``."

insetBounds[e_] := If[$gs === Null, pointInsetBounds, properInsetBounds][e];

pointInsetBounds = Case[
  TextBox[_, v:$CoordP, ___]   := StuffBag[$p, $t @ v];
  InsetBox[_, v:$CoordP, ___]  := StuffBag[$p, $t @ v];
  TextBox[_] | InsetBox[_]     := StuffBag[$p, {0, 0}];
  _                            := Null
];

properInsetBounds = Case[

  i:InsetBox[_GraphicsBox, ___] :=
    boxBound @ embedInsetBoxWithScale[i, $gs];

  InsetBox[FormBox[txt_, _], pos_, offset:Except[_Rule]:ImageScaled[{0.5,0.5}], size_:Automatic, dirx:Except[_Rule]:{1,0}, opts___Rule] := Scope[
    pos = ResolveOffsets[pos, $gs];
    offset //= Replace[ImageScaled[s_] :> (s - 0.5) * 2];
    (* TODO: Maybe use DefaultBaseStyle here? *)
    If[FreeQ[{opts}, BaseStyle], opts = Sequence[opts, BaseStyle -> $baseStyle]];
    {w, h} = TextRasterSize @ Text[RawBoxes @ txt, pos, opts] + 1;
    dirx = Normalize[dirx] / $gs;
    diry = VectorRotate90[dirx];
    dirx = dirx * w/2.;
    diry = diry * h/2.;
    off = Mean[{dirx, diry} * offset] * -2.;
    points = {dirx -diry, -dirx +diry, dirx -diry, dirx + diry};
    points += Threaded[off + pos];
    StuffBag[$p, points, 1]
  ];

  other_ := Message[FindAutomaticPadding::unsuppInset, MsgExpr @ other];
];
