PrivateSymbol[$customGraphicsHeadQ]

SetInitialValue[$customGraphicsHeadQ, <||>];

(**************************************************************************************************)

PrivateHead[$VectorArg, $VectorPairArg, $DeltaArg, $MatrixArg, $MatricesArg, $RadiusArg, $OpaqueArg, $PrimitivesArg, $CurveArg, $ColorArg, $PosRulesArg]

PublicFunction[SignPrimitive]

SetUsage @ "
SignPrimitive[sig$, symbol$] attaches a given signature$ to a graphics primitive symbol$.
SignPrimitive[sig$, s$1 | s$2 | $$] attaches a signature to multiple symbols.
* sig$ can be a string containing a single signature or multiple separated by '|'.
* each signature is a list of types seperated by ','.
* each type is one of the following:
| 'Vector' | a coordinate vector |
| 'Pair' | a pair of coordinate vectors |
| 'Delta' | a relative offset vector |
| 'Matrix' | a list of coordinate vectors |
| 'Matrices' | a list of matrices |
| 'Radius' | a numeric radius in coordinate space |
| 'Opaque' | an opaque value that will be ignored |
| 'Primitives' | a list of graphics primitives |
| 'Curve' | a symbolic curve |
| 'Rules' | rules from coordinate vectors to opaque objects |
* lookups against the database of signatures can be achieved via %PrimitiveSignatureLookup.
"

SignPrimitive[sig_, heads_List | heads_Alternatives] :=
  Scan[SignPrimitive[sig, #]&, List @@ heads];

$head = None;
SignPrimitive[sig_, head_Symbol] := CatchMessage @ Scope[
  $head = head;
  Scan[procSignature, toSignatures @ sig];
];

_SignPrimitive := BadArguments[];

toSignatures = Case[
  s_String := handleOpt /@ StringTrim[StringSplit[s, "|"]];
  other_   := ToList @ other;
]

handleOpt[s_] /; StringContainsQ[s, "?"] := Splice[{StringDelete[s, "?" ~~ ___], StringReplace[s, "?"->","]}];
handleOpt[s_] := s;

SetInitialValue[$primToSigs, UAssociation[]];
SetInitialValue[$sigToPrims, UAssociation[]];
SetInitialValue[$sigElemToPrims, UAssociation[]];

procSignature[str_String] := Scope[
  sigElems = parseSignatureString @ str;
  KeyUnionTo[$sigToPrims, sigElems, {$head}];
  KeyUnionTo[$primToSigs, $head, {sigElems}];
  ScanIndex1[procSignatureElem, sigElems];
];

General::badprimsig = "Bad signature `` for graphics primitive ``."
procSignature[shape_] := ThrowMessage["badprimsig", shape, $head];

Clear[parseSignatureString];
parseSignatureString[str_] := parseSignatureString[str] = Map[parseSigElem, StringSplit[str, ","]];

General::badprimsigelem = "Bad signature element `` for graphics primitive ``."
parseSigElem = Case[
  "Vector"     := $VectorArg;
  "Pair"       := $VectorPairArg;
  "Delta"      := $DeltaArg;
  "Matrix"     := $MatrixArg;
  "Matrices"   := $MatricesArg;
  "Radius"     := $RadiusArg;
  "Opaque"     := $OpaqueArg;
  "Primitives" := $PrimitivesArg;
  "Curve"      := $CurveArg;
  "Color"      := $ColorArg;
  "Rules"      := $PosRulesArg;
  sym_Symbol   := sym;
  e_           := ThrowMessage["badprimsigelem", e, $head];
];

procSignatureElem[type_, pos_] := KeyUnionTo[$sigElemToPrims, {pos, type}, {$head}];

(**************************************************************************************************)

PublicFunction[PrimitiveSignatureLookup]

SetUsage @ "
PrimitiveSignatureLookup[slot$ -> type$1] returns a list of primitive symbols which accept argument type$ at argument position slot$.
PrimitiveSignatureLookup[{rules$1, rule$2, $$}] applies multiple criteria simultaneously.
PrimitiveSignatureLookup['sig$'] returns a list of primitives that have exactly the signature sig$ (which can contain multiple specs).
PrimitiveSignatureLookup[sym$] returns a list of symbolic signatures for primitive sym$.
"

PrimitiveSignatureLookup::nosym = "`` is not a primitive symbol.";
PrimitiveSignatureLookup[expr_] := CatchMessage @ iPrimitiveSignatureLookup[expr];

iPrimitiveSignatureLookup = Case[

  pos_Integer -> sig_String :=
    Lookup[$sigElemToPrims, Key @ {pos, parseSigElem[sig]}, {}];

  rules:{__Rule} :=
    Intersection @@ Map[%, rules];

  sig_String /; StringContainsQ[sig, "|"|"?"] :=
    Union @@ Map[%, toSignatures @ sig];

  sig_String :=
    Lookup[$sigToPrims, Key @ parseSignatureString @ sig, {}];

  sym_Symbol :=
    Lookup[$primToSigs, sym, Message[PrimitiveSignatureLookup::nosym, sym]; {}];
];

(**************************************************************************************************)

(*
SignPrimitive["Vector", CompassCurve | AxesFlag | HorizontalCurve | VerticalCurve]
*)
SignPrimitive["Vector,Vector", Cuboid | Rectangle]; (* EmptyRectangle *)

SignPrimitive["Vector | Matrix", Point];
SignPrimitive["Matrix | Matrices", Polygon | Polyhedron | Line | Triangle];

(* CenteredRectangle | CenteredCuboid *)
SignPrimitive["Vector?Radius | Matrix?Radius", Circle | Disk | Sphere | Ball];
SignPrimitive["Vector?Radius", Annulus | Cube];

(* AnnotatedCoordinate ExtendedArrow | MorphismArrow | PathedText | RollingCurve | SetbackCurve | SmoothedCurve *)
SignPrimitive["Matrix", GraphicsComplex | BSplineCurve | BezierCurve | Simplex];

(* AnnotatedCoordinate ExtendedArrow | MorphismArrow | PathedText | RollingCurve | SetbackCurve | SmoothedCurve *)
SignPrimitive["Matrix?Radius | Matrices?Radius | Curve?Radius", Tube];

(* CircuitCurve | SnakeCurve *)
SignPrimitive["Pair", InfiniteLine | HalfLine];

SignPrimitive["Curve", Arrow];

(* ElbowCurve *)
SignPrimitive["Pair?Radius", Cylinder | Cone | CapsuleShape | StadiumShape];

(* VectorCurve | LoopCurve | Arrowhead | NamedIcon *)
SignPrimitive["Vector,Delta", InfiniteLine | HalfLine];

(* PlaneInset | AnchoredCurve *)
SignPrimitive["Opaque,Vector", Text | Inset];

SignPrimitive["Opaque", Arrowheads];

SignPrimitive["Primitives,Vector", Translate];

SignPrimitive["Primitives", Rotate | GeometricTransformation | Scale | Style | Annotation | Tooltip | StatusArea | PopupWindow | Mouseover | Hyperlink | EventHandler | Button];

(*
primitives whose first argument is a curve that is cooerced to a list of points
SignPrimitive["Curve", {RollingCurve, VectorCurve, SetbackCurve, AnchoredCurve, CircuitCurve, SmoothedCurve, Arrow, MorphismArrow}]
*)

(**************************************************************************************************)

PrivateSpecialFunction[DeclareCurvePrimitive, DeclareAtomicCurvePrimitive, DeclareCurveAlias]

PrivateVariable[$customCurveHeadQ]

SetUsage @ "
DeclareCurvePrimitive[head$, curveFn$] declares that head$ is a curve primitive whose points are defined by curveFn$.
DeclareCurvePrimitive[$$, boxFn$] declares that boxes should be produced by boxFn$.
* head$ will accept a first argument that is either a coordinate array or another curve primitive.
* %DiscretizeCurve will automatically work on head$[$$].
* $boxFn will be passed the same curve, but with the first argument replaced with points.
"

SetUsage @ "
DeclareAtomicCurvePrimitive[$$] is like %DeclareCurvePrimitive but does not normalize its first argument to a path.
"

SetUsage @ "
DeclareCurveAlias[head$, aliasFn$] declares that head$ is a curve that rewrites itself to another curve via aliasFn$.
"

SetInitialValue[$customCurveAliasFn, UAssociation[]];
SetInitialValue[$customCurveHeadQ, UAssociation[]];
SetInitialValue[$customCurveFn, UAssociation[]];
SetInitialValue[$customCurveBoxesFn, UAssociation[]];
SetInitialValue[$customCurveIsRecursive, UAssociation[]];

DeclareCurvePrimitive[head_Symbol, curveFn_, boxesFn_:Automatic] := (
  $customGraphicsHeadQ[head] = True;
  $customCurveHeadQ[head] = True;
  $customCurveIsRecursive[head] = True;
  $customCurveFn[head] = curveFn;
  $customCurveBoxesFn[head] = ReplaceAutomatic[boxesFn, toLineBox];
  Typeset`MakeBoxes[e_head, StandardForm | TraditionalForm, Graphics] := CurveToBoxes[e];
);

DeclareAtomicCurvePrimitive[head_, args___] := (
  DeclareCurvePrimitive[head, args];
  $customCurveIsRecursive[head] = False;
);

DeclareCurveAlias[head_, fn_] := (
  head /: Normal[curve_head] := resolveCurveAlias[curve, Identity];
  $customCurveAliasFn[head] = fn;
)

toLineBox[e_] := Construct[LineBox, First @ e];

(**************************************************************************************************)

PrivateFunction[CurveToEndpoints]

CurveToEndpoints[curve_] := Scope[
  extractEnds @ curve
];

extractEnds = Case[
  {a_, ___, b_}         := {a, b};
  Line[list_List]       := % @ list;
  Arrow[list_List, ___] := % @ list;
  DirectedEdge[a_, b_]  := {a, b};
  ObjectCoordinates[{a_, b_}] := ObjectCoordinates /@ {a, b};
  MorphismCoordinates[{a_, b_}] := MorphismCoordinates /@ {a, b};
  head_[first_, ___] := If[
    KeyExistsQ[$customCurveHeadQ, head] || KeyExistsQ[$customCurveAliasFn, head],
    % @ first,
    $Failed
  ]
];

(**************************************************************************************************)

PublicFunction[CurveToBoxes]

General::notcurve = "`` is not a curve."
General::badinnercurve = "`` contained an invalid curve.";
General::unrecogcurve = "Curve `` is not recognized.";
General::failcurve = "Curve `` did not produce a valid path."
General::failcurveboxes = "Curve `` failed to boxify."

CurveToBoxes[curve_] := Scope[
  points = CurveToPoints @ curve;
  If[FailureQ[points], Return @ {}];
  fn = Lookup[$customCurveBoxesFn, Head @ curve];
  res = Construct[fn, ReplacePart[curve, 1 -> points]];
  Which[
    Head[res] === fn, gprimMsg[curve, "unrecogcurve"],
    res === $Failed,  gprimMsg[curve, "failcurveboxes"],
    True,             res
  ]
];

CurveToBoxes[curve_] /; KeyExistsQ[$customCurveAliasFn, Head @ curve] :=
  Replace[resolveCurveAlias[curve, CurveToBoxes], $Failed -> curve];

(**************************************************************************************************)

PublicFunction[CurveToPoints]

CurveToPoints[curve:(_Line | _Circle | _BezierCurve | _BSplineCurve)] :=
  DiscretizeCurve[curve];

$ctpMsg = True;

CurveToPoints[curve_] := Scope[
  If[CoordinateMatrixQ @ curve, Return @ curve];
  curveFn = Lookup[$customCurveFn, Head @ curve];
  If[MissingQ[curveFn],
    If[$ctpMsg, Message[General::notcurve, MsgExpr @ curve]];
    ReturnFailed[];
  ];
  If[$customCurveIsRecursive @ Head @ curve,
    innerPath = Block[{$ctpMsg = False}, CurveToPoints @ First @ curve];
    If[FailureQ[innerPath],
      If[$ctpMsg, gprimMsg[curve, "badinnercurve"]];
      ReturnFailed[];
    ];
    points = curveFn @ ReplacePart[curve, 1 -> innerPath];
  ,
    points = curveFn @ curve;
  ];
  If[!CoordinateMatrixQ[points],
    If[$ctpMsg, gprimMsg[curve, If[Head[points] === curveFn, "unrecogcurve", "failcurve"]]];
    ReturnFailed[];
  ];
  points
];

CurveToPoints[curve_] /; KeyExistsQ[$customCurveAliasFn, Head @ curve] :=
  resolveCurveAlias[curve, CurveToPoints];

(**************************************************************************************************)

resolveCurveAlias[curve_, fn_] := Scope[
  aliasFn = Lookup[$customCurveAliasFn, Head @ curve];
  res = aliasFn @ curve;
  If[Head[res] === aliasFn || res === curve || res === $Failed,
    If[$ctpMsg || fn == CurveToBoxes, gprimMsg[curve, "unrecogcurve"]];
    If[type === CurveToBoxes, {}, $Failed]
  ,
    fn @ res
  ]
];

(**************************************************************************************************)

PrivateSpecialFunction[DeclareGraphicsPrimitive]

SetUsage @ "
DeclareGraphicsPrimitive[head$, signature$, fn$] declares that head$ is a custom graphics primitive with given argument signature(s) that boxifies via fn$.
DeclareGraphicsPrimitive[$$, dims$] specifies that rules should be created for either or both 2D or 3D.
* signatures are passed to
* this sets up a custom internal MakeBoxes rule for graphics.
* if fn$ returns $Failed or fails to evaluate, an appropriate message will be issued.
"

SetInitialValue[$customPrimitiveFns, UAssociation[]];

DeclareGraphicsPrimitive[head_Symbol, signature_, fn_, dims_:{2}] := (
  If[FailureQ @ SignPrimitive[signature, head], ReturnFailed[]];
  $customGraphicsHeadQ[head] = True;
  $customPrimitiveFns[head] = fn;
  If[MemberQ[dims, 2], Typeset`MakeBoxes[e_head, StandardForm | TraditionalForm, Graphics] := CustomPrimitiveToBoxes[e]];
  If[MemberQ[dims, 3], Typeset`MakeBoxes[e_head, StandardForm | TraditionalForm, Graphics3D] := CustomPrimitiveToBoxes[e]];
);

_DeclareGraphicsPrimitive := BadArguments[];

(**************************************************************************************************)

PublicFunction[CustomPrimitiveToBoxes]

SetHoldFirst[CustomPrimitiveToBoxes];

General::unrecogprim = "Unrecognized usage ``.";
General::failprim = "Failed to boxify ``.";

CustomPrimitiveToBoxes[prim_] := With[
  {fn = $customPrimitiveFns @ Head @ Unevaluated @ prim},
  {res = Block[{UnmatchedCase2 = Throw[$Failed, CustomPrimitiveToBoxes]&},
    Catch[fn @ prim, CustomPrimitiveToBoxes]]},
  Which[
    Head[res] === fn, gprimMsg[prim, "unrecogprim"],
    res === $Failed,  gprimMsg[prim, "failprim"],
    True,             res
  ]
];

gprimMsg[prim:(h_[___]), msg_] := (Message[MessageName[h, msg], MsgExpr @ prim]; {})
_gprimMsg := BadArguments[];

