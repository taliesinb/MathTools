PrivateFunction[ExpandPrimitives]

$expandPrimitivesDispatch = Dispatch[{
  p:Point[_ ? CoordinateMatrixQ] :> Thread[p],
  p:(Ball|Sphere)[_ ? CoordinateMatrixQ] :> Thread[p],
  p:(Ball|Sphere)[_ ? CoordinateMatrixQ, _] :> Thread[p],
  p:Line[_ ? CoordinateMatricesQ] :> Thread[p],
  p:Arrow[_ ? CoordinateMatricesQ, ___] :> Thread[p],
  p:(Polygon|Polyhedron)[_ ? CoordinateMatricesQ] :> Thread[p],
  (head:Polygon|Polyhedron)[l_ ? PositiveIntegerVectorsQ, arr_] :> Map[head[#, arr]&, l],
  Style[p_, s__] :> expandStyle @ Replace[p, $expandPrimitivesDispatch]
}];

expandStyle = Case[
  Style[p_List, s__] := Map[Style[#, s]&, p];
  other_             := other
];

ExpandPrimitives[primitives_, level_:{0,1}] := Replace[
  primitives,
  $expandPrimitivesDispatch,
  level
];

(**************************************************************************************************)

PrivateFunction[SimplifyGraphicsPrimitives]

SimplifyGraphicsPrimitives[primitives_] :=
  simplifyPrimitiveLists @ simplifyPrimitiveAnnotations @ simplifyPrimitiveStyles @ primitives;

(**************************************************************************************************)

$simplifyPrimitiveStyleRules = Dispatch @ {
  Directive[{}] :> {},
  Style[g_] :> g,
  Style[g_, {} | {{}}] :> g,
  Style[Style[g_, a___], b___] :> Style[g, a, b]
};

simplifyPrimitiveStyles[primitives_] :=
  ReplaceRepeated[primitives, $simplifyPrimitiveStyleRules];

(**************************************************************************************************)

$simplifyPrimitiveAnnotationRules = Dispatch @ {

  (* a single annotation with uniform primitives can use a single larger primitive *)
  anno:Annotation[{__Point} | {__Line} | {__Arrow}, __] :> RuleCondition @ joinAnnotationPrimitives[anno],

  (* fragmented uniform primitives can be combined into a single larger primitive *)
  annos:{Annotation[(head:(Point | Line | Arrow))[_], _List, type_]..} :> RuleCondition @ joinHeadAnnotations[annos, type],

  (* fragmented list primitives can be joined  *)
  annos:{Annotation[_List, _List, type_]..} :> RuleCondition @ joinListAnnotations[annos, type],

  (* singleton annos can be joined, even if they are non-uniform / wrapped in style *)
  annos:{Annotation[_, {_}, type_]..} :> RuleCondition @ joinSingletonAnnotations[annos, type]
};

joinHeadPrimitives[prims_] := Scope[
  If[Length[prims] <= 1 || !AllSameBy[prims, Length], Return @ prims];
  head = Part[prims, 1, 0];
  coords = Part[prims, All, 1];
  normFunc = If[head === Point, toCoordinateMatrix, toCoordinateArray];
  coordsArray = ToPackedRealArrays @ Apply[Join] @ Map[normFunc] @ coords;
  head[coordsArray]
];

joinAnnotationPrimitives[Annotation[prims_, args__]] :=
  Annotation[joinHeadPrimitives @ prims, args];

joinHeadAnnotations[annos_, type_] := Scope[
  primitives = joinHeadPrimitives @ Part[annos, All, 1];
  indices = Join @@ Part[annos, All, 2];
  Annotation[primitives, indices, type]
];

toCoordinateArray[e_] := If[CoordinateArrayQ[e] || VectorQ[e, CoordinateMatrixQ], e, List @ e];
toCoordinateMatrix[e_] := If[CoordinateMatrixQ[e], e, List @ e];

joinListAnnotations[annos_, type_] :=
  Annotation[Join @@ Part[annos, All, 1], Join @@ Part[annos, All, 2], type];

joinSingletonAnnotations[annos_, type_] :=
  Annotation[Part[annos, All, 1], Part[annos, All, 2, 1], type];

(* a list of singleton-index annotations can be replaced with a single such primitive annotation *)
simplifyPrimitiveAnnotations[primitives_] :=
  If[FreeQ[primitives, Annotation],
    primitives,
    ReplaceRepeated[primitives, $simplifyPrimitiveAnnotationRules]
  ];

(**************************************************************************************************)

simplifyPrimitiveLists[primitives_] :=
  ReplaceRepeated[primitives, $simplifyPrimitiveListsRules];

$simplifyPrimitiveListsRules = Dispatch @ {

  (* a single annotation with uniform primitives can use a single larger primitive. *)
  list:({__Point} | {__Line} | {__Arrow}) :>
    RuleCondition @ joinHeadPrimitives[list],

  {l___, seq:(Repeated[_Point, {2, Infinity}] | Repeated[_Line, {2, Infinity}] | Repeated[_Arrow, {2, Infinity}]), r___} :>
    RuleCondition @ {l, joinHeadPrimitives[{seq}], r},

  {single_Point | single_Line | single_Arrow} :>
    single

};

(**************************************************************************************************)

PublicFunction[SimplifyTranslate]

SimplifyTranslate[e_] := e //. Translate[Translate[g_, pos1_], pos2_] :> RuleCondition @ Translate[g, pos1 + pos2];

(**************************************************************************************************)

$coordArg1P = Point | Circle | Disk | Polygon | Line | Arrow | BSplineCurve | BezierCurve | Ball | Sphere | Cylinder | Tube | Cone | CapsuleShape | StadiumShape | Cuboid | CenteredRectangle | CenteredCuboid;
$coordArg2P = Text | Inset;
$coordRectP = Cuboid | Rectangle;

(* TODO: Will this handle nested translates? also, what about other transformations ? *)
BakeGraphicsTransformations[e_] := ReplaceRepeated[e, {
  Translate[f_, dx_ ? CoordinateVectorQ] :> RuleCondition @ ReplaceAll[f, {
    a_Arrowheads :> a,
    (head:$coordArg1P)[x_ ? CoordinateVectorOrMatrixQ, rest___]                   :> head[Threaded[dx] + x, rest],
    (head:$coordArg1P)[x_ ? CoordinateMatrixOrMatricesQ, rest___]                 :> head[Threaded[dx] + x, rest],
    (head:$coordArg2P)[first_, x_ ? CoordinateVectorQ, rest___]                   :> head[first, Threaded[dx] + x, rest],
    (head:$coordRectP)[x1_ ? CoordinateVectorQ, x2_ ? CoordinateVectorQ, rest___] :> head[Threaded[dx] + x1, Threaded[dx] + x2, rest]
  }]
}];
