PrivateFunction[ExpandPrimitives]

(* TODO: use the patterns like $GPrimVecsH to express these *)

$expandPrimitivesDispatch = Dispatch[{
  p:Point[_ ? CoordinateMatrixQ] :> Thread[p],
  p:(Ball|Sphere)[_ ? CoordinateMatrixQ] :> Thread[p],
  p:(Ball|Sphere)[_ ? CoordinateMatrixQ, _] :> Thread[p],
  p:Line[_ ? CoordinateMatricesQ] :> Thread[p],
  p:Arrow[_ ? CoordinateMatricesQ, ___] :> Thread[p],
  p:(Polygon|Polyhedron)[_ ? CoordinateMatricesQ] :> Thread[p],
  (head:Polygon|Polyhedron)[l_ ? PositiveIntegerVectorsQ, arr_] :> Map[head[#, arr]&, l],
  Style[p_, s__] :> expandStyle @ Rep[p, $expandPrimitivesDispatch]
}];

expandStyle = Case[
  Style[p_List, s__] := Map[Style[#, s]&, p];
  other_             := other
];

ExpandPrimitives[primitives_, level_:{0,1}] := Rep[
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
  RepRep[primitives, $simplifyPrimitiveStyleRules];

(**************************************************************************************************)

$simplifyPrimitiveAnnotationRules = Dispatch @ {

  (* a single annotation with uniform primitives can use a single larger primitive *)
  anno:Annotation[{__Point} | {__Line} | {__Arrow}, __] :> RuleEval @ joinAnnotationPrimitives[anno],

  (* fragmented uniform primitives can be combined into a single larger primitive *)
  annos:{Annotation[(head:(Point | Line | Arrow))[_], _List, type_]..} :> RuleEval @ joinHeadAnnotations[annos, type],

  (* fragmented list primitives can be joined  *)
  annos:{Annotation[_List, _List, type_]..} :> RuleEval @ joinListAnnotations[annos, type],

  (* singleton annos can be joined, even if they are non-uniform / wrapped in style *)
  annos:{Annotation[_, {_}, type_]..} :> RuleEval @ joinSingletonAnnotations[annos, type]
};

joinHeadPrimitives[prims_] := Scope[
  If[Len[prims] <= 1 || !AllSameBy[prims, Len], Return @ prims];
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

toCoordinateArray[e_] := If[CoordinateArrayQ[e] || VecQ[e, CoordinateMatrixQ], e, List @ e];
toCoordinateMatrix[e_] := If[CoordinateMatrixQ[e], e, List @ e];

joinListAnnotations[annos_, type_] :=
  Annotation[Join @@ Part[annos, All, 1], Join @@ Part[annos, All, 2], type];

joinSingletonAnnotations[annos_, type_] :=
  Annotation[Part[annos, All, 1], Part[annos, All, 2, 1], type];

(* a list of singleton-index annotations can be replaced with a single such primitive annotation *)
simplifyPrimitiveAnnotations[primitives_] :=
  If[FreeQ[primitives, Annotation],
    primitives,
    RepRep[primitives, $simplifyPrimitiveAnnotationRules]
  ];

(**************************************************************************************************)

simplifyPrimitiveLists[primitives_] :=
  RepRep[primitives, $simplifyPrimitiveListsRules];

$simplifyPrimitiveListsRules = Dispatch @ {

  (* a single annotation with uniform primitives can use a single larger primitive. *)
  list:({__Point} | {__Line} | {__Arrow}) :>
    RuleEval @ joinHeadPrimitives[list],

  {l___, seq:(Repeated[_Point, {2, Inf}] | Repeated[_Line, {2, Inf}] | Repeated[_Arrow, {2, Inf}]), r___} :>
    RuleEval @ {l, joinHeadPrimitives[{seq}], r},

  {single_Point | single_Line | single_Arrow} :>
    single

};

(**************************************************************************************************)

PublicFunction[SimplifyTranslate]

SimplifyTranslate[e_] := e //. {
  Translate[Translate[g_, pos1_], pos2_] :> RuleEval @ Translate[g, pos1 + pos2],
  Translate[{t__Translate}, pos_] :> RuleEval @ SimplifyTranslate[Map[Translate[#, pos]&, {t}]]
}

(**************************************************************************************************)

PublicFunction[BakeGraphicsTransformations]

(* TODO: Will this handle nested translates? also, what about other transformations ? *)
BakeGraphicsTransformations[e_] := RepRep[e, {
  Translate[prims_, dx_ ? CoordinateVectorQ] :> RuleEval @ TranslatePrimitives[prims, dx]
}];
