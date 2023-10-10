PublicSymbol[CommutativeDiagram]

PublicOption[DebugBounds, AutoSetback, Origin]

PublicVariable[$CommutativeDiagramColorRules]

SetUsage @ "
CommutativeDiagram[items$] evaluates to a graphics object that contains a commutative diagram.
CommutativeDiagram[objects$, morphisms$] is a legacy form that is also supported.
The list items$ can consist of objects, morphisms, and arbitrary graphics primitives in any order, described below:
* an object can be one of:
| pos$ -> obj$ | object obj$ at position pos$, automatically named |
| pos$ -> 'name$' -> obj$ | provide a string name |
* obj$ can be wrapped in %Sized[$$, {w$, h$}] or %Sized[$$, diam$] which indicates overrides the automatically calculated size for purposes of arrow setback.
* pos$ is a coordinate pair {x$, y$} running down the page and to the right.
* a morphism can be one of:
| src$ => dst$ | an unlabeled arrow between objects with given names |
| src$ => dst$ -> label$ | a labeled arrow |
| {edge$, label$} | a labeled arrow |
| {edge$, label$, type$, $$} | a labeled arrow of type type$, and any additional options |
| %MorphismArrow[$$] | a fully specified arrow |
* %Morphism[$$], %UniqueMorphism[$$], %DoubleMorphism[$$], etc. that prespecify morphism type can be used for readability.
* sources and destinations can be objects or previously-declared morphisms
* names for objects and morphisms can be given as integers (referring to the objects in order they appear), or strings.
* string names are automatically generated from objects and morphisms using %FormToPlainString, which spells out greek, ignored tagged forms, etc.
* multiple identical automatic names have successive integers appended to them.
* morphisms can also be specified using 'lbl$1' => 'lbl$2'.
* the type$ given above include 'Iso', 'Epi', 'Mono', 'MapsTo', 'DoubleArrow', 'Equality', 'Line', but pre-specified morphism heads are clearer.
* %ObjectCoordinates[$$] and %MorphismCoordinates[$$] can be used to refer symbolically to locations or centroids of objects and morphisms, see their usages.
* %LabelPosition -> 'Outer' evaluates to %LabelPosition -> %AwayFrom[%ObjectCoordinates['Center']].
* %LabelPosition -> 'Inner' evaluates to %LabelPosition -> %Towards[%ObjectCoordinates['Center']].
The following options are supported:
| %Transposed | whether to interpret positions as {x$, y$} (False) or {y$, x$} (True) |
| %FlipX | whether to flip horizontally |
| %FlipY | whether to flip vertically |
| %GraphicsScale | size of one coordinate unit in pixels |
| %Alignment | alignment of text labels of objects |
| %Setback | additional setback distances of arrows, in pixels |
| %AutoSetback | whether to automatically calculate per-object setbacks |
| %DebugBounds | whether to show red rectangles around object bounds |
| %LabelFontSize | font size to use for morphisms |
| %FontSize | font size to use for objects |
| %TextModifiers | list of modifiers to apply to object and morphism labels |
| %Origin | position at which the origin of the diagram should go |
| %ColorRules | rules to apply to recolor elements of labels |
"

SetUsage @ "
ObjectCoordinates[$$] can be used as a symbolic coordinate in %CommutativeDiagram.
ObjectCoordinates[i$] represents the coordinate of object i$.
ObjectCoordinates['name$'] refers to the coordinates of the given named object.
ObjectCoordinates[{obj$1, obj$2, $$}] returns a list of object coordinates.
ObjectCoordinates['Center'] refers to the center point of the bounding box of all objects.
ObjectCoordinates[spec$, fn$] applies fn$ to the resolve of ObjectCoordinates[spec$].
"

SetUsage @ "
MorphismCoordinates[$$] can be used as a symbolic coordinate in %CommutativeDiagram.
MorphismCoordinates[$$] represents the half-way point along the given arrow's path.
MorphismCoordinates[$$, pos$] represents a given point along the arrow's path.
MorphismCoordinates[n$] represents the n$th declared morphism.
MorphismCoordinates['name$'] represents the given named morphism.
MorphismCoordinates['src$' => 'dst$'] represents the first morphism between endpoints.
MorphismCoordinates['src$' => 'dst$' -> n$] represents the n$th morphism between endpoints.
* generally it is not needed, since morphisms can be specified using their names or the spec 'lbl$1' => 'lbl$2'.
* %DoubleMorphism automatically uses %MorphismCoordinates when necessary."

Options[CommutativeDiagram] = JoinOptions[
  Transposed             -> False,
  FlipX                  -> False,
  FlipY                  -> False,
  GraphicsScale          -> 120,
  Alignment              -> Center,
  Setback                -> Automatic,
  AutoSetback            -> True,
  DebugBounds            -> False,
  LabelFontSize          -> 18,
  FontFamily             :> $MathFont,
  FontSize               -> 20,
  TextModifiers          -> {},
  Origin                 -> {0, 0},
  ColorRules             -> Automatic,
  $morphismArrowOptions
];

DeclareGraphicsPrimitive[CommutativeDiagram, "Rules", cdToBoxes];

cdToBoxes[cd_] := ToGraphicsBoxes @ cdToPrimitives @ cd;

SetInitialValue[$CommutativeDiagramColorRules, {}];

(**************************************************************************************************)

$cdPatterns = {
  CommutativeDiagram[_List, _List, ___Rule],
  CommutativeDiagram[Rule[_List, _List], _List, ___Rule],
  CommutativeDiagram[_List, ___Rule]
};

Scan[(Format[cd:#, StandardForm] := cdToGraphics[cd])&, $cdPatterns];

cdToGraphics[cd:CommutativeDiagram[Shortest[___], opts___Rule]] :=
  ScaleGraphics[
    cdToPrimitives[cd],
    GraphicsScale -> Lookup[{opts}, GraphicsScale, 120],
    ImagePadding -> Lookup[{opts}, ImagePadding, Automatic],
    AdjustFontSize -> False
  ];

(**************************************************************************************************)

Scan[(CommutativeDiagram /: Normal[cd:#] := cdToPrimitives[cd])&, $cdPatterns];

(**************************************************************************************************)

cdToPrimitives[CommutativeDiagram[r:Rule[_List, _List], morphisms_List, opts___Rule]] :=
  cdToPrimitives @ CommutativeDiagram[Prepend[morphisms, r], opts];

cdToPrimitives[CommutativeDiagram[objects_List, morphisms_List, opts___Rule]] :=
  cdToPrimitives @ CommutativeDiagram[Join[objects, morphisms], opts];

$objectNames = {};
$inheritedOptions = Sequence[];

cdToPrimitives[CommutativeDiagram[items_List, opts___Rule]] := Scope[
  UnpackOptionsAs[CommutativeDiagram,
    {FilterOptions[CommutativeDiagram, opts]},
    alignment, $setback, labelFontSize, fontSize, fontFamily,
    graphicsScale, $autoSetback, $debugBounds,
    transposed, flipX, flipY, textModifiers, origin, colorRules
  ];
  $objectNames = {};
  $objectCoordinates = {};
  $objectSizes = UAssociation[];
  $itemSize = Automatic;
  $morphismNames = {};
  $morphismCurves = Association[];
  $inheritedOptions = opts;

  SetAutomatic[colorRules, $CommutativeDiagramColorRules];
  colorModifierFn = toRecoloringFunction @ colorRules;
  {$objectTextModifierFn, $morphismTextModifierFn} = Map[
    Composition[toModifierFunction[#], colorModifierFn]&,
    If[AssociationQ[textModifiers],
      Lookup[textModifiers, {"Objects", "Morphisms"}, {}],
      {textModifiers, textModifiers}
    ]
  ];

  $calculateLabelSizes = $autoSetback || $debugBounds;

  $currentFontSize = fontSize; (* <- for recoloring rule *)
  objectPrimitives = parseObject /@ items;

  SetAutomatic[$setback, Rectangular[{15, 5}]];
  initialSetback = $setback;

  $toHigherPath = toHigherPath;
  $currentFontSize = labelFontSize; (* <- for recoloring rule *)
  morphismPrimitives = parseMorphism /@ items;
  morphismPrimitives //= ReplaceAll[$morphismCanonicalizationRules];

  $objectCoordinates = MapColumn[# + origin&, 2, $objectCoordinates];
  $coordReplacement = Dispatch @ Append[$objectCoordinates, z_String :> unresolvedCoord[z]];
  $biasedCenterCoords := $biasedCenterCoords = resolveObjectCoords[ObjectCoordinates["BiasedCenter"]];

  (* we save these so we can call resolveCoordinates *)
  saveMorphismCoords @ morphismPrimitives;
  primitives = resolveCoordinates @ {objectPrimitives, morphismPrimitives};
  primitives = ReplacePrimitiveCoordinates[primitives, $coordReplacement];
  arrowOpts = DeleteOptions[
    {FilterOptions[MorphismArrow, opts]},
    {Setback, LabelFontSize, LabelFontSize, GraphicsScale}];
  {
    Sequence @@ arrowOpts,
    BendRadius -> 0.25,
    GraphicsScale -> graphicsScale,
    Setback -> initialSetback,
    LabelFontSize -> labelFontSize,
    FontSize -> fontSize,
    FontFamily -> fontFamily,
    primitives
  } /. $primitiveCanonicalizationRules
];

_cdToPrimitives := BadArguments[];

(**************************************************************************************************)

PrivateFunction[toModifierFunction]

toModifierFunction = Case[
  {} | None | Identity := Identity;
  f_                   := f;
  list_List            := Composition @@ list;
];

(**************************************************************************************************)

$morphismCanonicalizationRules = {
  MorphismArrow[DirectedEdge[a_, b_], rest___] :>
    MorphismArrow[{a, b}, rest]
};

$primitiveCanonicalizationRules = {
  cd_CommutativeDiagram :> cd,
  Rule[Setback, sb_] :>
    RuleCondition @ Rule[Setback, toScaled[sb]],
  Rule[LabelPosition, s:("Outer"|"Inner")] :>
    RuleCondition @ Rule[LabelPosition, If[s === "Outer", AwayFrom, Towards][$biasedCenterCoords]]
};

toScaled = Case[
  {a_, b_}              := {% @ a, % @ b};
  Rectangular[{a_, b_}] := Rectangular[N @ {a, b} / graphicsScale];
  p:$NumberP            := N @ p / graphicsScale;
  other_                := other;
];

CommutativeDiagram::unresolvedcoord = "Could not resolve symbolic coordinate ``, available are: ``.";
unresolvedCoord[z_] := (
  Message[CommutativeDiagram::unresolvedcoord, z, StringRiffle[$objectNames, ","]];
  {2, -1.5}
)

CommutativeDiagram::badargs = "`` was not a valid specification."
cdToPrimitives[other_] := (
  Message[CommutativeDiagram::badargs, MsgExpr @ other];
  {}
);

(**************************************************************************************************)

CommutativeDiagram::badrecolor = "Bad recoloring rule element ``."
toRecoloringFunction = Case[
  {} | None     := Identity;
  list_List     := ReplaceAll @ Map[toRecolorRule, list];
  other_        := (Message[CommutativeDiagram::badrecolor, other]; Identity);
];

toRecolorRule = Case[
  (f:FunctorSymbol[name_]) -> {c1_, c2_} :=
    RuleDelayed[f, RuleCondition @ ColoredFunctorSymbol[name, c1, c2, $currentFontSize]];
  other_ -> {c1_, c2_} :=
    RuleDelayed[f, RuleCondition @ ColoredFunctorSymbol[name, c1, c2, $currentFontSize]];
  a_ -> (form_?StyleFormHeadQ) :=
    RuleDelayed[(z:a), RuleCondition[form[z]]];
  a_ -> color:$ColorPattern    :=
    RuleDelayed[(z:a), Style[z, color]];
  _Rule := (
    Message[CommutativeDiagram::badrecolor, other];
    Nothing
  );
  a_ /; MemberQ[$CommutativeDiagramColorRules, a -> _] := Splice @ Cases[
    $CommutativeDiagramColorRules,
    rule:Rule[a, _] :> %[rule]
  ];
  other_ := (
    Message[CommutativeDiagram::badrecolor, other];
    Nothing
  );
];

(**************************************************************************************************)

PublicHead[EqualityMorphism, UniqueMorphism, DoubleMorphism, Morphism, ProMorphism, AdjointMorphism, LongAdjointMorphism]

$morphismHeadP = Morphism | DoubleMorphism | UniqueMorphism | EqualityMorphism | ProMorphism | AdjointMorphism | LongAdjointMorphism;

Scan[DeclareCurveAlias[#, toMorphism]&, $morphismHeadP];

(*
Scan[DeclareGraphicsPrimitive[#, "Curve", fixedMorphismBoxes]&, $morphismHeadP]; *)

fixedMorphismBoxes[m_] := With[
  {m2 = toMorphism[m]},
  If[m2 === m, $Failed, ToGraphicsBoxes @ m2]
];

$toHigherPath = Identity;
toMorphism := Case[
  Morphism[args___] := MorphismArrow[args];
  UniqueMorphism[path_, lbl:Except[_Rule]:None, args___]   := MorphismArrow[path, lbl, args, ArrowDashing -> Dashed];
  EqualityMorphism[path_, lbl:Except[_Rule]:None, args___] := MorphismArrow[path, lbl, "Equality", args];
  ProMorphism[path_, lbl:Except[_Rule]:None, args___]      := MorphismArrow[path, lbl, "Proarrow", args];
  AdjointMorphism[path_, args___]                          := MorphismArrow[$toHigherPath @ path, None, "Adjoint", args, ArrowThickness -> 1.25];
  LongAdjointMorphism[path_, args___]                      := MorphismArrow[$toHigherPath @ path, None, "LongAdjoint", args, ArrowThickness -> 1.25];
  DoubleMorphism[path_, lbl:Except[_Rule]:None, args___]   := MorphismArrow[$toHigherPath @ path, lbl, "DoubleArrow", args];
  other_ := other;
];

toHigherPath = Case[
  {a_, b_} | (a_ => b_)          := {toHigherPathElem @ a, toHigherPathElem @ b};
  MorphismCoordinates[ab:{_, _}] := Map[toHigherPathElem @ MorphismCoordinates @ #&, ab];
  head_Symbol[path_, args___] /; $customCurveHeadQ[head] := head[toHigherPath @ path, args];
  other_                         := other;
];

$higherCoordP = _DirectedEdge | _UndirectedEdge | Rule[_, _Integer] | _String | _Integer;
CommutativeDiagram::badhighercoord = "2-morphism endpoint `` is not recognized."
toHigherPathElem = Case[
  name_String /; MemberQ[$objectNames, name] := name;
  pos:$CoordP                                := pos;
  m_MorphismCoordinates                      := m;
  o_ObjectCoordinates                        := o;
  Placed[spec:$higherCoordP, pos:$NumberP]   := MorphismCoordinates[spec, pos];
  spec:$higherCoordP                         := MorphismCoordinates[spec];
  spec_                                      := (Message[CommutativeDiagram::badhighercoord, MsgExpr @ spec]; {0, 0})
];

(**************************************************************************************************)

parseObject = Case[
  objs:Rule[{__List}, _List] ? threadedObjectsQ := Splice @ Map[%, Thread[objs]];
  obj:({_, _} -> _)                             := placeObject[obj];
  _                                             := Nothing;
];

threadedObjectsQ = Case[
  pos:{__List} -> lbls_List := CoordinateMatrix2DQ[pos] && SameLengthQ[pos, lbls];
  _                         := False;
]

(**************************************************************************************************)

PublicHead[Sized]

placeObject = Case[
  Rule[pos_, obj_] :=
    % @ Rule[pos, Rule[makeAutoName[obj, $objectNames], obj]];
  Rule[pos_, Rule[lbl_, obj_]] := (
    AppendTo[$objectCoordinates, lbl -> {1,-1}*doFlip[pos]];
    AppendTo[$objectNames, lbl];
    fmtLabel[lbl, obj]
  );
];

makeAutoName[expr_, list_] := Scope[
  autoName = FormToPlainString @ expr;
  If[MemberQ[list, autoName],
    names = autoName <> #& /@ {"2", "3", "4", "5", "6", "8", "9", "10", "11", "12"};
    autoName = SelectFirst[names, !MemberQ[list, #]&];
  ];
  autoName
];

(**************************************************************************************************)

$legacyObjP = (_String|Integer);
parseMorphism = Case[
  {_, _} -> _                                        := Nothing;
  ({__List} -> _List) ? threadedObjectsQ             := Nothing;
  Null                                               := Nothing;
  Setback -> sb_                                     := ($setback = sb; Nothing);
  opt:(_Symbol -> _)                                 := flipSymbolicPositions @ opt;

  DirectedEdge[a_, DirectedEdge[b_, c_]] :=
    % /@ {DirectedEdge[a, b], DirectedEdge[b, c]};
  DirectedEdge[a_, DirectedEdge[b_, DirectedEdge[c_, d_]]] :=
    % /@ {DirectedEdge[a, b], DirectedEdge[b, c], DirectedEdge[c, d]};
  DirectedEdge[a_, DirectedEdge[b_, c_]] -> {r1_, r2_} :=
    % /@ {DirectedEdge[a, b] -> r1, DirectedEdge[b, c] -> r2};
  DirectedEdge[a_, DirectedEdge[b_, DirectedEdge[c_, d_]]] -> {r1_, r2_, r3_} :=
    % /@ {DirectedEdge[a, b] -> r1, DirectedEdge[b, c] -> r2, DirectedEdge[c, d] -> r3};

  a:$legacyObjP -> b:$legacyObjP                     := % @ {DirectedEdge[a, b]};
  {a:$legacyObjP -> b:$legacyObjP, args__}           := % @ {DirectedEdge[a, b], args};
  DirectedEdge[a_, b_, lbl]                          := % @ {DirectedEdge[a, b], lbl};
  de_DirectedEdge                                    := % @ {de};
  de_DirectedEdge -> rhs_                            := % @ {de, rhs};

  {DirectedEdge[s_, t_], lbl_:None, args___} :=
    processMorphism1 @ MorphismArrow[{s, t}, Switch[lbl, None, {}, _List, lbl, _, {{0.5, Above} -> lbl}], args];

  cd_CommutativeDiagram                              := flipSymbolicPositions @ Append[cd, Unevaluated @ $inheritedOptions];

  other_                                             := processMorphism1 @ other;
]

(**************************************************************************************************)

$coordinateCanonicalizationRules = {
  cd_CommutativeDiagram :> cd,
  c_ObjectCoordinates :> RuleCondition @ resolveObjectCoords[c],
  c_MorphismCoordinates :> RuleCondition @ resolveMorphismCoords[c]
};

resolveCoordinates[e_] := ReplaceAll[e, $coordinateCanonicalizationRules];

(**************************************************************************************************)

PublicHead[ObjectCoordinates, MorphismCoordinates]

CommutativeDiagram::badobjlbl = "No object named `` in ObjectCoordinates."
CommutativeDiagram::badobjspec = "`` is not a valid specification for ObjectCoordinates."
CommutativeDiagram::objcspeclen = "Object coordinates for `` requested, but only `` available."

$allObjCoords := Values @ $objectCoordinates;

resolveObjectCoords = Case[
  ObjectCoordinates[All]            := $allObjCoords;
  ObjectCoordinates["Center"]       := Mean @ CoordinateBoundingBox @ $allObjCoords;
  ObjectCoordinates["BiasedCenter"] := Lerp[Mean @ $allObjCoords, % @ ObjectCoordinates @ "Center", .9];
  ObjectCoordinates[i_Integer]      := ReplaceMissing[SafePart[$objectCoordinates, i, 2], Message[CommutativeDiagram::objcspeclen, i, Length @ $objectCoordinates]; {0, 0}];
  ObjectCoordinates[lbl_String]     := Lookup[$objectCoordinates, lbl, Message[CommutativeDiagram::badobjlbl, lbl]; {0, 0}];
  ObjectCoordinates[list_List]      := Map[% @ ObjectCoordinates[#]&, list];
  ObjectCoordinates[spec_, fn_]     := fn @ % @ ObjectCoordinates[spec];
  spec_      := (Message[CommutativeDiagram::badobjspec, spec]; {0, 0});
]

CommutativeDiagram::badmorphspec = "Cannot resolve morphism coordinates for ``."
resolveMorphismCoords = Case[
  MorphismCoordinates[spec_]                 := % @ MorphismCoordinates[spec, .5];
  MorphismCoordinates[list_List, pos_]       := Map[% @ MorphismCoordinates[#, pos]&, list];
  MorphismCoordinates[curve_, pos_?NumberQ]  := resolveCurvePos[resolveCoordinates @ resolveMorphism @ curve, pos];
  other_                                     := (Message[CommutativeDiagram::badmorphspec, MsgExpr @ other]; {0, 0});
];

CommutativeDiagram::badmorphname = "No morphism with name ``."
CommutativeDiagram::badmorphconn = "No morphism with connection ``."
CommutativeDiagram::badmorphconnind = "No morphism with index `` for connection ``."
CommutativeDiagram::badmorphind = "No morphism with index ``."

resolveMorphism = Case[
  spec_DirectedEdge | spec_UndirectedEdge :=
    %[spec -> 1];
  name_String := Scope[
    ind = IndexOf[$morphismNames, name];
    If[MissingQ[ind], ReturnFailed[CommutativeDiagram::badmorphname, name]];
    Part[$morphismCurves @ name, 1]
  ];
  i_Integer := Scope[
    morph = SafePart[$morphismCurves @ None, i];
    If[MissingQ[morph], ReturnFailed[CommutativeDiagram::badmorphind, i]];
    morph
  ];
  spec_ -> i_Integer := Scope[
    morphList = Lookup[$morphismCurves, spec, ReturnFailed[CommutativeDiagram::badmorphconn, spec]];
    morph = SafePart[morphList, i];
    If[MissingQ[morph], ReturnFailed[CommutativeDiagram::badmorphconnind, i, name]];
    morph
  ];
  other_ := Message[CommutativeDiagram::badmorphspec, other];
];

(**************************************************************************************************)

resolveCurvePos[$Failed, _] := {{0,0}, {1,0}};

resolveCurvePos[l_List, pos_] := resolveCurvePos[Line @ l, pos];

resolveCurvePos[MorphismArrow[curve_, ___], pos_] :=
  resolveCurvePos[curve, pos];

CommutativeDiagram::morphcoordsfail = "Cannot resolve position along curve ``."
resolveCurvePos[curve_, pos_] := Scope[
  curve = ReplacePrimitiveCoordinates[curve, $coordReplacement];
  points = DiscretizeCurve @ curve;
  If[!CoordinateMatrix2DQ[points], Message[CommutativeDiagram::morphcoordsfail, curve]; Return @ {0, 0}];
  PointAlongLine[points, Scaled[pos]]
];

(**************************************************************************************************)

doFlip[pos_] := ApplyFlip[pos, {flipX, flipY}, transposed];

flipSymbolicPositions[expr_] := expr /. side:($SidePattern|Above|Below) :> RuleCondition[doFlip[side]];

(**************************************************************************************************)

$itemSize = Automatic;

fmtLabel[lbl_, Sized[obj_, size_]] := Scope[
  $itemSize = If[NumberQ[size], size/2, size];
  %[lbl, obj]
];

fmtLabel[lbl_, None] := (
  If[$calculateLabelSizes, $objectSizes[lbl] = ReplaceAutomatic[$itemSize, {1, 1}]];
  Nothing
);

fmtLabel[lbl_, obj_] := Scope[
  text = Text[$objectTextModifierFn @ obj, lbl, Lookup[$SideToCoords, alignment]];
  If[$calculateLabelSizes,
    size = $itemSize;
    If[ContainsQ[size, Automatic],
      text2 = Append[text, BaseStyle -> {FontSize -> fontSize, FontFamily -> fontFamily}];
      isize = N[TextRasterSize @ text2];
      If[size === Automatic, size = {Automatic, Automatic}];
      size = MapThread[ReplaceAutomatic, {size, isize}];
    ];
    $objectSizes[lbl] ^= size;
    If[$debugBounds,
      Tooltip[
        {text, FaceForm[None], EdgeForm[Red], If[NumberQ @ size, {Red, Circle[lbl, size/graphicsScale]}, CenteredRectangle[lbl, size/graphicsScale]]},
        Round[size * graphicsScale]
      ],
      text
    ]
  ,
    text
  ]
];

fmtLabel[lbl_, c_Customized] := customizedBlock[c, {Alignment} :> {alignment}, fmtLabel[lbl, #]&];

(**************************************************************************************************)

$digits = {"2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"};
saveMorphismCoords = Case[
  e_List                    := Scan[%, e];
  m:(MorphismArrow|$morphismHeadP)[path_, rest___] := Scope[
    KeyAppendTo[$morphismCurves, None, path];
    st = findSourceTarget @ path;
    If[st === None, Return[]];
    {src, tgt} = st;
    lbl = SafePart[{rest}, 1];
    If[!MissingQ[lbl] && !RuleQ[lbl],
      autoName = makeAutoName[lbl, $morphismNames];
      AppendTo[$morphismNames, autoName];
      KeyAppendTo[$morphismCurves, autoName, path];
    ];
    KeyAppendTo[$morphismCurves, DirectedEdge[src, tgt],   path];
    KeyAppendTo[$morphismCurves, UndirectedEdge[src, tgt], path];
    KeyAppendTo[$morphismCurves, UndirectedEdge[tgt, src], path];
  ];
  _ := Null;
];

$edgeyHead = (List | Rule | DirectedEdge);

CommutativeDiagram::badobjindex = "No object with index ``.";

getObjectName[i_] := If[
  1 <= Abs[i] <= Length[$objectNames],
  Part[$objectNames, i],
  Message[CommutativeDiagram::badobjindex, i]; {0, 0}
];

processMorphism1 = Case[
  m:($morphismHeadP[__])                            := % @ toMorphism @ m;
  MorphismArrow[$edgeyHead[i_Integer, j_], args___] := % @ MorphismArrow[{getObjectName @ i, j}, args];
  MorphismArrow[$edgeyHead[i_, j_Integer], args___] := % @ MorphismArrow[{i, getObjectName @ j}, args];
  other_                                            := processMorphism2 @ flipSymbolicPositions @ other;
];

processMorphism2 = Case[
  ma_MorphismArrow /; TrueQ[$autoSetback] && FreeQ[ma, Setback] := Scope[
    path = First @ ma;
    st = findSourceTarget @ path;
    If[st === None, Return @ ma];
    {src, tgt} = st;
    {sz1, sz2} = lookupObjectSize /@ {src, tgt};
    setback = Map[makeEndpointSetback, {sz1, sz2}];
    setback = ExtendSetback[setback, $setback];
    % @ Append[ma, Setback -> setback]
  ];
  ma_MorphismArrow /; TrueQ[$morphismTextModifierFn =!= Identity] && FreeQ[ma, TextModifiers] :=
    % @ Append[ma, TextModifiers -> ($morphismTextModifierFn /. HoldPattern[$currentFontSize] :> RuleCondition[$currentFontSize])];
  other_ := other;
]

makeEndpointSetback = Case[
  {a_, b_} := Rectangular[{a, b}];
  a_       := a;
];

lookupObjectSize = Case[
  s_String := Lookup[$objectSizes, s, {15, 15}];
  _        := {15, 15};
];

CommutativeDiagram::resolvesrctgt = "Cannot resolve source and target of MorphismArrow with path ``."
findSourceTarget = Case[
  {a_, b_} | (a_ => b_)               := findST /@ {a, b};
  HorizontalCurve[a_, ___]            := % @ {a, None};
  VerticalCurve[a_, ___]              := % @ {a, None};
  MorphismCoordinates[list_List, ___] := None;
  _AnchoredCurve                      := None;
  other_                              := Scope[
    st = CurveToEndpoints[other];
    If[FailureQ[st],
      Message[CommutativeDiagram::resolvesrctgt, other]; None,
      findST /@ st
    ]
  ];
];

findST = Case[
  _                        := None;
  v_ ? CoordinateVector2DQ := v;
  name_String              := name;
  ObjectCoordinates[spec_] := % @ spec;
  i_Integer                := getObjectName[i];
]

(**************************************************************************************************)

PublicFunction[CommutativeSquare]

SetUsage @ "
CommutativeSquare[{nw$, ne$, sw$, se$}, {n$, s$, w$, e$}] constructs a %CommutativeDiagram with 4 objects and 4 morphisms.
* additional morphisms can be specified after the four.
* the morphisms n$, s$, w$, e$ can be given as label$ or {label$, type$, opts$$}.
"

CommutativeSquare[{nw_, ne_, sw_, se_}, {n_, s_, w_, e_, extra___}, opts___Rule] :=
  CommutativeDiagram[
    {{1, 1} -> nw, {2, 1} -> ne, {1, 2} -> sw, {2, 2} -> se},
    {toComSugarArrow[1 => 2, n],
     toComSugarArrow[2 => 4, e],
     toComSugarArrow[1 => 3, w],
     toComSugarArrow[3 => 4, s],
     extra},
    opts,
    LabelPosition -> "Outer"
  ];

(**************************************************************************************************)

PublicFunction[InTriangleDiagram, OutTriangleDiagram]

SetUsage @ "
InTriangleDiagram[{l$, r$, b$}, {lr$, lb$, rb$}] constructs a %CommutativeDiagram with 3 objects and 3 morphisms connecting them.
InTriangleDiagram[$$, side$] places b$ below on side side$.
* side$ can be %Left, %Center, or %Right.
* the morphisms can be given as label$ or {label$, type$, opts$$}.
* additional morphisms can be included after the three.
"

SetUsage @ "
OutTriangleDiagram[{t$, l$, r$}, {tl$, tr$, lr$}] constructs a %CommutativeDiagram with 3 objects and 3 morphisms connecting them.
OutTriangleDiagram[$$, side$] places x$ below on side side$.
* side$ can be %Left, %Center, or %Right.
* the morphisms can be given as label$ or {label$, type$, opts$$}.
* additional morphisms can be included after the three.
"

$triCoord = With[{r = 1.125, s = 1.41421}, {Left -> {0, r}, Center -> {s/2, s}, Right -> {r, r}}];

InTriangleDiagram[{l_, r_, b_, obs___}, {lr_, lb_, rb_, mors___}, side:(Left|Center|Right):Center, opts___Rule] :=
  commutativeTriangle[{l, r, b, obs}, {lr, lb, rb, mors}, False, side, opts];

OutTriangleDiagram[{t_, l_, r_, obs___}, {tl_, tr_, lr_, mors___}, side:(Left|Center|Right):Center, opts___Rule] :=
  commutativeTriangle[{l, r, t, obs}, {lr, tl, tr, mors}, True, side, opts];

commutativeTriangle[{l_, r_, x_, obs___}, {l2r_, l2x_, r2x_, mors___}, top_, side_, opts___] := Scope[
  {xx, rx} = Lookup[$triCoord, side] + 1;
  {xy, lry} = If[top, {1, 2}, {2, 1}];
  rev = If[top, Identity, Reverse];
  CommutativeDiagram[Flatten @ {
    {{xx, xy}  -> x, {1, lry}  -> l, {rx, lry} -> r},
    obs,
    toComSugarArrow[rev[1 => 2], l2x],
    toComSugarArrow[rev[1 => 3], r2x],
    toComSugarArrow[2 => 3, l2r],
    mors},
    LabelPosition -> "Outer",
    opts
  ]
];

(**************************************************************************************************)

PublicFunction[DoubleTriangleDiagram]

SetUsage @ "
DoubleTriangleDiagram[{l$, m$, r$, x$}, {lm$, rm$, lx$, m$, rx$}] constructs a %CommutativeDiagram with 4 objects and 5 morphisms connecting them.
DoubleTriangleDiagram[$$, side$] places the bottom object on side$.
* l$ is placed on the left, m$ in the middle, r$ on the right, and x$ below or above.
* the morphisms can be given as label$ or head$[label$, opts$$], where head$ is %Morphism, DoubleMorphism%, etc.
* the direction of the arrows between l$, m, r$ and x$ are always downward.
* additional morphisms can be included after the four.
"

With[{r = 1.125, s = 1.41421}, {t = s/2},
$cdtCoordRules = {
  Top         -> {{0, 1}, {s, 1}, {t, 0}},
  TopRight    -> {{0, 1}, {r, 1}, {r, 0}},
  BottomLeft  -> {{0, 0}, {r, 0}, {0, 1}},
  Bottom      -> {{0, 0}, {s, 0}, {t, 1}},
  BottomRight -> {{0, 0}, {r, 0}, {r, 1}}
}];

DoubleTriangleDiagram[{l_, m_, r_, x_}, {lm_, rm_, lx_, mx_, rx_, extra___}, side:(Bottom|Top):Bottom, opts___Rule] := Scope[
  pos = {{1, 1}, {2, 1}, {3, 1}, {2, 2}};
  rev = Identity;
  If[side === Top, rev = Reverse; pos //= MapAt[If[# == 2, 1, 2]&, {All, 2}]];
  CommutativeDiagram[{
    Splice @ RuleThread[pos, {l, m, r, x}],
    toComSugarArrow[1 => 2, lm],
    toComSugarArrow[3 => 2, rm],
    toComSugarArrow[rev[1 => 4], lx],
    toComSugarArrow[rev[2 => 4], mx],
    toComSugarArrow[rev[3 => 4], rx],
    extra},
    LabelPosition -> "Outer",
    opts
  ]
];

toComSugarArrow[edge_, (head:$morphismHeadP)[args___]] := head[edge, args];
toComSugarArrow[edge_, label_] := MorphismArrow @@ ToList[edge, label];
toComSugarArrow[edge_, Reversed[label_]] := toComSugarArrow[Reverse @ edge, label];

(**************************************************************************************************)

PublicFunction[ParallelArrowDiagram]

SetUsage @ "
ParallelArrowDiagram[{l$, r$}, {t$, b$, v$}] constructs a %CommutativeDiagram with 2 objects and 2 morphisms connecting them.
ParallelArrowDiagram[$$, {t$, b$, v$}] specifies the double morphism v$ between the two parallel morphisms.
ParallelArrowDiagram[{pos$l -> l$, pos$r -> r$}, $$] specifies positions for l$ and r$.
* l$ is placed on the left, r$ on the right.
* $t connects l$ and r$ above, and b$ connects l$ and r$ below. v$ is a vertical double morphism between them.
"

ParallelArrowDiagram[{lp_ -> l_, rp_ -> r_}, {t_, b_, v_:Null, rest___}, opts___Rule] := Scope[
  rev = If[Head[v] === Reversed, v //= First; Reverse, Identity];
  curve = Lookup[{opts}, CurveFunction, TrapezoidCurve];
  bend = Lookup[{opts}, BendRadius, .33];
  CommutativeDiagram[{
    lp -> l, rp -> r,
    toComSugarArrow[curve[{OC @ 1, OC @ 2}, bend], t],
    toComSugarArrow[curve[{OC @ 1, OC @ 2}, -bend], b],
    If[v =!= Null, DoubleMorphism[MorphismCoordinates[rev @ {1, 2}], v, Setback -> 10, LabelPosition -> Right]],
    rest
  }, LabelPosition -> "Outer", FilterOptions @ opts]
];

ParallelArrowDiagram[{l:Except[_Rule], r:Except[_Rule]}, morphisms_List, opts___Rule] :=
  ParallelArrowDiagram[{{1, 1} -> l, {2, 1} -> r}, morphisms, opts];

(**************************************************************************************************)

PublicFunction[AdjunctionDiagram]

SetUsage @ "
AdjunctionDiagram[{l$, r$}, {lr$, rl$}] constructs a %CommutativeDiagram representing the adjunction between l$ and r$.
AdjunctionDiagram[{pos$l -> l$, pos$r -> r$}, $$] specifies positions for l$ and r$.
"

AdjunctionDiagram[{lpos_ -> l_, rpos_ -> r_}, {lr_, rl_, extra___}, opts___Rule] := Scope[
  curve = Lookup[{opts}, CurveFunction, TrapezoidCurve];
  bend = Lookup[{opts}, BendRadius, .33];
  CommutativeDiagram[{
    lpos -> l, rpos -> r,
    toComSugarArrow[curve[{OC @ 1, OC @ 2}, bend], lr],
    toComSugarArrow[curve[{OC @ 2, OC @ 1}, bend], rl],
    LongAdjointMorphism[2 => 1, Setback -> 30],
    extra},
    LabelPosition -> "Outer", FilterOptions @ opts
  ]
];

AdjunctionDiagram[{l_, r_}, morphisms_List, opts___Rule] :=
  AdjunctionDiagram[{{1, 1} -> l, {2, 1} -> r}, morphisms, opts]

(**************************************************************************************************)

PublicFunction[AdjointTripleDiagram]

SetUsage @ "
AdjointTripleDiagram[{a$, b$}, {l$, r$, ba$}] constructs a %CommutativeDiagram representing a triple adjunction.
* l$ is the morphism on the left, r$ is the morphism on the right, and ba$ is the reverse morphism.
"

OC = ObjectCoordinates;

AdjointTripleDiagram[{a_, b_}, {abl_, abr_, ba_, extra___}, opts___Rule] := Scope[
  curve = Lookup[{opts}, CurveFunction, TrapezoidCurve];
  bend = Lookup[{opts}, BendRadius, .5];
  setback = Lookup[{opts}, "AdjointSetback", 5];
  CommutativeDiagram[{
    {1, 1} -> a, {1, 2} -> b,
    toComSugarArrow[curve[{OC @ 1, OC @ 2}, -bend], abl],
    toComSugarArrow[curve[{OC @ 1, OC @ 2}, bend], abr],
    toComSugarArrow[ObjectCoordinates @ {2,1}, ba],
    AdjointMorphism[3 => 1, Setback -> {0, setback}], AdjointMorphism[2 => 3, Setback -> {setback, 0}],
    extra},
    FilterOptions @ opts,
    LabelPosition -> Center, LabelOrientation -> Horizontal
  ]
];

(**************************************************************************************************)

PublicFunction[ArrowDiagram]

SetUsage @ "
ArrowDiagram[a$, b$, f$] constructs a %CommutativeDiagram consisting of a single arrow f$ between a% and b%.
* pos$ -> obj$ specifies obj$ should be at position pos$, defaulting to below a$.
"

ArrowDiagram[a_, b_, f_, opts___Rule] := Scope[
  $apos = {1, 1};
  CommutativeDiagram[{
    sadApos[a], sadBpos[b],
    toComSugarArrow[1 => 2, f]
  }, opts]
]

sadApos = Case[
  r_Rule := ($apos = First @ r; r);
  obj_   := $apos -> obj;
]

sadBpos = Case[
  Rule[side:$SidePattern|Above|Below, obj_] := Plus[$apos, {1, -1} * Lookup[$SideToCoords, side]] -> obj;
  r_Rule := r;
  obj_ := % @ Rule[Below, obj];
]
