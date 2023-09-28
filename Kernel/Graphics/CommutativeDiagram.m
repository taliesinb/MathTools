PublicSymbol[CommutativeDiagram]

PublicOption[DebugLabelBounds, AutoSetback, Origin]

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
| src$ \[DirectedEdge] dst$ | an unlabeled arrow between objects with given names |
| src$ \[DirectedEdge] dst$ -> label$ | a labeled arrow |
| {edge$, label$} | a labeled arrow |
| {edge$, label$, type$} | a labeled arrow of type type$ |
| {edge$, label$, type$, opts$$} | specify options |
| %MorphismArrow[$$] | a fully specified arrow |
| %UniqueMorphism[$$], %DoubleMorphism[$$], $$ | any of the family of pre-specified morphisms |
* sources and destinations can be objects or previously-declared morphisms
* names for objects and morphisms can be given as integers (referring to the objects in order they appear), or strings.
* string names are automatically generated from objects and morphisms using %FormToPlainString, which spells out greek, ignored tagged forms, etc.
* multiple identical automatic names have successive integers appended to them.
* morphisms can also be specified using 'lbl$1' \[DirectedEdge] 'lbl$2'.
* the type$ given above include 'Iso', 'Epi', 'Mono', 'MapsTo', 'DoubleArrow', 'Equality', 'Line', but pre-specified morphism heads are clearer.
* the special head %ObjectCoordinates[obj$] can be used to refer to the location of an object or centroid of a set of objects, see usage for more info.
* the special head %MorphismCoordinates[$$] can specify the coordinates along a morphisms path, see usage for more info.
* the spec obj$ can be an integer or string name or list of these.
* %LabelPosition -> 'Outer' evaluates to %LabelPosition -> %AwayFrom[%ObjectCoordinates['Center']].
* %LabelPosition -> 'Inner' evaluates to %LabelPosition -> %Towards[%ObjectCoordinates['Center']].
The following options are supported:
| %Transposed | whether to interpret positions as {x$, y$} (False) or ${y$, x$} (True) |
| %FlipX | whether to flip horizontally |
| %FlipY | whether to flip vertically |
| %GraphicsScale | size of one coordinate unit in pixels |
| %Alignment | alignment of text labels of objects |
| %Setback | additional setback distances of arrows, in pixels |
| %AutoSetback | whether to automatically calculate per-object setbacks |
| %DebugLabelBounds | whether to show red rectangles around object bounds |
| %LabelFontSize | font size to use for morphisms |
| %FontSize | font size to use for objects |
| %TextModifiers | list of modifiers to apply to objects |
| %Origin | position at which the origin of the diagram should go |
| %ColorRules | rules to apply to recolor elements of labels |
"

SetUsage @ "
ObjectCoordinates[$$] can be used as a symbolic coordinate in %CommutativeDiagram.
ObjectCoordinates[i$] represents the coordinate of object i$.
ObjectCoordinates['name$'] refers to the coordinates of the given named object.
ObjectCoordinates[{obj$1, obj$2, $$}] refers to the mean coordinate of several objects.
ObjectCoordinates['Center'] refers to the center point of the bounding box of all objects.
"

SetUsage @ "
MorphismCoordinates[$$] can be used as a symbolic coordinate in %CommutativeDiagram.
MorphismCoordinates[$$] represents the half-way point along the given arrow's path.
MorphismCoordinates[$$, pos$] represents a given point along the arrow's path.
MorphismCoordinates[n$] represents the n$th declared morphism.
MorphismCoordinates['name$'] represents the given named morphism.
MorphismCoordinates['src$' \[DirectedEdge] 'dst$'] represents the first morphism between endpoints.
MorphismCoordinates['src$' \[DirectedEdge] 'dst$' -> n$] represents the n$th morphism between endpoints.
* generally it is not needed, since morphisms can be specified using their names or the spec 'lbl$1' \[DirectedEdge] 'lbl$2'.
* %DoubleMorphism automatically uses %MorphismCoordinates when necessary."

Options[CommutativeDiagram] = JoinOptions[
  Transposed             -> False,
  FlipX                  -> False,
  FlipY                  -> False,
  GraphicsScale          -> 120,
  Alignment              -> Center,
  ArrowPathSetback       -> Automatic,
  Setback                -> Automatic,
  AutoSetback            -> True,
  DebugLabelBounds       -> False,
  LabelFontSize          -> 18,
  FontFamily             :> $MathFont,
  FontSize               -> 20,
  TextModifiers          -> {},
  Origin                 -> {0, 0},
  ColorRules             -> {},
  $morphismArrowOptions
];

declareGraphicsFormatting[cd:CommutativeDiagram[__List, ___Rule] :> cdToBoxes[cd], Graphics];

cdToBoxes[cd_] := ToGraphicsBoxes @ cdToPrimitives[cd];

(**************************************************************************************************)

CommutativeDiagram /: Normal[cd:CommutativeDiagram[__List, ___Rule]] := cdToPrimitives[cd];

(**************************************************************************************************)

Format[cd:CommutativeDiagram[__List, opts___Rule], StandardForm] :=
  ScaleGraphics[
    cdToPrimitives[cd],
    GraphicsScale -> Lookup[{opts}, GraphicsScale, 120],
    ImagePadding -> Lookup[{opts}, ImagePadding, Automatic],
    AdjustFontSize -> False
  ];

(**************************************************************************************************)

cdToPrimitives[CommutativeDiagram[objects_List, morphisms_List, opts___Rule]] :=
  cdToPrimitives @ CommutativeDiagram[Join[objects, morphisms], opts];

$objectNames = {};
$inheritedOptions = Sequence[];

cdToPrimitives[CommutativeDiagram[items_List, opts___Rule]] := Scope[
  UnpackOptionsAs[CommutativeDiagram,
    {FilterOptions[CommutativeDiagram, opts]},
    alignment, arrowPathSetback, setback, labelFontSize, fontSize, fontFamily,
    graphicsScale, autoSetback, debugLabelBounds,
    transposed, flipX, flipY, textModifiers, origin, colorRules
  ];
  $objectNames = {};
  $objectCoordinates = {};
  $objectSizes = UAssociation[];
  $itemSize = Automatic;
  $morphismNames = {};
  $morphismCurves = Association[];
  $inheritedOptions = opts;
  If[colorRules =!= {}, AppendTo[textModifiers, toRecolorRules @ colorRules]];
  $textModifierFn = Composition @@ textModifiers;
  calculateLabelSizes = autoSetback || debugLabelBounds;
  objectPrimitives = parseObject /@ items;
  SetAutomatic[arrowPathSetback, setback];
  SetAutomatic[arrowPathSetback, Rectangular[{10, 0}]];
  initialArrowPathSetback = arrowPathSetback;
  morphismPrimitives = parseMorphism /@ items;
  morphismPrimitives //= ReplaceAll[$morphismCanonicalizationRules];
  $objectCoordinates = MapColumn[# + origin&, 2, $objectCoordinates];
  $coordReplacement = Dispatch @ Append[$objectCoordinates, z_String :> unresolvedCoord[z]];
  $centerCoords := $centerCoords = resolveObjCoords["Center"];
  saveMorphismCoords @ morphismPrimitives;
  primitives = {objectPrimitives, morphismPrimitives};
  primitives = primitives /. $coordinateCanonicalizationRules;
  primitives = recurseGraphicsCoordinates[ReplaceAll[#, $coordReplacement]&, primitives];
  arrowOpts = DeleteOptions[{FilterOptions[MorphismArrow, opts]}, {ArrowPathSetback, LabelFontSize, LabelFontSize}];
  {
    Sequence @@ arrowOpts,
    ArrowPathSetback -> initialArrowPathSetback,
    LabelFontSize -> labelFontSize,
    FontSize -> fontSize,
    FontFamily -> fontFamily,
    primitives
  } /. $primitiveCanonicalizationRules
];

$morphismCanonicalizationRules = {
  MorphismArrow[DirectedEdge[a_, b_], rest___] :>
    MorphismArrow[{a, b}, rest]
};

$primitiveCanonicalizationRules = {
  cd_CommutativeDiagram :> cd,
  Rule[ArrowPathSetback, sb_] :>
    RuleCondition @ Rule[ArrowPathSetback, toScaled[sb]],
  Rule[LabelPosition, s:("Outer"|"Inner")] :>
    RuleCondition @ Rule[LabelPosition, If[s === "Outer", AwayFrom, Towards][$centerCoords]]
};

$coordinateCanonicalizationRules = {
  ObjectCoordinates[o_] :> RuleCondition @ resolveObjCoords[o],
  c_MorphismCoordinates :> RuleCondition @ resolveMorphismCoords[c]
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

CommutativeDiagram::badrecolor = "Bad recoloring rules ``."
toRecolorRules = Case[
  list:{__Rule} := ReplaceAll @ Map[toRecolorRule, list];
  other_        := (Message[CommutativeDiagram::badrecolor, other]; Identity);
];

toRecolorRule = Case[
  a_ -> (form_?StyleFormHeadQ) := (z:a) :> RuleCondition[form[z]];
  a_ -> color:$ColorPattern    := (z:a) :> Style[z, color];
  other_                       := (Message[CommutativeDiagram::badrecolor, other]; Nothing);
];

(**************************************************************************************************)

PublicHead[EqualityMorphism, UniqueMorphism, DoubleMorphism, Morphism, ProMorphism, AdjointMorphism]

$morphismHeadP = Morphism | DoubleMorphism | UniqueMorphism | EqualityMorphism | ProMorphism | AdjointMorphism;

declareGraphicsFormatting @ Map[
  head |-> m_head :> With[{m2 = toMorphism[m]}, If[m2 === m, $Failed, Typeset`MakeBoxes[m2, StandardForm, Graphics]]],
  List @@ $morphismHeadP
];

toMorphism := Case[
  Morphism[args___] := MorphismArrow[args];
  UniqueMorphism[path_, lbl:Except[_Rule]:None, args___]   := MorphismArrow[path, lbl, args, ArrowShaftDashing -> Dashed];
  EqualityMorphism[path_, lbl:Except[_Rule]:None, args___] := MorphismArrow[path, lbl, "Equality", args];
  ProMorphism[path_, lbl:Except[_Rule]:None, args___]      := MorphismArrow[path, lbl, "Proarrow", args];
  AdjointMorphism[path_, args___]                          := MorphismArrow[toHigherPath @ path, None, "Adjoint", args];
  DoubleMorphism[path_, lbl:Except[_Rule]:None, args___]   := MorphismArrow[toHigherPath @ path, lbl, "DoubleArrow", args];
  other_ := other;
];

toHigherPath = Case[
  {a_, b_} := {toHigherPathElem @ a, toHigherPathElem @ b};
  other_   := other;
];

toHigherPathElem = Case[
  name_String /; MemberQ[$objectNames, name] := name;
  {a_, b_}                                   := {a, b};
  Placed[path_, pos:$NumberP]                := MorphismCoordinates[path, pos];
  path_                                      := MorphismCoordinates[path];
];

(**************************************************************************************************)

parseObject = Case[
  obj:({_, _} -> _)   := placeObject[obj];
  _                   := Nothing;
];

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
    autoName = First @ Complement[names, list]
  ];
  autoName
];

(**************************************************************************************************)

$legacyObjP = (_String|Integer);
parseMorphism = Case[
  {_, _} -> _                                        := Nothing;
  Null                                               := Nothing;
  ArrowPathSetback|Setback -> sb_                    := (arrowPathSetback = sb; Nothing);
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
  {DirectedEdge[s_, t_], lbl_:None, type_:Automatic} := autoSetback1 @ flipSymbolicPositions @ MorphismArrow[{s, t}, Switch[lbl, None, {}, _List, lbl, _, {{0.5, Above} -> lbl}], type];

  cd_CommutativeDiagram                              := flipSymbolicPositions @ Append[cd, Unevaluated @ $inheritedOptions];

  other_                                             := autoSetback1 @ flipSymbolicPositions @ other;
]

(**************************************************************************************************)

PublicHead[ObjectCoordinates, MorphismCoordinates]

CommutativeDiagram::badobjlbl = "No object named `` in ObjectCoordinates."
CommutativeDiagram::badobjspec = "`` is not a valid specification for ObjectCoordinates."
resolveObjCoords = Case[
  All        := Mean @ Values @ $objectCoordinates;
  "Center"   := Mean @ CoordinateBoundingBox @ Values @ $objectCoordinates;
  i_Integer  := Part[$objectCoordinates, i, 2];
  lbl_String := Lookup[$objectCoordinates, lbl, Message[CommutativeDiagram::badobjlbl, lbl]; {0, 0}];
  list_List  := Mean[% /@ list];
  spec_      := (Message[CommutativeDiagram::badobjspec, spec]; {0, 0});
]

CommutativeDiagram::badmorphspec = "Cannot resolve morphism coordinates for ``."
resolveMorphismCoords = Case[
  MorphismCoordinates[spec_]                 := % @ MorphismCoordinates[spec, .5];
  MorphismCoordinates[list_List, pos_]       := Map[% @ MorphismCoordinates[#, pos]&, list];
  MorphismCoordinates[curve_, pos_?NumberQ]  := resolveCurvePos[resolveMorphism @ curve, pos];
  other_                                     := (Message[CommutativeDiagram::badmorphspec, other]; {0, 0});
];

CommutativeDiagram::badmorphname = "No morphism with name ``."
CommutativeDiagram::badmorphconn = "No morphism with connection ``."
CommutativeDiagram::badmorphconnind = "No morphism with index `` for connection ``."
CommutativeDiagram::badmorphind = "No morphism with index ``."

resolveMorphism = Case[
  spec_DirectedEdge | spec_UndirectedEdge := %[spec -> 1];
  name_String        := Scope[
    ind = IndexOf[$morphismNames, name];
    If[MissingQ[ind], ReturnFailed[CommutativeDiagram::badmorphname, name]];
    Part[$morphismCurves @ name, 1]
  ];
  i_Integer          := Scope[
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
  other_                   := Message[CommutativeDiagram::badmorphspec, other];
];

(**************************************************************************************************)

resolveCurvePos[$Failed, _] := {{0,0}, {1,0}};

resolveCurvePos[l_List, pos_] := resolveCurvePos[Line @ l, pos];

resolveCurvePos[MorphismArrow[curve_, ___], pos_] :=
  resolveCurvePos[curve, pos];

CommutativeDiagram::morphcoordsfail = "Cannot resolve position along curve ``."
resolveCurvePos[curve_, pos_] := Scope[
  curve = recurseGraphicsCoordinates[ReplaceAll[#, $coordReplacement]&, curve];
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
  If[calculateLabelSizes, $objectSizes[lbl] = ReplaceAutomatic[$itemSize, {1, 1}]];
  Nothing
);

fmtLabel[lbl_, obj_] := Scope[
  text = Text[$textModifierFn @ obj, lbl, Lookup[$SideToCoords, alignment]];
  If[calculateLabelSizes,
    size = $itemSize;
    If[ContainsQ[size, Automatic],
      text2 = Append[text, BaseStyle -> {FontSize -> fontSize, FontFamily -> fontFamily}];
      isize = N[TextRasterSize @ text2];
      If[size === Automatic, size = {Automatic, Automatic}];
      size = MapThread[ReplaceAutomatic, {size, isize}];
    ];
    $objectSizes[lbl] ^= size;
    If[debugLabelBounds,
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
  MorphismArrow[path_, rest___] := Scope[
    If[ContainsQ[path, MorphismCoordinates], Return[]];
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

autoSetback1 = Case[
  m:($morphismHeadP[__])                  := % @ toMorphism @ m;
  MorphismArrow[{i_Integer, j_}, args___] := % @ MorphismArrow[{Part[$objectNames, i], j}, args];
  MorphismArrow[{i_, j_Integer}, args___] := % @ MorphismArrow[{i, Part[$objectNames, j]}, args];
  other_                                  := autoSetback2 @ other
];

autoSetback2 = Case[
  ma:MorphismArrow[___, ArrowPathSetback|Setback -> _, ___] := ma;
  ma_MorphismArrow /; TrueQ[autoSetback] := Scope[
    path = First @ ma;
    st = findSourceTarget @ path;
    If[st === None, Return @ ma];
    {src, tgt} = st;
    {sz1, sz2} = lookupObjectSize /@ {src, tgt};
    setback = Map[makeEndpointSetback, {sz1, sz2}];
    setback = ExtendSetback[setback, arrowPathSetback];
    Append[ma, ArrowPathSetback -> setback]
  ];
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
  DirectedEdge[a_, b_]                := % @ {a, b};
  {a_, b_}                            := findST /@ {a, b};
  {a_, __, b_}                        := % @ {a, b};
  ElbowCurve[{a_, b_}, ___]           := % @ {a, b};
  CircuitCurve[path_, ___]            := % @ path;
  HorizontalCurve[a_, ___]            := % @ {a, None};
  VerticalCurve[a_, ___]              := % @ {a, None};
  MorphismCoordinates[list_List, ___] := None;
  _AnchoredCurve                      := None;
  other_                              := (Message[CommutativeDiagram::resolvesrctgt, other]; None);
];

findST = Case[
  _                        := None;
  v_ ? CoordinateVector2DQ := v;
  name_String              := name;
  i_Integer                := Part[$objectNames, i];
]

(**************************************************************************************************)

PublicFunction[CommutativeSquare]

CommutativeSquare[{nw_, ne_, se_, sw_}, {n_, e_, s_, w_}, opts___Rule] :=
  CommutativeDiagram[
    {{1, 1} -> "NW" -> nw, {2, 1} -> "NE" -> ne, {1, 2} -> "SW" -> sw, {2, 2} -> "SE" -> se},
    {MorphismArrow[{"NW", "NE"}, n, LabelPosition -> Above],
     MorphismArrow[{"NE", "SE"}, e, LabelPosition -> Right],
     MorphismArrow[{"NW", "SW"}, w, LabelPosition -> Left],
     MorphismArrow[{"SW", "SE"}, s, LabelPosition -> Bottom]},
    opts
  ];
