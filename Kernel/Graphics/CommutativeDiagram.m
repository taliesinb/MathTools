PublicTypesettingForm[CommutativeDiagram]

PublicOption[DebugBounds, AutoSetback, Origin, SymbolReplacements, CloneOptions, CloningFunction, DiagramScaling, DefaultMorphism]

PublicSymbol[Outwards, Inwards]

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
* sources and destinations can be objects or previously-declared morphisms.
* names for objects and morphisms can be given as integers (referring to the objects in order they appear), or strings.
* string names are automatically generated from objects and morphisms using %FormToPlainString, which spells out greek, ignored tagged forms, etc.
* multiple identical automatic names have successive integers appended to them.
* morphisms can also be specified using 'lbl$1' => 'lbl$2'.
* the type$ given above include 'Iso', 'Epi', 'Mono', 'MapsTo', 'DoubleArrow', 'Equality', 'Line', but pre-specified morphism heads are clearer.
* %ObjectCoordinates[$$] and %MorphismCoordinates[$$] can be used to refer symbolically to locations or centroids of objects and morphisms, see their usages.
* %LabelPosition -> %Outwards evaluates to %LabelPosition -> %AwayFrom[%ObjectCoordinates['Center']].
* %LabelPosition -> %Inwards evaluates to %LabelPosition -> %Towards[%ObjectCoordinates['Center']].
* the special object %Cloned[obj$, elem$] will generate a clone of the object displaying as elem$ and a morphism to the clone.
* %Cloned[obj$, elem$, label$] will attach label$ to morphism that connects the object to its clone.
* cloning can be customized as described by %CloneOptions.
The following options are supported:
| %Transposed | whether to interpret positions as {x$, y$} (False) or {y$, x$} (True) |
| %FlipX | whether to flip horizontally |
| %FlipY | whether to flip vertically |
| %DiagramScaling | scale factors to apply to x$ and y$ |
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
| %SymbolReplacements | rules to rewrite contents of objects and morphisms prior to display |
| %CloneOptions | how to display %Cloned objects |
| %CloningFunction | function to produce %Cloned objects and morphisms |
| %DefaultMorphism | the default morphism to use |
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

SetUsage @ "
SymbolReplacements is an option for CommutativeDiagram and consists of a list of rules.
* each rule will be applied to objects and morphisms before display.
* %ColorRules are applied after replacement.
* however, if colors in %ColorRules would apply to the original entity but not the replacement, the color is carried over.
* the special value 'DiskArrow' will replace all %CategoryObjectSymbols with disks and %CategoryArrowSymbols with arrows.
"

SetUsage @ "
CloningFunction is an option for CommutativeDiagram, and takes the following specifications:
| fn$ | apply fn$ to each object to obtain a cloned object %Cloned[obj$, fn$[obj$]] |
| {fn$o, fn$m} | apply fn$o to objects and fn$m to morphisms |
* if fn$ returns None, the object is not cloned.
"

Options[CommutativeDiagram] = JoinOptions[
  Transposed             -> False,
  FlipX                  -> False,
  FlipY                  -> False,
  DiagramScaling         -> {1, 1},
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
  ColorRules             -> {},
  SymbolReplacements     -> None,
  CloneOptions           -> {},
  CloningFunction        -> None,
  DefaultMorphism        -> MorphismArrow,
  $morphismArrowOptions
];

DeclareGraphicsPrimitive[CommutativeDiagram, "Rules", cdToBoxes];

cdToBoxes[cd_] := ToGraphicsBoxes @ cdToPrimitives @ cd;

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
    transposed, flipX, flipY, diagramScaling, origin,
    textModifiers, colorRules, symbolReplacements,
    cloneOptions, cloningFunction,
    $defaultMorphism
  ];
  $objectNames = {};

  $objectCoordsList = {}; (* does not contain derived coordinates ($towardsCenter) *)
  $objects = $objectCoords = $objectSizes = $morphGradColors = UAssociation[];

  $itemSize = Automatic;
  $morphismNames = {};
  $morphismCurves = $cloneChildren = Association[];
  $inheritedOptions = opts;

  $saveMorphGradColors = False;
  colorModifierFn = toRecoloringFunction @ colorRules;
  replacementFn = toReplacementFunction[symbolReplacements, colorModifierFn];
  {$objectTextModifierFn, $morphismTextModifierFn} = Map[
    (* we apply replacements first because multiple diagrams can come from one baseline... see note in toReplacementFunction *)
    Composition[toModifierFunction[#], replacementFn, colorModifierFn]&,
    If[AssociationQ[textModifiers],
      Lookup[textModifiers, {"Objects", "Morphisms"}, {}],
      {textModifiers, textModifiers}
    ]
  ];

  $calculateLabelSizes = $autoSetback || $debugBounds;
  SetAutomatic[$setback, Rectangular[{15, 5}]];
  initialSetback = $setback;

  $cloneAbsoluteSetback = $cloneObjectLinkSetback = $cloneMorphismLinkSetback = $cloneDisplaceFn = $cloneInteriorLinkFn = $cloneObjectLinkFn = $cloneMorphismLinkFn = Null;
  $cloneInteriorLinks = $cloneExteriorLinks = {};
  $objFn = $arrFn = Identity;
  {interiorLinkOptions, exteriorLinkOptions} = processCloneSpecs[cloneOptions, cloningFunction];

  $currentFontSize = fontSize; (* <- for recoloring rule to produce the right GradientSymbol *)
  objectPrimitives = parseObject /@ items;

  $toHigherPath = toHigherPath;
  $clonesExist = Length[$cloneChildren] > 0;
  $currentFontSize = labelFontSize; (* <- for recoloring rule *)
  If[$saveMorphGradColors, saveMorphGradColors @ items];

  morphismPrimitives = parseMorphism /@ items;
  If[$clonesExist, morphismPrimitives = {
      {morphismPrimitives, Flatten @ {interiorLinkOptions, $cloneInteriorLinks}},
      Flatten @ {exteriorLinkOptions, $cloneExteriorLinks}
  }];

  $center := $center = Mean @ CoordinateBoundingBox @ $objectCoordsList;
  $median := $median = Mean @ $objectCoordsList;
  $biasedCenter := $biasedCenter = Lerp[Mean @ $objectCoordsList, $center, .9];

  (* we save morphism coords first so that resolveCoordinates can refer to them to calculate 2-morphism coordinates *)
  saveMorphismCoords @ morphismPrimitives;
  primitives = resolveCoordinates @ {objectPrimitives, morphismPrimitives};

  (* we apply resolveCoordinates again because Cloned produces a coordinate containing ObjectCoordinates[..] *)
  $coordReplacement = Append[resolveCoordinates @ Normal @ $objectCoords, z_String :> unresolvedCoord[z]];
  primitives = ReplacePrimitiveCoordinates[primitives, $coordReplacement];

  arrowOpts = DeleteOptions[
    {FilterOptions[MorphismArrow, opts]},
    {Setback, LabelFontSize, LabelFontSize, GraphicsScale}];
  result = {
    Sequence @@ arrowOpts,
    BendRadius -> 0.25,
    GraphicsScale -> graphicsScale,
    Setback -> initialSetback,
    LabelFontSize -> labelFontSize,
    FontSize -> fontSize,
    FontFamily -> fontFamily,
    primitives
  } /. $primitiveCanonicalizationRules;

  If[origin != {0, 0}, TranslatePrimitives[result, origin], result]
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

$primitiveCanonicalizationRules = {
  cd_CommutativeDiagram :> cd,
  Rule[Setback, sb_] :>
    RuleCondition @ Rule[Setback, toScaled[sb]],
  Outwards :> AwayFrom[$biasedCenter],
  Inwards  :> Towards[$biasedCenter]
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
  {} | None            := Identity;
  el:(_Rule | _String) := % @ {el};
  list_List            := ReplaceAll @ Map[toRecolorRule, list];
  other_               := (Message[CommutativeDiagram::badrecolor, other]; Identity);
];

$colorP = _Integer | (_Symbol ? StyleFormHeadQ) | $ColorPattern;

toCol = Case[
  s_Symbol := StyleFormData @ s;
  other_ := other;
]

toRecolorRule = Case[

  "Rainbow" := Splice[% /@ {"RainbowObjects", "RainbowArrows"}];

  "RainbowObjects" :=
    RuleDelayed[z_CategoryObjectSymbol, RuleCondition @ Style[z, ToRainbowColor @ ToRainbowInteger @ z]];

  "RainbowArrows" := (
    $saveMorphGradColors = True;
    RuleDelayed[z_CategoryArrowSymbol, RuleCondition @ GradientSymbol[z, $gradColor[z, 1], $gradColor[z, 2], $currentFontSize]]
  );

  f_ -> {a:$colorP, b:$colorP} := With[{c1 = toCol @ a, c2 = toCol @ b},
    RuleDelayed[f, RuleCondition @ GradientSymbol[f, c1, c2, $currentFontSize]]
  ];

  a_ -> c:$colorP := With[{c1 = toCol @ c},
    RuleDelayed[(z:a), Style[z, c1]]
  ];

  other_Rule := (
    Message[CommutativeDiagram::badrecolor, other];
    Nothing
  );

  other_ := (
    Message[CommutativeDiagram::badrecolor, other];
    Nothing
  );
];

(**************************************************************************************************)

(* if a replacement would prevent a color modifier from applying, we pre-apply it and then do the
replacement, so that we can replace $Af | $Ag to \[RightArrow] but *also* have their colors applied *)

CommutativeDiagram::badrepspec = "SymbolReplacements -> `` is invalid.";

toReplacementFunction[None | {}, _] := Identity;

(* replace these with ObjectToken and MorphismToken *)
$diskArrowRules = {
  _CategoryObjectSymbol -> "\[FilledCircle]",
  _CategoryArrowSymbol -> "\[RightArrow]"
};

toReplacementFunction["DiskArrow", cf_] :=
  toReplacementFunction[$diskArrowRules, cf]

toReplacementFunction[rules_, colorFn_] := Scope[
  ReplaceAll[parseRep /@ ToList[rules]]
,
  parseRep[rule:(lhs_ -> rhs_)] :=
    If[colorFn[lhs] =!= lhs && colorFn[rhs] === rhs,
      lhs -> ReplaceAll[colorFn[lhs], rule],
      rule
    ],
  parseRep[spec_] := (
    Message[CommutativeDiagram::badrepspec, spec];
    Nothing
  )
];

(**************************************************************************************************)

PublicGraphicsPrimitive[EqualityMorphism, MapsToMorphism, UniqueMorphism, DoubleMorphism, Morphism, ProMorphism, AdjointMorphism, LongAdjointMorphism, LineMorphism]

$morphismHeadP = Morphism | DoubleMorphism | UniqueMorphism | EqualityMorphism | MapsToMorphism | ProMorphism | AdjointMorphism | LongAdjointMorphism | LineMorphism;

Scan[DeclareCurveAlias[#, toMorphism]&, $morphismHeadP];

(*
Scan[DeclareGraphicsPrimitive[#, "Curve", fixedMorphismBoxes]&, $morphismHeadP]; *)

fixedMorphismBoxes[m_] := With[
  {m2 = toMorphism[m]},
  If[m2 === m, $Failed, ToGraphicsBoxes @ m2]
];

$toHigherPath = Identity;
$defaultMorphism = MorphismArrow;

toMorphism := Case[
  Morphism[args___]                                        := $defaultMorphism[args];
  UniqueMorphism[path_, lbl:Except[_Rule]:None, args___]   := MorphismArrow[path, lbl, args, ArrowDashing -> Dashed];
  EqualityMorphism[path_, lbl:Except[_Rule]:None, args___] := MorphismArrow[path, lbl, "Equality", args];
  MapsToMorphism[path_, lbl:Except[_Rule]:None, args___]   := MorphismArrow[path, lbl, "MapsTo", args];
  ProMorphism[path_, lbl:Except[_Rule]:None, args___]      := MorphismArrow[path, lbl, "Proarrow", args];
  LineMorphism[path_, lbl:Except[_Rule]:None, args___Rule] := MorphismArrow[path, lbl, "Line", LabelPosition -> Center, LabelOrientation -> Aligned, ArrowColor -> $LightGray, args];
  LineMorphism[path_, lbl_, n:(_Integer | {_Integer, _Integer}), args___Rule] := % @ LineMorphism[path, lbl, LabelOffset -> AlignedOffset[If[IntegerQ[n], {0, n}, n]], args];
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

makeAutoName[Cloned[expr_, _], list_] := makeAutoName[expr, list];

makeAutoName[expr_, list_] := Scope[
  autoName = FormToPlainString @ expr;
  If[MemberQ[list, autoName],
    names = autoName <> #& /@ {"2", "3", "4", "5", "6", "8", "9", "10", "11", "12"};
    autoName = SelectFirst[names, !MemberQ[list, #]&];
  ];
  autoName
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

PublicHead[Sized, Cloned]

placeObject = Case[

  Rule[pos_, obj_] :=
    placeLabeledObj[pos, makeAutoName[obj, $objectNames], $objFn @ obj];

  Rule[pos_, Rule[lbl_, obj_]] :=
    placeLabeledObj[pos, lbl, $objFn @ obj];

];

placeLabeledObj[pos_, lbl_, obj_] := Scope[
  pos //= toCoord;
  AssociateTo[$objectCoords, lbl -> pos];
  AssociateTo[$objects, lbl -> obj];
  AppendTo[$objectCoordsList, pos];
  AppendTo[$objectNames, lbl];
  fmtLabel[lbl, obj]
];

toCoord[pos_] := {1,-1} * doFlip[pos] * diagramScaling;

(**************************************************************************************************)

placeLabeledObj[pos_, lbl_, Cloned[a_, b_, c_:None]] := Scope[
  cloneLbl = lbl <> "$";
  (* we don't add a name because we don't want to change the numbered object order *)
  AssociateTo[$cloneChildren, lbl -> cloneLbl];
  AssociateTo[$objects, cloneLbl -> b];
  (* this will get resolved later by resolveCoordinates, by which time we will know 'Center' *)
  AssociateTo[$objectCoords, cloneLbl -> $cloneDisplaceFn[lbl]];
  res = {placeLabeledObj[pos, lbl, a], fmtLabel[cloneLbl, b]};
  objLink = processClonedObjectLink @ $cloneObjectLinkFn[{lbl, cloneLbl}, c];
  AppendTo[$cloneExteriorLinks, objLink];
  Splice @ res
];


SetUsage @ "
CloneOptions is an option to %CommutativeDiagram that specifies how %Cloned objects should be handled.
It should be set to a list of rules with the following keys:
| 'Displacement' | how to derive coordinates of cloned object from its original |
| 'InteriorLink' | how to connect object clones to other object clones |
| 'ExteriorLink' | how to connect objects or morphisms to their clones |
| 'ObjectLink' | how to connect objects to their clones |
| 'MorphismLink' | how to connect morphisms to their clones |
| 'ObjectLinkSetback' | how to setback the morphisms between objects and their clones |
| 'MorphismLinkSetback' | how to setback the morphisms between morphisms and their clones |
| 'AbsoluteSetback' | global setback for both of the above that ignores object bounding boxes |
| 'InteriorLinkOptions' | additional options to apply to interior morphisms |
| 'ExteriorLinkOptions' | additional options to apply to exterior morphisms |
* 'ObjectLink' and 'MorphismLink' go from the cloned element to its clone.
* 'ExteriorLink' provides a default for both 'ObjectLink' and 'MorphismLink'.
* the 'Link' specifications can be any of the following:
| 'type$' | a named %MorphismArrow type |
| %Reversed[spec$] | reversed form of spec$ |
| %Sized[spec$, size$] | change the %ArrowheadSize of spec$ |
| morphism$[opts$$] | use a bespoke morphims like %DoubleMorphism with specific options |
| None | do not create this kind of link |
| fn$ | fn$[pos$, lbl$] to create the link |
* 'InteriorLink' -> Inherited uses the same morphism that is being cloned.
* additionally, the list of rules can also contain the named presets 'Element', 'Functor', 'FullFunctor'.
* 'Displacement' can be one of the following:
| coord$ | global offset |
| {side$, d$} | a distance d$ in direction side$ |
* side$ can %%Inwards, %%Outwards, Top, Bottom, etc.
"

Options[CloneOptions] = {
  "Displacement" -> Automatic,
  "InteriorLink" -> Inherited,
  "ExteriorLink" -> None,
  "ObjectLink" -> None,
  "MorphismLink" -> None,
  "ObjectLinkSetback" -> Automatic,
  "MorphismLinkSetback" -> Automatic,
  "AbsoluteSetback" -> None,
  "InteriorLinkOptions" -> {},
  "ExteriorLinkOptions" -> {}
};

$namedCloneOpts = <|
  "Element" -> {"InteriorLink" -> "MapsTo", "ObjectLink" -> Reversed["Element"]},
  "Functor" -> {"ObjectLink" -> "MapsTo"},
  "FullFunctor" -> {"ExteriorLink" -> "MapsTo"}
|>;

namedCloneOpts[s_] := LookupOrMessageKeys[$namedCloneOpts, s, {}];

processCloneSpecs[opts_, cloneFn_] := (
  Switch[cloneFn,
    None,   Null,
    {_, _}, {$objFn, $arrFn} = cloneFnWrapper /@ cloneFn,
    _,      $objFn = cloneFnWrapper @ cloneFn
  ];
  processCloneOptions[opts]
);

_cloneFnWrapper[None] := None;
cloneFnWrapper[fn_][e_] := With[{r = fn[e]}, If[r === None, e, Cloned[e, r]]];

processCloneOptions = Case[

  None|Automatic := % @ {};

  s_String := % @ namedCloneOpts[s];

  {s_String, opts___Rule} := % @ Join[{opts}, namedCloneOpts @ s];

  opts:{___Rule} := Scope[
    UnpackOptionsAs[CloneOptions, opts,
      displacement, interiorLink, exteriorLink, objectLink, morphismLink,
      objectLinkSetback, morphismLinkSetback, absoluteSetback,
      interiorLinkOptions, exteriorLinkOptions
    ];
    (* absoluteSetback overides morphismLinkSetback if it is provided *)
    SetAutomatic[morphismLinkSetback, ReplaceNone[absoluteSetback, 15]];
    SetAutomatic[objectLinkSetback, initialSetback];
    SetNone[objectLink, exteriorLink];
    SetNone[morphismLink, exteriorLink];
    $cloneAbsoluteSetback     ^= absoluteSetback;
    $cloneObjectLinkSetback   ^= objectLinkSetback;
    $cloneMorphismLinkSetback ^= morphismLinkSetback;
    $cloneDisplaceFn          ^= toCloneDisplaceFn @ displacement;
    $cloneInteriorLinkFn      ^= toCloneMorphismFn @ interiorLink;
    $cloneObjectLinkFn        ^= toCloneMorphismFn @ objectLink;
    $cloneMorphismLinkFn      ^= toCloneMorphismFn @ morphismLink;
    Flatten @ ToList @ #& /@ {interiorLinkOptions, exteriorLinkOptions}
  ];

  other_ := (
    Message[CommutativeDiagram::badclonespec, MsgExpr @ other];
    % @ {}
  )
];

toCloneMorphismFn = Case[
  Inherited                       := Inherited;
  None                            := Nothing&;
  s_String | _Rule                := MorphismArrow[#1, #2, s]&;
  Sized[e_, s_]                   := ReplaceAll[% @ e, (h:$morphismHeadP|MorphismArrow)[args___] :> h[args, ArrowheadSize -> s]];
  Reversed[s_]                    := With[{fn = % @ s}, fn[Reverse @ #1, #2]&];
  (h:$morphismHeadP)[opts___Rule] := h[#1, #2, opts]&;
  fn_ ? MightEvaluateWhenAppliedQ := fn;
  spec_                           := (
    Message[CommutativeDiagram::badclonemorphfn, spec];
    Nothing&
  )
];

toCloneDisplaceFn = Case[
  Automatic                    := % @ {Inwards, 0.4};
  {Inwards, d:$NumberP}        := ObjectCoordinates[{#, "Median"}, PointAlongLine[d]]&;
  {Outwards, d:$NumberP}       := %[{Inwards, -d}];
  {s:$SidePattern, d:$NumberP} := ObjectCoordinates[#, PlusVector[d * Lookup[$SideToCoords, s]]]&;
  d:$CoordP                    := ObjectCoordinates[#, PlusVector[toCoord @ d]]&;
  spec_                        := (Message[CommutativeDiagram::badcloneopt, "Displacement" -> spec]; % @ {0, 1})
];

CommutativeDiagram::badcloneopt = "Bad setting `` in CloneOptions."
CommutativeDiagram::badclonemorphfn = "`` is not a valid setting for a morphism function in CloneOptions.";
CommutativeDiagram::badclonespec = "CloneOptions -> `` is invalid."

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

resolveCoordinates[e_] := ReplaceRepeated[e, $coordinateCanonicalizationRules];

(**************************************************************************************************)

PublicHead[ObjectCoordinates, MorphismCoordinates]

CommutativeDiagram::badobjlbl = "No object named `` in ObjectCoordinates."
CommutativeDiagram::badobjspec = "`` is not a valid specification for ObjectCoordinates."
CommutativeDiagram::objcspeclen = "Object number `` doesn't exist (`` available)."

resolveObjectCoords = Case[
  ObjectCoordinates[All]            := $objectCoordsList;
  ObjectCoordinates["Center"]       := $center;
  ObjectCoordinates["Median"]       := $median;
  ObjectCoordinates["BiasedCenter"] := $biasedCenter;
  ObjectCoordinates[i_Integer]      := Lookup[$objectCoords, getObjectName @ i, Message[CommutativeDiagram::objcspeclen, i, Length @ $objectNames]; {0, 0}];
  ObjectCoordinates[lbl_String]     := Lookup[$objectCoords, lbl, Message[CommutativeDiagram::badobjlbl, lbl]; {0, 0}];
  ObjectCoordinates[list_List]      := Map[% @ ObjectCoordinates[#]&, list];
  ObjectCoordinates[spec_, fn_]     := fn @ % @ ObjectCoordinates[spec];
  spec_      := (Message[CommutativeDiagram::badobjspec, spec]; {0, 0});
]

CommutativeDiagram::badmorphspec = "Cannot resolve morphism coordinates for ``."
resolveMorphismCoords = Case[
  MorphismCoordinates[spec_]                 := % @ MorphismCoordinates[spec, .5];
  MorphismCoordinates[list_List, pos_]       := Map[% @ MorphismCoordinates[#, pos]&, list];
  MorphismCoordinates[curve_, pos_?NumberQ]  := resolveCurvePos[resolveMorphism @ curve, pos];
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
  curve = ReplacePrimitiveCoordinates[curve, Normal @ $objectCoords];
  curve //= resolveCoordinates;
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

  e_List := Scan[%, e];

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

CommutativeDiagram::badobjindex = "No object with index ``.";

getObjectName[i_] := If[
  1 <= Abs[i] <= Length[$objectNames],
  Part[$objectNames, i],
  Message[CommutativeDiagram::badobjindex, i]; {0, 0}
];

(**************************************************************************************************)

$edgeyHead = (List | Rule | DirectedEdge);

(* resolve alternate heads and lazy specs to become MorphismArrow *)
processMorphism1 = Case[
  m:($morphismHeadP[__])                              := % @ toMorphism @ m;
  MorphismArrow[$edgeyHead[i_Integer, j_], args___]   := % @ MorphismArrow[{getObjectName @ i, j}, args];
  MorphismArrow[$edgeyHead[i_, j_Integer], args___]   := % @ MorphismArrow[{i, getObjectName @ j}, args];
  MorphismArrow[(Rule|DirectedEdge)[i_, j_], args___] := % @ MorphismArrow[{i, j}, args];
  other_                                              := processMorphism2 @ flipSymbolicPositions @ other;
];

(**************************************************************************************************)

(* attach automatic setbacks and text modifiers *)
processMorphism2 = Case[

  (* TODO: setback intefereces with cloning *)
  ma_MorphismArrow /; TrueQ[$autoSetback] && FreeQ[ma, Setback] := Scope[
    st = findSourceTarget @ First @ ma;
    If[st === None, Return @ ma];
    {src, tgt} = st;
    {sz1, sz2} = lookupObjectSize /@ {src, tgt};
    setback = Map[makeEndpointSetback, {sz1, sz2}];
    setback = ExtendSetback[setback, $setback];
    % @ Append[ma, Setback -> setback]
  ];

  ma_MorphismArrow /; TrueQ[$morphismTextModifierFn =!= Identity] && FreeQ[ma, TextModifiers] := Scope[
    modifiers = $morphismTextModifierFn /. HoldPattern[$currentFontSize] :> RuleCondition[$currentFontSize];
    modifiers //= resolveGradColors;
    % @ Append[ma, TextModifiers -> modifiers]
  ];

  other_ := processMorphism3 @ other;
];

resolveGradColors[ma_] := With[
  {gradColors = $morphGradColors},
  ma /. $gradColor[m_, i_] :> Part[gradColors[m], i]
];

saveMorphGradColors = Case[

  e_List := Scan[%, e];

  m:(MorphismArrow|$morphismHeadP)[path_, lbl_, ___] := Scope[
    st = Lookup[$objects, findSourceTarget @ path];
    If[ContainsQ[st, Missing], Return[]];
    stColored = $objectTextModifierFn /@ st;
    colors = DeepFirstCase[#, Style[___, c:$ColorPattern, ___] :> c]& /@ stColored;
    If[Length[colors] == 2, AssociateTo[$morphGradColors, Replace[lbl, Cloned[c_, _] :> c] -> colors]];
  ];

  _ := Null
];

makeEndpointSetback = Case[
  {a_, b_} := Rectangular[{a, b}];
  a_       := a;
];

lookupObjectSize = Case[
  s_String := Lookup[$objectSizes, s, {15, 15}];
  _        := {15, 15};
];

(**************************************************************************************************)

(* produce cloned morphisms if any *)
processMorphism3 = Case[

  ma_MorphismArrow /; TrueQ[$arrFn =!= Identity] :=
    processMorphismWithCloning @ MapAt[$arrFn, ma, 2];

  ma_MorphismArrow /; TrueQ[$clonesExist] :=
    processMorphismWithCloning @ ma;

  other_ := other;
];

processMorphismWithCloning[ma_MorphismArrow] := Scope[

  st = findSourceTarget @ First @ ma;
  If[!StringVectorQ[clonedSt = Lookup[$cloneChildren, st]], Return @ ma];

  ma2 = ma;
  label = SafePart[ma, 2];
  If[MatchQ[label, Cloned[_, _]],
    {Part[ma2, 2], clonedLabel} = List @@ label,
    clonedLabel = None
  ];

  (* link the corresponding cloned objects to eachother *)
  cloneLink = processClonedMorphism @ If[$cloneInteriorLinkFn === Inherited,
    DeleteCases[ReplacePart[ma, {1 -> clonedSt, 2 -> clonedLabel}], Setback -> {__Rectangular}],
    $cloneInteriorLinkFn[clonedSt, clonedLabel]
  ];
  If[cloneLink === Nothing, Return @ ma2];
  AppendTo[$cloneInteriorLinks, cloneLink];

  (* link the cloned morphism to the original morphism *)
  cloneLinkPath = MorphismCoordinates[DirectedEdge @@@ {st, clonedSt}];
  morphismLink = processClonedMorphismLink @ $cloneMorphismLinkFn[cloneLinkPath, None];
  If[morphismLink === Nothing, Return @ ma2];
  AppendTo[$cloneExteriorLinks, morphismLink];

  ma2
];

processClonedMorphismLink[e_] :=
  replaceSetback[processClonedMorphism @ e, $cloneMorphismLinkSetback];

processClonedObjectLink[e_] := Block[
  {$setback = $cloneObjectLinkSetback},
  replaceSetback[processClonedMorphism @ e, $cloneAbsoluteSetback]
];

processClonedMorphism[e_] := Block[
  {$arrFn = Identity},
  processMorphism1 @ e
];

replaceSetback[Nothing, _] := Nothing;
replaceSetback[ma_, None] := ma;
replaceSetback[ma_, sb_] := ReplaceOptions[ma, Setback -> sb];

(**************************************************************************************************)

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
    LabelPosition -> Outwards
  ];

(**************************************************************************************************)

PublicFunction[InTriangleDiagram, OutTriangleDiagram, CompositionTriangleDiagram]

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

SetUsage @ "
CompositionTriangleDiagram[{l$, t$, r$}, {lt$, tr$, lr$}] constructs a %CommutativeDiagram with 3 objects and 3 morphisms connecting them.
* the morphisms can be given as label$ or {label$, type$, opts$$}.
* additional morphisms can be included after the three.
"

$triCoord = With[{r = 1.1547}, {Left -> {0, r}, Center -> {r/2, r}, Right -> {r, r}}];

InTriangleDiagram[{l_, r_, b_, obs___}, {lr_, lb_, rb_, mors___}, side:(Left|Center|Right):Center, opts___Rule] :=
  commutativeTriangle[{l, r, b, obs}, {lr, lb, rb, mors}, False, side, opts];

OutTriangleDiagram[{t_, l_, r_, obs___}, {tl_, tr_, lr_, mors___}, side:(Left|Center|Right):Center, opts___Rule] :=
  commutativeTriangle[{l, r, t, obs}, {lr, tl, tr, mors}, True, side, opts];

CompositionTriangleDiagram[{l_, t_, r_, obs___}, {l2t_, t2r_, l2r_, mors___}, side:(Top|Bottom):Top, opts___Rule] :=
  commutativeTriangle[{l, r, t, obs}, {l2r, Reversed @ l2t, t2r, mors}, side === Top, Center, opts];

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
    LabelPosition -> Outwards,
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
    LabelPosition -> Outwards,
    opts
  ]
];

toComSugarArrow[edge_, Null] := Nothing;
toComSugarArrow[edge_, (head:$morphismHeadP)[args___]] := head[edge, args];
toComSugarArrow[edge_, label_] := Morphism @@ ToList[edge, label];
toComSugarArrow[edge_, Reversed[label_]] := toComSugarArrow[Reverse @ edge, label];
toComSugarArrow[edge_, Reversed[Reversed[label_]]] := %[edge, label];

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
  }, LabelPosition -> Outwards, FilterOptions @ opts]
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
    LabelPosition -> Outwards, FilterOptions @ opts
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

(**************************************************************************************************)

PublicFunction[CommutativePentagon, CommutativeHexagon]

$pentagonPoints := $pentagonPoints = VectorReflectVertical @ ClockwiseCirclePoints[5];

CommutativePentagon[objs:{_, _, _, _, _}, arrows:{_, _, _, _, _}, opts___Rule] :=
  commutativeNgon[5, objs, arrows, opts];

CommutativeHexagon[objs:{_, _, _, _, _, _}, arrows:{_, _, _, _, _, _}, opts___Rule] :=
  commutativeNgon[6, objs, arrows, opts];

(**************************************************************************************************)

ngonPoints[n_] := ngonPoints[n] = VectorReflectVertical @ ClockwiseCirclePoints @ n;

commutativeNgon[n_, objects_, arrows_, opts___Rule] := CommutativeDiagram[
  ngonPoints[n] -> objects,
  MapIndex1[toComSugarArrow[#2 => Mod[#2 + 1, n, 1], #1]&, arrows],
  FontSize -> 14, LabelFontSize -> 15, GraphicsScale -> 150,
  LabelPosition -> Outwards, Setback -> 10, opts
]

