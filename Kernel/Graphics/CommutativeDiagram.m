PublicTypesettingForm[CommutativeDiagram]

PublicOption[DebugBounds, AutoSetback, Origin, SymbolReplacements, CloneOptions, CloningFunction, DiagramScaling, DefaultMorphism, MorphismColors, DiagramColorRules, GradientSymbolOptions]

PublicSymbol[Outwards, Inwards]

SetUsage @ "
CommutativeDiagram[items$] evaluates to a %FixedGraphics object that depicts a commutative diagram.
CommutativeDiagram[objects$, morphisms$] is a legacy form that is also supported.
The list items$ can consist of objects, morphisms, and arbitrary graphics primitives in any order, described below:

## Objects
* an object can be one of:
| pos$ -> obj$ | object obj$ at position pos$, automatically named |
| pos$ -> 'name$' -> obj$ | provide a string name |
* obj$ can be wrapped in %Sized[$$, {w$, h$}] or %Sized[$$, diam$] which indicates overrides the automatically calculated size for purposes of arrow setback.
* pos$ is a coordinate pair {x$, y$} running down the page and to the right.

## Morphisms
* a morphism can be one of:
| src$ => dst$ | an unlabeled arrow between objects with given names |
| src$ => dst$ -> label$ | a labeled arrow |
| {edge$, label$} | a labeled arrow |
| {edge$, label$, type$, $$} | a labeled arrow of type type$, and any additional options |
| %MorphismArrow[$$] | a fully specified arrow |
* %Morphism[$$], %UniqueMorphism[$$], %DoubleMorphism[$$], etc. that prespecify morphism type can be used for readability.
* the type$ given above include 'Iso', 'Epi', 'Mono', 'MapsTo', 'DoubleArrow', 'Equality', 'Line', but pre-specified morphism heads are clearer.
* morphisms can also be specified using 'lbl$1' => 'lbl$2'.

## Names
* sources and destinations can be objects or previously-declared morphisms.
* names for objects and morphisms can be given as integers (referring to the objects in order they appear), or strings.
* string names are automatically generated from objects and morphisms using %FormToPlainString, which spells out greek, ignored tagged forms, etc.
* multiple identical automatic names have successive integers appended to them.

## Coordinates
* %ObjectCoordinates[$$] and %MorphismCoordinates[$$] can be used to refer symbolically to locations or centroids of objects and morphisms, see their usages.
* %LabelPosition -> %Outwards evaluates to %LabelPosition -> %AwayFrom[%ObjectCoordinates['Center']].
* %LabelPosition -> %Inwards evaluates to %LabelPosition -> %Towards[%ObjectCoordinates['Center']].

## Cloning

* the special object %Cloned[obj$, elem$] will generate a clone of the object   playing as elem$ and a morphism to the clone.
* %Cloned[obj$, elem$, label$] will attach label$ to morphism that connects the object to its clone.
* cloning can be customized as described by %CloneOptions.

## Options

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
| %DefaultMorphism | the default morphism to use, which is %MorphismArrow |
| %MorphismColors | how to color morphisms |
| %GradientSymbolOptions | additional options to customize %GradientSymbol |
* for more information about %ColorRules, see the usage of %DiagramColorRules, which is an alias.
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
* this feature can be used to make small tweaks to existing 'baseline' diagrams.
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
  LabelFontSize          -> Scaled[9/10],
  FontFamily             :> $MathFont,
  FontSize               -> 20,
  TextModifiers          -> {},
  Origin                 -> {0, 0},
  ColorRules             -> {},
  SymbolReplacements     -> None,
  CloneOptions           -> {},
  CloningFunction        -> None,
  DefaultMorphism        -> MorphismArrow,
  MorphismColors         -> None,
  DiagramColorRules      -> Automatic,
  GradientSymbolOptions  -> {},
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
  FixedGraphics[
    cdToPrimitives[cd],
    GraphicsScale -> Lookup[{opts}, GraphicsScale, 120]
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
    textModifiers, colorRules, diagramColorRules, morphismColors, symbolReplacements,
    cloneOptions, cloningFunction, gradientSymbolOptions,
    $defaultMorphism
  ];
  $objectNames = {};

  $objectCoordsList = {}; (* does not contain derived coordinates ($towardsCenter) *)
  $objects = $objectCoords = $objectSizes = $morphGradColors = UAssoc[];

  $itemSize = Automatic;
  $morphismNames = {};
  $morphismCurves = $cloneChildren = Assoc[];
  $inheritedOptions = opts;

  $saveMorphGradColors = False;
  SetAutomatic[diagramColorRules, colorRules];
  colorModifierFn = parseDiagramColorRules[diagramColorRules, gradientSymbolOptions];
  replacementFn = parseSymbolReplacements[symbolReplacements];
  extraModifiers = Composition[colorModifierFn, replacementFn];
  {$objectTextModifierFn, $morphismTextModifierFn} = Map[
    Composition[toModifierFunction[#], extraModifiers]&,
    If[AssocQ[textModifiers],
      Lookup[textModifiers, {"Objects", "Morphisms"}, {}],
      {textModifiers, textModifiers}
    ]
  ];
  $morphismColorFunction = processMorphismColors[morphismColors];

  $calculateLabelSizes = $autoSetback || $debugBounds;
  SetAutomatic[$setback, Rectangular[{15, 5}]];
  initialSetback = $setback;

  $cloneAbsoluteSetback = $cloneObjectLinkSetback = $cloneMorphismLinkSetback = $cloneDisplaceFn = $cloneInteriorLinkFn = $cloneObjectLinkFn = $cloneMorphismLinkFn = Null;
  $cloneInteriorLinks = $cloneExteriorLinks = {};
  $objFn = $arrFn = Id;
  {interiorLinkOptions, exteriorLinkOptions} = processCloneSpecs[cloneOptions, cloningFunction];

  items = desugar /@ items;
  $currentDiagramFontSize = fontSize; (* <- for recoloring rule to produce the right GradientSymbol *)
  objectPrimitives = parseObject /@ items;

  $toHigherPath = toHigherPath;
  $clonesExist = Len[$cloneChildren] > 0;
  SetScaledFactor[labelFontSize, fontSize];
  labelFontSize //= RoundNear;
  fontSize //= RoundNear;
  $currentDiagramFontSize = labelFontSize;
  If[$saveMorphGradColors, saveMorphismGradColors @ items];

  $morphIndex = 0;
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
  $coordReplacement = Append[resolveCoordinates @ Normal @ $objectCoords, z_Str :> unresolvedCoord[z]];
  primitives = ReplacePrimitiveCoordinates[primitives, $coordReplacement];

  arrowOpts = SeqDropOptions[{Setback, LabelFontSize, LabelFontSize, GraphicsScale, TextModifiers}] @ FilterOptions[MorphismArrow, opts];

  result = {
    arrowOpts,
    BendRadius -> 0.25,
    Setback -> initialSetback,
    LabelFontSize -> labelFontSize,
    FontSize -> fontSize,
    FontFamily -> fontFamily,
    primitives
  } /. $primitiveCanonicalizationRules;

  (* this evaluates the TextModifiers now, for two reasons: they are chunky ReplaceAll expressions that show up in if we call Normal on the CD and hence interfere with debugging
  (and whose effect is impossible to see directly since they haven't applied yet), and they can contain references to localColorOf which will shortly go out of scope and
  won't evaluate when boxifying. maybe we can do this as parseMorphism time? not sure! *)
  result //= applyMorphismTextModifiers;

  If[origin != {0, 0}, TranslatePrimitives[result, origin], result]
];

_cdToPrimitives := BadArguments[];

(**************************************************************************************************)

$legacyObjP = (_Str|_Int);

desugar = Case[

  DirectedEdge[a_, DirectedEdge[b_, c_]] :=
    Splice[% /@ {DirectedEdge[a, b], DirectedEdge[b, c]}];
  DirectedEdge[a_, DirectedEdge[b_, DirectedEdge[c_, d_]]] :=
    Splice[% /@ {DirectedEdge[a, b], DirectedEdge[b, c], DirectedEdge[c, d]}];
  DirectedEdge[a_, DirectedEdge[b_, c_]] -> {r1_, r2_} :=
    Splice[% /@ {DirectedEdge[a, b] -> r1, DirectedEdge[b, c] -> r2}];
  DirectedEdge[a_, DirectedEdge[b_, DirectedEdge[c_, d_]]] -> {r1_, r2_, r3_} :=
    Splice[% /@ {DirectedEdge[a, b] -> r1, DirectedEdge[b, c] -> r2, DirectedEdge[c, d] -> r3}];

  a:$legacyObjP -> b:$legacyObjP                     := % @ {DirectedEdge[a, b]};
  {a:$legacyObjP -> b:$legacyObjP, args__}           := % @ {DirectedEdge[a, b], args};
  DirectedEdge[a_, b_, lbl]                          := % @ {DirectedEdge[a, b], lbl};
  de_DirectedEdge                                    := % @ {de};
  de_DirectedEdge -> rhs_                            := % @ {de, rhs};

  {DirectedEdge[s_, t_], lbl_:None, args___} :=
    MorphismArrow[{s, t}, lbl, args];

  other_ := other;
];

(**************************************************************************************************)

PrivateFunction[toModifierFunction]

SetUsage @ "
TextModifiers is an option to %CommutativeDiagram that gives one or a list of modifier functions to apply after coloring and symbol replacement.
* a list of rules can be given to, these will behave like SymbolReplacements, but will automatically replace through styles via (un)burrowing.
"

CommutativeDiagram::badModifier = "Element `` of TextModifiers should be None, a function, list of rules, or a list of these."

toModifierFunction = Case[
  {} | None | Id         := Id;
  rules:$RuleListPattern := UnburrowModifiers /* ReplaceAll[rules] /* BurrowModifiers;
  list_List              := Composition @@ Map[%, Rev @ list];
  fn_                    := fn;
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

SetUsage @ "
DiagramColorRules is an option to %CommutativeDiagram and %StringDiagram that consists of rules for colors to apply to specific elements.

* elements that are colored are contents of objects, as well as morphism labels.

* rules can be in any of the following forms:
| patt$ -> color$ | apply color$ to any expressions matching patt$ |
| patt$ -> {color$1, color$2} | apply a gradient color to matching expressions |
| head$ -> 'Rainbow' | color head$[name$] canonically based on %ToRainbowInteger[name$] |
| head$ -> 'Gradient' | color head$[name$] as gradient based on source and target of morphism labeled as name$ |
| head$ -> 'Coloring' | typeset head$[name$][$$] as $$ colored based on name$ |
| head$ -> 'Framing' | typeset head$[name$][$$] as framed $$ with frame color based on name$ |

* 'Coloring' and 'Framing' will use the color associated with head$[name$], if there is one, or otherwise the canonical color for name$ |

* head$ must be a unary form like %CategoryObjectSymbol, %CategoryArrowSymbol, or %CategorySymbol.

* the following named rulesets are supported:
| 'Objects' | equivalent to %CategoryObjectSymbol -> 'Rainbow' |
| 'Arrows' | equivalent to %CategoryArrowSymbol -> 'Rainbow' |
| 'Functors' | equivalent to %FunctorSymbol -> 'Rainbow' |
| 'Categories' | equivalent to %FunctorSymbol -> 'Rainbow' |
| 'GradientArrows' | equivalent to {'Objects', %CategoryArrowSymbol -> 'Gradient'} |
| 'GradientFunctors' | equivalent to {'Categories', %CategoryFunctorSymbol -> 'Gradient'} |
| 'FramingFunctors' | depict functor application as a colored frame |
"
CommutativeDiagram::badrecolor = "Bad recoloring rule element ``."

PrivateFunction[parseDiagramColorRules]
PrivateVariable[$currentDiagramFontSize]

parseDiagramColorRules[e_, {opts__Rule}] := Scope[
  rules = parseDiagramColorRules2[e];
  rules = rules /. GradientSymbol[a___] :> GradientSymbol[a, opts]
];

parseDiagramColorRules[e_, {}] :=
  parseDiagramColorRules2[e];

parseDiagramColorRules[e_, o_] := (
  BadOptionSetting[StringDiagram, GradientSymbolOptions, MsgExpr @ o];
  parseDiagramColorRules2[e];
)

parseDiagramColorRules2 = Case[
  {} | None            := Id;
  el:(_Rule | _Str) := % @ {el};
  list_List            := ReplaceAll @ Map[toRecolorRule, list];
  other_               := (Message[CommutativeDiagram::badrecolor, other]; Id);
];

CommutativeDiagram::cruleNotUnaryForm = "Symbol `` provided as color rule element is not a unary form."

specialRecoloringRule[head_, "Rainbow"] :=
  RuleDelayed[z_head, RuleCondition @ StyledForm[z, ToRainbowColor @ ToRainbowInteger @ P1 @ z]];

specialRecoloringRule[head_, "Gradient"] := (
  $saveMorphGradColors = True;
  RuleDelayed[z_head, RuleCondition @ GradientSymbol[z, ColorGradient[{$gradColor[z, 1], $gradColor[z, 2]}], FontSize -> $currentDiagramFontSize]]
);

specialRecoloringRule[head_, "Framing"] :=
  RuleDelayed[z_head[e_], RuleCondition @ FramedForm[e, localColorOf[z]]];

specialRecoloringRule[head_, "Coloring"] :=
  RuleDelayed[z_head[e_], RuleCondition @ StyledForm[e, localColorOf[z]]];

PrivateFunction[localColorOf]

(* so, this is used to ensure that if we've declared a color for a head like FunctorSymbol,
we will use it for the 'Framing' / 'Coloring' spec above *)
localColorOf[z_] := Scope[
  z2 = colorModifierFn[z];
  If[z2 =!= z, findInteriorColor @ z2, ToRainbowColor @ ToRainbowInteger @ P1 @ z]
];

$namedRecoloringElements = <|
  "Objects"          -> {CategoryObjectSymbol -> "Rainbow"},
  "Arrows"           -> {CategoryArrowSymbol  -> "Rainbow"},
  "Functors"         -> {FunctorSymbol        -> "Rainbow"},
  "Categories"       -> {CategorySymbol       -> "Rainbow"},
  "GradientArrows"   -> {CategoryArrowSymbol  -> "Gradient", "Objects"},
  "GradientFunctors" -> {FunctorSymbol        -> "Gradient", "Categories"},
  "FramingFunctors"  -> {FunctorSymbol        -> "Framing"},
  "ColoringFunctors" -> {FunctorSymbol        -> "Coloring"}
|>;

$colorP = _Int | (_Symbol ? $styleFormHeadQ) | $ColorPattern;

(*
we've got to do something a bit tricky here, which is to deal with the fact
that turning FunctorSymbol["X"] into Style[FunctorSymbol["X"], $Red] will ruin the
normal typesetting behavior of things like FunctorSymbol["X"][...].

hence the use of StyledForm, which burrows.

TODO: do the same thing for GradientSymbol!
*)

toRecolorRule = Case[

  str_Str := Splice[% /@ LookupOrMessageKeys[$namedRecoloringElements, str, {}]];

  head_Symbol -> type:("Rainbow"|"Gradient"|"Framing"|"Coloring") := If[TrueQ @ $unaryFormHeadQ[head],
    specialRecoloringRule[head, type],
    Message[CommutativeDiagram::cruleNotUnaryForm, head]; Nothing
  ];

  f_ -> {a:$colorP, b:$colorP} := With[{c1 = toCol @ a, c2 = toCol @ b},
    RuleDelayed[f, RuleCondition @ GradientSymbol[f, ColorGradient[{c1, c2}], FontSize -> $currentDiagramFontSize]]
  ];

  a_ -> c:$colorP := With[{c1 = toCol @ c},
    RuleDelayed[(z:a), StyledForm[z, c1]]
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

toCol = Case[
  s_Symbol  := StyleFormData @ s;
  i_Int := ToRainbowColor @ i;
  other_    := other;
]

(**************************************************************************************************)

applyMorphismTextModifiers[primitives_] := ReplaceAll[
  primitives,
  MorphismArrow[path_, lbl:Except[_Rule], lopts___, TextModifiers -> fn_, ropts___] :>
    MorphismArrow[path, ApplyScriptScaling @ applyToLabel[toModifierFunction[fn], lbl], lopts, ropts]
];

applyToLabel = Case[

  Seq[fn_, x_ -> item_] := x -> %[fn, item];

  Seq[fn_, Placed[item_, p_]] := Placed[%[fn, item], p];

  Seq[fn_, Customized[item_, opts___]] := Customized[%[fn, item], opts];

  Seq[_, None] := None;

  Seq[fn_, item_] := fn @ item;

  Seq[fn_, list_List] := Map[%[fn, #]&, list];

];

(**************************************************************************************************)

SetUsage @ "
MorphismColors is an option to %CommuativeDiagram which can be:
| None | no colors |
| Inherited | inherit colors from the colors of their labels |
| 'Gradient' | color arrows as gradient between colors of their endpoints |
| {rule$1, rule$2, $$} | specify colors for individual morphisms |
* each rule$ can be a mapping 'label$' -> color$ or ind$ -> color$.
";

CommutativeDiagram::badMorphismColors = "MorphismColors -> `` is not recognized."
processMorphismColors = Case[
  None := None;

  Inherited := morphismLabelColor;

  "Gradient" := (
    $saveMorphGradColors = True;
    morphismGradColors
  );

  {rule__Rule, type_String} := orColor[% @ {rule}, % @ type];

  rules:{__Rule} := morphismColorRuleLookup[Append[_ -> None] @ rules];

  other_ := (Message[CommutativeDiagram::badMorphismColors, MsgExpr[other]]; None)
];

orColor[f_, g_][ma_] := Replace[f[ma], None :> g[ma]];

morphismLabelColor[_] := None;
morphismLabelColor[ma:MorphismArrow[_, lbl:Except[_Rule], ___]] :=
  findInteriorColor @ colorModifierFn @ lbl;

morphismGradColors[ma:MorphismArrow[path_, ___]] := Scope[
  st = findSourceTarget @ path;
  If[!StringVectorQ[st], Return @ None];
  cols = Lookup[$morphGradColors, DirectedEdge @@ st];
  If[!ColorVectorQ[cols], Return @ None];
  cols
];

morphismColorRuleLookup[rules_][ma_MorphismArrow] := Scope[
  label = SafePart[ma, 2];
  res = Replace[label, rules];
  If[res =!= None, Return @ res];
  Replace[$morphIndex, rules]
];

(**************************************************************************************************)

PrivateFunction[parseSymbolReplacements]

CommutativeDiagram::badrepspec = "SymbolReplacements -> `` is invalid.";

parseSymbolReplacements = Case[
  None | {}                               := Id;
  rules:($RulePattern | $RuleListPattern) := ReplaceAll[rules];
  other_                                  := (Message[CommutativeDiagram::badrepspec, other]; Id)
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

$toHigherPath = Id;
$defaultMorphism = MorphismArrow;

toMorphism := Case[
  Morphism[args___]                                        := $defaultMorphism[args];
  UniqueMorphism[path_, lbl:Except[_Rule]:None, args___]   := MorphismArrow[path, lbl, args, ArrowDashing -> Dashed];
  EqualityMorphism[path_, lbl:Except[_Rule]:None, args___] := MorphismArrow[path, lbl, "Equality", args];
  MapsToMorphism[path_, lbl:Except[_Rule]:None, args___]   := MorphismArrow[path, lbl, "MapsTo", args];
  ProMorphism[path_, lbl:Except[_Rule]:None, args___]      := MorphismArrow[path, lbl, "Proarrow", args];
  LineMorphism[path_, lbl:Except[_Rule]:None, args___Rule] := MorphismArrow[path, lbl, "Line", LabelPosition -> Center, LabelOrientation -> Aligned, ArrowColor -> $LightGray, args];
  LineMorphism[path_, lbl_, n:(_Int | {_Int, _Int}), args___Rule] := % @ LineMorphism[path, lbl, LabelOffset -> AlignedOffset[If[IntegerQ[n], {0, n}, n]], args];
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

$higherCoordP = _DirectedEdge | _UndirectedEdge | Rule[_, _Int] | _Str | _Int;
CommutativeDiagram::badhighercoord = "2-morphism endpoint `` is not recognized."
toHigherPathElem = Case[
  name_Str /; MemberQ[$objectNames, name] := name;
  pos:$CoordP                                := pos;
  m_MorphismCoordinates                      := m;
  o_ObjectCoordinates                        := o;
  Placed[spec:$higherCoordP, pos:$NumberP]   := MorphismCoordinates[spec, pos];
  spec:$higherCoordP                         := MorphismCoordinates[spec];
  spec_                                      := (Message[CommutativeDiagram::badhighercoord, MsgExpr @ spec]; {0, 0})
];

(**************************************************************************************************)

PrivateFunction[toComSugarArrow]

(* used in NamedDiagrams.m *)
toComSugarArrow[edge_, Null] := Nothing;
toComSugarArrow[edge_, (head:$morphismHeadP)[args___]] := head[edge, args];
toComSugarArrow[edge_, label_] := Morphism @@ ToList[edge, label];
toComSugarArrow[edge_, Reversed[label_]] := toComSugarArrow[Rev @ edge, label];f$morphismHeadP
toComSugarArrow[edge_, Reversed[Reversed[label_]]] := %[edge, label];

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

  s_Str := % @ namedCloneOpts[s];

  {s_Str, opts___Rule} := % @ Join[{opts}, namedCloneOpts @ s];

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
  s_Str | s_Rule                  := MorphismArrow[#1, #2, s]&;
  Sized[e_, s_]                   := ReplaceAll[% @ e, (h:$morphismHeadP|MorphismArrow)[args___] :> h[args, ArrowheadSize -> s]];
  Reversed[s_]                    := With[{fn = % @ s}, fn[Rev @ #1, #2]&];
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

parseMorphism = Case[
  {_, _} -> _                                        := Nothing;
  ({__List} -> _List) ? threadedObjectsQ             := Nothing;
  Null                                               := Nothing;
  Setback -> sb_                                     := ($setback = sb; Nothing);
  opt:(_Symbol -> _)                                 := flipSymbolicPositions @ opt;

  cd_CommutativeDiagram                              := flipSymbolicPositions @ Append[cd, Unevaluated @ $inheritedOptions];

  other_                                             := ($morphIndex++; processMorphism1 @ other);
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
  ObjectCoordinates[i_Int]          := Lookup[$objectCoords, getObjectName @ i, Message[CommutativeDiagram::objcspeclen, i, Len @ $objectNames]; {0, 0}];
  ObjectCoordinates[lbl_Str]        := Lookup[$objectCoords, lbl, Message[CommutativeDiagram::badobjlbl, lbl]; {0, 0}];
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
  name_Str := Scope[
    ind = IndexOf[$morphismNames, name];
    If[MissingQ[ind], ReturnFailed[CommutativeDiagram::badmorphname, name]];
    Part[$morphismCurves @ name, 1]
  ];
  i_Int := Scope[
    morph = SafePart[$morphismCurves @ None, i];
    If[MissingQ[morph], ReturnFailed[CommutativeDiagram::badmorphind, i]];
    morph
  ];
  spec_ -> i_Int := Scope[
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
  text = ApplyScriptScaling @ Text[$objectTextModifierFn @ obj, lbl, Lookup[$SideToCoords, alignment]];
  If[$calculateLabelSizes,
    size = $itemSize;
    If[ContainsQ[size, Automatic],
      text2 = Append[text, BaseStyle -> {FontSize -> fontSize, FontFamily -> fontFamily}];
      isize = N[MakeTextImageSize @ text2] + 1;
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
  1 <= Abs[i] <= Len[$objectNames],
  Part[$objectNames, i],
  Message[CommutativeDiagram::badobjindex, i]; {0, 0}
];

(**************************************************************************************************)

$edgeyHead = (List | Rule | DirectedEdge);

(* resolve alternate heads and lazy specs to become MorphismArrow *)
processMorphism1 = Case[
  m:($morphismHeadP[__])                              := % @ toMorphism @ m;
  MorphismArrow[$edgeyHead[i_Int, j_], args___]       := % @ MorphismArrow[{getObjectName @ i, j}, args];
  MorphismArrow[$edgeyHead[i_, j_Int], args___]       := % @ MorphismArrow[{i, getObjectName @ j}, args];
  MorphismArrow[(Rule|DirectedEdge)[i_, j_], args___] := % @ MorphismArrow[{i, j}, args];
  other_                                              := processMorphism2 @ flipSymbolicPositions @ other;
];

(**************************************************************************************************)

(* attach automatic setbacks, text modifiers, and arrow colors *)
processMorphism2 = Case[

  (* TODO: setback intefereces with cloning *)
  ma_MorphismArrow /; TrueQ[$autoSetback] && FreeQ[ma, Setback] := Scope[
    st = findSourceTarget @ P1 @ ma;
    If[st === None, Return @ ma];
    {src, tgt} = st;
    {sz1, sz2} = lookupObjectSize /@ {src, tgt};
    setback = Map[makeEndpointSetback, {sz1, sz2}];
    setback = ExtendSetback[setback, $setback];
    % @ Append[ma, Setback -> setback]
  ];

  ma_MorphismArrow /; TrueQ[$morphismTextModifierFn =!= Id] && FreeQ[ma, TextModifiers] := Scope[
    modifiers = resolveGradColors @ $morphismTextModifierFn;
    % @ Append[ma, TextModifiers -> modifiers]
  ];

  ma_MorphismArrow /; TrueQ[$morphismColorFunction =!= None] && FreeQ[ma, ArrowColor] := Scope[
    col = $morphismColorFunction @ ma;
    If[col === None, ma, % @ Append[ma, ArrowColor -> col]]
  ];

  other_ := processMorphism3 @ other;
];

resolveGradColors[ma_] := With[
  {gradColors = $morphGradColors},
  ma /. $gradColor[m_, i_] :> Part[gradColors[m], i]
];

saveMorphismGradColors = Case[

  e_List := Scan[%, e];

  m:(MorphismArrow|$morphismHeadP)[path_, ___] := Scope[
    stRaw = findSourceTarget @ path;
    st = Lookup[$objects, stRaw];
    If[ContainsQ[st, Missing], Return[]];
    stColored = $objectTextModifierFn /@ st;
    colors = findInteriorColor /@ stColored;
    If[Len[colors] == 2,
      (* save by label and by edge *)
      lbl = SafePart[m, 2];
      If[!RuleQ[lbl] && !MissingQ[lbl], AssociateTo[$morphGradColors, Replace[lbl, Cloned[c_, _] :> c] -> colors]];
      AssociateTo[$morphGradColors, Apply[DirectedEdge, stRaw] -> colors];
    ];
  ];

  _ := Null;
];

makeEndpointSetback = Case[
  {a_, b_} := Rectangular[{a, b}];
  a_       := a;
];

lookupObjectSize = Case[
  s_Str := Lookup[$objectSizes, s, {15, 15}];
  _        := {15, 15};
];

(**************************************************************************************************)

PrivateFunction[findInteriorColor]

findInteriorColor[e_] := DeepFirstCase[e,
  GradientSymbol[_, {c1_, c2_} | ColorGradient[{c1_, c2_}, ___], ___] :> ToRainbowColor[c1],
  DeepFirstCase[e,
    StyledForm[__, c:$ColorPattern, ___] :> c,
    DeepFirstCase[e, (Style[___, c:$ColorPattern, ___] | _FramedForm[_, c:$ColorPattern]) :> c]
  ]
];

(**************************************************************************************************)

(* produce cloned morphisms if any *)
processMorphism3 = Case[

  ma_MorphismArrow /; TrueQ[$arrFn =!= Id] :=
    processMorphismWithCloning @ MapAt[$arrFn, ma, 2];

  ma_MorphismArrow /; TrueQ[$clonesExist] :=
    processMorphismWithCloning @ ma;

  other_ := other;
];

processMorphismWithCloning[ma_MorphismArrow] := Scope[

  st = findSourceTarget @ P1 @ ma;
  If[!StringVectorQ[clonedSt = Lookup[$cloneChildren, st]], Return @ ma];
  (* if the morphism's endpoints were both cloned, we need to create a cloned morphism *)

  ma2 = ma;

  (* come up with a label for the clone morphism *)
  label = SafePart[ma, 2];
  If[MatchQ[label, Cloned[_, _]],
    {Part[ma2, 2], clonedLabel} = List @@ label,
    clonedLabel = None
  ];

  (* generate the clone morphism that links the clone objects *)
  cloneLink = processClonedMorphism @ If[$cloneInteriorLinkFn === Inherited,
    DeleteCases[ReplacePart[ma, {1 -> clonedSt, 2 -> clonedLabel}], Setback -> {__Rectangular}],
    $cloneInteriorLinkFn[clonedSt, clonedLabel]
  ];
  If[cloneLink === Nothing, Return @ ma2];
  AppendTo[$cloneInteriorLinks, cloneLink];

  (* link the cloned morphism to the original morphism, if desired *)
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
  {$arrFn = Id},
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
  name_Str                 := name;
  ObjectCoordinates[spec_] := % @ spec;
  i_Int                    := getObjectName[i];
]
