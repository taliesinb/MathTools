PublicOption[ZOrder, PathStyle, RegionStyle, HighlightRadius, PathRadius, PathOutline, PathVertexHighlighting, DiskRadius, EdgeSetback, SimplifyRegions]

SetUsage @ "ZOrder is an option that controls how graphical elements are sorted from back to front."
SetUsage @ "PathStyle is an option that controls how paths are rendered in highlights."
SetUsage @ "RegionStyle is an option that controls how regions are rendered in highlights."
SetUsage @ "HighlightRadius is an option that controls the radius of highlighted regions."
SetUsage @ "PathRadius is an option that controls the radius of highlighted paths."
SetUsage @ "PathVertexHighlighting is an option that controls how which vertices on paths are highlighted."
SetUsage @ "DiskRadius is an option that controls the radius of highlighted vertices."
SetUsage @ "EdgeSetback is an option that controls how far an edge should be set back from final vertex."
SetUsage @ "PathOutline is an option that controls whether paths are outlined."
SetUsage @ "SimplifyRegions is an option that detects e.g. circular regions and uses Disk to highlight them."

(**************************************************************************************************)

PublicFunction[HighlightGraphRegion]

DeclareArgumentCount[HighlightGraphRegion, {2, 3}];

Options[HighlightGraphRegion] = $ExtendedGraphOptions;

declareSyntaxInfo[HighlightGraphRegion, {_, _, OptionsPattern[]}];

HighlightGraphRegion[graph_, highlights_, style:(_List | _String | $ColorPattern), opts:OptionsPattern[]] :=
  HighlightGraphRegion[graph, highlights, HighlightStyle -> style, opts];

HighlightGraphRegion[graph_, highlights_, opts:OptionsPattern[]] := Scope[
  oldHighlights = AnnotationValue[graph, GraphRegionHighlight];
  oldHighlights = If[FailureQ[oldHighlights], {}, ToList @ oldHighlights];
  ExtendedGraph[
    graph,
    GraphRegionHighlight -> Join[oldHighlights, ToList @ highlights],
    opts
  ]
];

(**************************************************************************************************)

PrivateFunction[GraphRegionGraphics]

GraphRegionGraphics[graph_, regionSpec_] := Scope[

  graph = CoerceToGraph[1];

  GraphPlotScope[graph,
    {graphics, padding} = resolveGraphRegionHighlightGraphics @ regionSpec;
    graphics = Part[graphics, All, 2];
    plotRange = $GraphPlotRange
  ];

  If[FailureQ[graphics], ReturnFailed[]];

  Graphics[graphics, PlotRange -> plotRange, Framed -> False]
];

(**************************************************************************************************)

(**************************************************************************************************)

(** highlight processing code              **)
(**************************************************************************************************)


PrivateFunction[resolveGraphRegionHighlightGraphics]

resolveGraphRegionHighlightGraphics[None | {} | <||>] :=
  {{}, None, 0};

GraphRegionHighlight::badopt = "Invalid setting ``.";

resolveGraphRegionHighlightGraphics[spec_] := Scope[

  If[MatchQ[spec, {__Association}], spec = Join @@ spec];

  {$highlightStyle, $highlightColor, $highlightOpacity} = LookupExtendedThemedOption[$Graph, {HighlightStyle, HighlightColor, HighlightOpacity}];

  {$highlightStyle, $highlightColor} = {$highlightStyle, $highlightColor} /. cc_CardinalColor :> evalCardinalColor[cc];
  $highlightOpacity //= normalizeStyles;
  $highlightColor //= normalizeStyles;
  $highlightStyle //= normalizeStyles;

  SetAutomatic[$highlightStyle, {}];
  SetAutomatic[$highlightOpacity, 0.5];
  If[!NumberQ[$highlightOpacity], $highlightOpacity //= ColorOpacity];

  defaultPalette = If[FreeQ[spec, "Foreground" | "Background" | Opaque | "Replace" | "ReplaceEdges"], "Dark", "Medium"];
  SetAutomatic[$highlightColor, Offset[defaultPalette, 2]];

  If[MatchQ[$highlightColor, _String | Offset[_String, _]], $highlightColor //= ToColorPalette];
  If[!ColorQ[$highlightColor] && !ColorVectorQ[$highlightColor],
    Message[GraphRegionHighlight::badopt, $highlightColor];
    $highlightColor = $DarkGreen;
  ];
  
  If[ListQ[$highlightStyle] && !VectorQ[$highlightStyle, ListQ],
    $highlightStyle = {$highlightStyle} (* a single list will be repeated for each element *)
  ];

  $highlightRadius = $GraphMaxSafeVertexSize;

  (* todo: use the same conversion functions as I use in GraphPlotting *)
  $pathRadius = Automatic; $diskRadius = Automatic; $pathVertexHighlighting = All;
  $graphPlotWidth = First @ $GraphPlotSize;
  $radiusScaling = If[$GraphIs3D, 0.25, 1];
  $edgeBaseStyle := $edgeBaseStyle = FirstCase[
    $GraphPlotGraphics,
    Annotation[Style[_, style___], _, "EdgePrimitivesRoot"] :> toDirective[{style}],
    {}, {0, Infinity}
  ];

  $cardinalFilter = All;
  $zorder = 1;
  $requiredPadding = 0;
  $pathStyle = "Line";
  $regionStyle = "Highlight";
  $arrowheadPosition = 1.0;
  $arrowheadSize = Automatic;
  $arrowheadStyle = Automatic;
  $edgeSetback = 1;
  $outline = False;
  $simplifyRegions = False;

  CollectTo[{$highlightsBag, $legendsBag}, processOuterSpec[spec]];
  legend = If[$legendsBag === {}, None, DiscreteColorLegend @ Association @ $legendsBag];

  $highlightsBag = Sort[$highlightsBag];
  {Part[$highlightsBag, All, {1, 3}], legend, $requiredPadding}
];

requirePadding[p_] := $requiredPadding = Max[$requiredPadding, p];

$translation = None;
sowHighlight[g_] /; $translation =!= None :=
  StuffBag[$highlightsBag, {$zorder, BagLength[$highlightsBag], Translate[g, $translation]}];

sowHighlight[g_] := StuffBag[$highlightsBag, {$zorder, BagLength[$highlightsBag], g}];

sowLegend[name_, color_] := StuffBag[$legendsBag, name -> color];

debugGraphic[g_] := (StuffBag[$highlightsBag, {100, 100, {Black, g}}]; g);

echoGraphic[g_] := (Echo[Graphics[g, ImageSize -> {200, 200}]]; g);

(**************************************************************************************************)

processOuterSpec = Case[
  spec_ ? ListOrAssociationQ := Block[
    {$i = 1, $highlightStyle = $highlightStyle, $highlightColor = $highlightColor},
    MapIndex1[processIndexedStyleSpec[#1, #2, $i++]&, spec];
  ];
  s_Style :=
    (* this applies styles before iterating over a list or associations *)
    processStyleSpec[s, processOuterSpec];
  other_ := % @ {other};
];

processIndexedStyleSpec[spec_, key_, index_] := Block[
  {$highlightStyle = lookupIndexedSpec[$highlightStyle, key, index],
   $highlightColor = lookupIndexedSpec[$highlightColor, key, index],
   spec2 = spec},
  If[Head[key] === Key, spec2 = Legended[spec2, First @ key]];
  processStyleSpec[Style[spec2, $highlightStyle, HighlightColor -> $highlightColor], processGeneralSpec]
];

GraphRegionHighlight::missspecstyle = "Cannot find a style to use for spec part ``.";

lookupIndexedSpec[<||> | {}, _, _] := Null;
lookupIndexedSpec[spec_Association, key_, index_] := Lookup[spec, key, Message[GraphRegionHighlight::missspecstyle]; Null];
lookupIndexedSpec[spec_List, key_, index_] := If[index <= Length[spec], Part[spec, index], Last @ spec];
lookupIndexedSpec[spec_, key_, index_] := spec;

(**************************************************************************************************)

GraphRegionHighlight::badelem = "Unknown highlight element ``.";

PublicHead[DotLine, DotLineDot, LineDot, DotArrow, DotArrowDot, ArrowDot]

processGeneralSpec = Case[
  Legended[Style[spec_, opts__], label_]            := % @ Style[Legended[spec, label], opts];
  Legended[spec_, label_]                           := (% @ spec; sowLegend[label, $highlightColor];);
  Axes -> spec_                                     := processAxesSpec @ spec;
  spec_Style                                        := processStyleSpec[spec, %];
  Labeled[spec_, _]                                 := % @ spec;
  region_ ? GraphRegionElementQ                     := resolveHighlightSpec @ region;
  Translate[spec_, t_]                              := Scope[$translation = t; % @ spec];
  (head:$dotLineP)[spec_ ? validVertexVectorQ]      := % @ head @ Line @ spec;
  (head:$dotLineP)[spec_]                           := Scope[$pathStyle = SymbolName[head]; % @ spec];
  (head:$dotArrowP)[spec_]                          := Scope[$pathStyle = SymbolName[head]; % @ Arrow @ spec];
  Arrow[spec_List ? validVertexVectorQ]             := % @ Arrow @ Line @ spec;
  Arrow[spec_]                                      := Block[{$pathStyle = Replace[$pathStyle, "Line" -> "Arrow"]}, % @ spec];
  Null                                              := Null;
  list_List                                         := Scan[processGeneralSpec, list];
  other_                                            := Message[GraphRegionHighlight::badelem, Shallow[other]];
  ,
  {$dotLineP -> (DotLine|LineDot|DotLineDot), $dotArrowP -> (DotArrow|ArrowDot|DotArrowDot)}
];

validVertexVectorQ[vs_] := VectorQ[vs, validVertexQ];

(* Offset needs processRegionSpec to be resolved, which is not available yet, so assume it's correct *)
validVertexQ[v_] := If[ContainsQ[v, Offset], True, !FailureQ[findVertexIndex @ v]];

(**************************************************************************************************)

processAxesSpec[spec:(All|_Integer|{__Integer})] := Scope[
  colors = LookupCardinalColors[$Graph];
  KeyValueMap[axisHighlight, Part[colors, If[IntegerQ[spec], {spec}, spec]]]
];

processAxesSpec[spec_] := Scope[
  colors = LookupCardinalColors[$Graph];
  words = Map[ToPathWord, ToList @ spec];
  colors = AssociationMap[HumanBlend @ LookupCardinalColors[$Graph, StripInverted /@ #]&, words];
  KeyValueMap[axisHighlight, colors]
];

axisHighlight[word_, color_] := Scope[
  path = First @ processRegionSpec @ InfiniteLine[GraphOrigin, word];
  If[Length @ DeleteDuplicates @ First @ path <= 2, Return[]];
  processGeneralSpec @ Style[path, color, HighlightRadius -> 2];
];

(**************************************************************************************************)

GraphRegionHighlight::badstylespec = "Unknown style specification ``.";

processStyleSpec[spec_, f_] := Scope[
  $innerFunc =f ;
  iProcessStyleSpec @ spec
];


$additionalStyleOptions = {
  PerformanceGoal, PathStyle, RegionStyle, ArrowheadPosition, ArrowheadSize, ArrowheadStyle, PointSize, HighlightRadius, DiskRadius,
  PathRadius, EdgeSetback, SimplifyRegions, ZOrder, Cardinals, PathVertexHighlighting
};

Unprotect[Style];
SyntaxInformation[Style] = ReplaceOptions[
  SyntaxInformation[Style],
  "OptionNames" -> Union[Keys @ Options @ Style, SymbolName /@ $additionalStyleOptions]
];
Protect[Style];

$namedTransformsPattern = "FadeGraph" | "FadeProtect" | "FadeEdges" | "FadeVertices" | "HideArrowheads" | "HideEdges" | "HideVertices" | "SemitransparentArrowheads";
$namedStyles = "Background" | "Foreground" | "Replace" | "ReplaceEdges" | $namedTransformsPattern;
$highlightStylePattern = Rule[Alternatives @@ $additionalStyleOptions, _] | $namedStyles;

iProcessStyleSpec = Case[
  Style[most__, HighlightColor -> Null]        := % @ Style[most];
  Style[most__, HighlightColor -> color_] := Scope[$highlightColor = SetColorOpacity[color, $highlightOpacity]; % @ Style @ most];
  Style[most__, HighlightOpacity -> o_] :=   Scope[$highlightOpacity = ExtractFirstOpacity @ o; Block[
    {$highlightColor = SetColorOpacity[RemoveColorOpacity @ $highlightColor, $highlightOpacity]},
    % @ Style @ most
  ]];
  Style[most__, o:$OpacityPattern]             := % @ Style[most, HighlightOpacity -> o];
  Style[most__, c:$ColorPattern]               := % @ Style[most, HighlightColor -> c];
  Style[most__, {list___}]                     := % @ Style[most, list];
  Style[most__, SimplifyRegions -> boole_]     := Scope[$simplifyRegions = boole;         % @ Style @ most];
  Style[most__, PerformanceGoal -> goal_]      := Scope[$perfGoal = goal;                 % @ Style @ most];
  Style[most__, PathStyle -> style_]           := Scope[$pathStyle = style;               % @ Style @ most];
  Style[most__, RegionStyle -> style_]         := Scope[$regionStyle = style;             % @ Style @ most];
  Style[most__, ArrowheadPosition -> pos_]     := Scope[$arrowheadPosition = N[pos];      % @ Style @ most];
  Style[most__, DiskRadius -> sz_]             := Scope[$diskRadius = sz;                 % @ Style @ most];(* this is measured in points, not in fraction of image width *)
  Style[most__, ArrowheadStyle -> style_]      := Scope[$arrowheadStyle = style;          % @ Style @ most];
  Style[most__, ArrowheadSize -> sz_]          := Scope[$arrowheadSize = sz;              % @ Style @ most];
  Style[most__, HighlightRadius -> r_]         := Scope[$radiusScaling = r;               % @ Style @ most];
  Style[most__, PathRadius -> r_]              := Scope[$pathRadius = r;                  % @ Style @ most];
  Style[most__, PathVertexHighlighting -> ph_] := Scope[$pathVertexHighlighting = ph;     % @ Style @ most];
  Style[most__, PathOutline -> out_]           := Scope[$outline = out;                   % @ Style @ most];
  Style[most__, EdgeSetback -> r_]             := Scope[$edgeSetback = r;                 % @ Style @ most];
  Style[most__, Cardinals -> cards_]           := Scope[$cardinalFilter = ToList @ cards; % @ Style @ most];
  Style[most__, n:$namedTransformsPattern]     := (AttachGraphPlotAnnotation[n];          % @ Style @ most);
  (* this doesn't really work, since the point styles for vertices live outside the "VertexPrimitives", so we even though the points
  get protected, the point style still gets faded *)
  Style[most__, "FadeProtect"]                 := % @ Style[most, "FadeGraph", RegionStyle -> "FadeProtect"];
  Style[most__, "Background"]                  := % @ Style[most, Opaque, ZOrder -> -10];
  Style[most__, "Foreground"]                  := % @ Style[most, Opaque, ZOrder -> 10];
  Style[most__, "Replace"]                     := % @ Style[most, Opaque, ZOrder -> 10, PathStyle -> "Replace", RegionStyle -> "Replace"];
  Style[most__, "ReplaceEdges"]                := % @ Style[most, Opaque, ZOrder -> 10, PathStyle -> "ReplaceEdges", RegionStyle -> "ReplaceEdges"];
  Style[most__, ZOrder -> z_Integer]           := Scope[$zorder = z; % @ Style @ most];
  Style[most__, Null]                          := % @ Style[most];
  Style[elem_]                                 := $innerFunc[elem];
  Style[__, remaining_]                        := Message[GraphRegionHighlight::badstylespec, Print[remaining]; remaining];
];

(**************************************************************************************************)

resolveHighlightSpec[region_] := Scope[
  results = ToList @ processRegionSpec @ region;
  Scan[highlightRegion, results]
];

GraphRegionHighlight::interror = "Internal error: couldn't highlight `` data.";

highlightRegion[other_] := (
  Message[GraphRegionHighlight::interror, other];
);

highlightRegion[GraphRegionData[vertices_, edges_]] /; $regionStyle === "FadeProtect" := Scope[
  $newVertices = {}; $newEdges = {};
  TransformGraphPlotPrimitives[fadeProtectPrimitives, edges, "EdgePrimitives"];
  TransformGraphPlotPrimitives[fadeProtectPrimitives, vertices, "VertexPrimitives"];
];

highlightRegion[GraphRegionData[vertices_, edges_]] /; StringQ[$regionStyle] && StringStartsQ[$regionStyle, "Replace"] := Scope[
  $newVertices = {}; $newEdges = {};
  TransformGraphPlotPrimitives[removeHighlightedPathEdges, edges, "EdgePrimitives"];
  TransformGraphPlotPrimitives[removeHighlightedPathVertices, vertices, "VertexPrimitives"];
  newEdges = SimplifyGraphicsPrimitives @ $newEdges;
  newVertices = SimplifyGraphicsPrimitives @ $newVertices;
  diskRadius = $diskRadius;
  If[diskRadius =!= Automatic,
    newVertices = replaceVertexSize[newVertices, PointSize[diskRadius / $GraphPlotEffectiveImageWidth]];
  ];
  sowHighlight @ Style[
    replaceWithColor[{newEdges, newVertices}, $highlightColor, $regionStyle === "ReplaceEdges"],
    $edgeBaseStyle
  ];
];

$pointSizeP = (_AbsolutePointSize | _PointSize);
replaceVertexSize[primitives_, size_] :=
  If[FreeQ[primitives, $pointSizeP],
    Style[primitives, size],
    ReplaceAll[primitives, $pointSizeP :> size]
  ];

highlightRegion[GraphRegionData[vertices_, edges_]] /; $regionStyle === "Highlight" := Scope[
  If[$perfGoal === "Speed",
    Return @ highlightRegion @ GraphRegionData[vertices, {}]];
  graphics = subgraphCoveringGraphics[
    $highlightRadius * $radiusScaling, vertices, edges,
    $IndexGraphEdgeList, $VertexCoordinates, $EdgeCoordinateLists
  ];
  sowHighlight @ Style[graphics, $highlightColor];
];

highlightRegion[GraphRegionData[vertices_, {}]] /; $regionStyle === "Highlight" :=
  sowVertexPoints @ vertices;

sowVertexPoints[vertices_] := Scope[
  diskRadius = $diskRadius;
  SetAutomatic[diskRadius, 10];
  requirePadding @ diskRadius;
  coords = Part[$VertexCoordinates, DeleteDuplicates @ vertices];
  highlights = If[$GraphIs3D,
    Style[
      Sphere[coords, diskRadius / $GraphPlotEffectiveImageWidth * $graphPlotWidth],
      Color3D[$highlightColor]
    ]
  ,
    Style[
      Point @ coords,
      PointSize[diskRadius / $GraphPlotEffectiveImageWidth], $highlightColor
    ]
  ];
  sowHighlight @ highlights
];

$currentRegionAnnotations = <||>;
highlightRegion[GraphRegionAnnotation[data_, anno_]] := Scope[
  $currentRegionAnnotations = anno;
  highlightRegion @ data;
]

applyPathVertexHighlighting[vertices_, All] := vertices;
applyPathVertexHighlighting[_, None] := {};
applyPathVertexHighlighting[vertices_, "Endpoints"] := FirstLast @ vertices;
applyPathVertexHighlighting[vertices_, "Interior"] := Part[vertices, 2;;-2];

highlightRegion[GraphPathData[vertices_, edges_, inversions_]] := Scope[

  segments = Part[$EdgeCoordinateLists, edges];
  If[inversions =!= {},
    segments //= MapIndices[Reverse, inversions]];
  numSegments = Length @ segments;

  pathRadius = $pathRadius;
  SetAutomatic[pathRadius, Switch[$pathStyle,
    "Replace" | "ReplaceEdges",           First @ evalGraphicsValue @ GraphicsValue["EdgeThickness"],
    s_ /; StringContainsQ[s, "Arrow"],    4.0,
    "Line",                               6.0,
    s_ /; StringContainsQ[s, "Line"],     4.0,
    _,                                    1.0
  ]];

  pathThickness = pathRadius / $GraphPlotEffectiveImageWidth;
  thicknessRange = pathThickness * $graphPlotWidth * 1.5;
  bendRange = Max[thicknessRange/2, 5 / $GraphPlotEffectiveImageWidth * $graphPlotWidth];

  adjustments = parseAdjustments /@ Lookup[$currentRegionAnnotations, PathAdjustments, {}];

  lastIsModified = !MatchQ[Lookup[adjustments, numSegments], _Missing | _String | {"Arrowhead", _}];
  $extraArrowheads = {};
  segments = joinSegments[segments, adjustments, $pathStyle =!= "Replace"];

  doArrow = StringContainsQ[$pathStyle, "Arrow"];

  color = FirstCase[{$highlightColor, $highlightStyle}, $ColorPattern, Gray, {0, Infinity}];
  darkerColor := OklabDarker[RemoveColorOpacity @ color, .05];
  arrowheadColor = If[$highlightOpacity == 1, darkerColor, color];

  vertexSizePR = evalGraphicsValue @ GraphicsValue["VertexSize", "PlotRange"] * 1.2;
  setbackDistance = If[lastIsModified || !doArrow || Max[$arrowheadPosition] != 1, 0, Max[$edgeSetback * thicknessRange/2, vertexSizePR]];
  If[$edgeSetback == 0, setbackDistance = 0];
  isEdgeBased = True;

  pathPrimitives = If[$GraphIs3D && $pathStyle === "Line",
    isEdgeBased = False;
    Tube[segments, thicknessRange / 3]
  ,
    Switch[
      StringReplace[$pathStyle, "Dot" -> "Disk"],
      "Arrow" | "DiskArrow" | "DiskArrowDisk" | "ArrowDisk",
        arrowheadSize = $arrowheadSize;

        If[NumericQ[arrowheadSize],
          arrowheadSize = N[arrowheadSize] / $GraphPlotEffectiveImageWidth];
        SetAutomatic[arrowheadSize, pathThickness];

        (* note: arrowhead size is not a fraction of the full image width, but rather a fraction of the *displayed* image width, which
        excludes ImagePadding and is also modified when the aspect ratio of PlotRange does not match that of ImageSize. so this can be much
        larger than 1 for a very narrow horizontal plot range. *)
        baseArrowheads = List[
          arrowheadSize, $apos,
          makeHighlightArrowheadShape[arrowheadColor, 5, $GraphIs3D]
        ];
        arrowheads = Arrowheads @ If[ListQ[$arrowheadPosition],
          Map[ReplaceAll[baseArrowheads, $apos -> #]&, $arrowheadPosition],
          List @ ReplaceAll[baseArrowheads, $apos -> $arrowheadPosition]
        ];
        diskRadius = $diskRadius;
        SetAutomatic[diskRadius, pathRadius * 1.5];
        diskPrimitives = makeEndDiskPrimitives[vertices, $pathStyle, diskRadius];
        arrow = setbackArrow[segments, setbackDistance];
        extraArrowheads = If[$extraArrowheads === {}, Nothing,
          Style[Arrow[#1], Transparent, Arrowheads @ List @ ReplaceAll[baseArrowheads, $apos -> #2]]& @@@ $extraArrowheads
        ];
        {arrowheads, arrow, diskPrimitives, extraArrowheads},

      "DiskLine" | "DiskLineDisk" | "LineDisk",

        diskRadius = $diskRadius;
        SetAutomatic[diskRadius, pathRadius * 1.5];

        diskPrimitives = makeEndDiskPrimitives[vertices, $pathStyle, diskRadius];
        linePrimitives = setbackLine[segments, setbackDistance];

        If[$highlightOpacity < 1,
          pathRadius2 = (pathRadius / 2) / $GraphPlotEffectiveImageWidth * $graphPlotWidth;
          shapeFn = If[$GraphIs3D, CapsuleShape[#, pathRadius2]&, StadiumShape[#, pathRadius2]&];
          linePrimitives //= ReplaceAll[Line[coords_] :> MapWindowed[shapeFn, coords]];
          (* reg = BoundaryDiscretizeRegion @ RegionUnion @ Flatten @ {diskPrimitives, linePrimitives}; *)
          reg = BoundaryDiscretizeGraphics @ Flatten @ {diskPrimitives, linePrimitives};
          polygon = RegionPolygon @ reg;
          polygon
        ,
        
          {diskPrimitives, linePrimitives}
        ]
      ,
      "Replace" | "ReplaceEdges",
        Block[{$cardinalFilter = $cardinalFilter},
          If[$pathStyle === "ReplaceEdges", SetAll[$cardinalFilter, {}]];
          $newVertices = $newEdges = {}; $firstRemovedArrowheads = None;
          TransformGraphPlotPrimitives[removeHighlightedPathEdges, edges, "EdgePrimitives"];
          TransformGraphPlotPrimitives[removeHighlightedPathVertices, applyPathVertexHighlighting[vertices, $pathVertexHighlighting], "VertexPrimitives"];
          $newEdges = Which[
            edges === {}, {},
            adjustments === {}, $newEdges,
            True, Style[Arrow @ segments, $firstRemovedArrowheads, CapForm @ "Round"]
          ];
          edgeBaseStyle = $edgeBaseStyle;
          If[$pathRadius =!= Automatic,
            $newEdges //= removeThickness;
            edgeBaseStyle //= removeThickness;
          ];
          pathPrimitives = {Style[$newEdges, edgeBaseStyle], $newVertices};
          replaceWithColor[pathPrimitives, $highlightColor, $arrowheadStyle === Inherited]
        ]
      ,
      "Line" | _,
        {CapForm["Round"], setbackLine[segments, setbackDistance]}
    ]
  ];

  If[$GraphIs3D && $pathStyle =!= "Replace",
    diskRadius = $diskRadius;
    SetAutomatic[diskRadius, pathRadius * 1.5];
    diskRadius = diskRadius / $GraphPlotEffectiveImageWidth * $graphPlotWidth;
    pathPrimitives = pathPrimitives /. {
      Disk[p_, r_] :> Sphere[p, r],
      Arrow[a___] :> Arrow[Tube[a, diskRadius / 1.5]]
    };
    color = color /. c:$ColorPattern :> Color3D[c]
  ];

  color2 = SetColorOpacity[color, $highlightOpacity];
  pathStyle = If[$GraphIs3D && $highlightOpacity < 0.9 && !isEdgeBased,
    FaceForm[color2, None], color2];

  If[$outline === True,
    outlineColor = If[$outline === True, darkerColor, $outline];
    sowHighlight @ Style[
      Line @ segments,
      JoinForm @ "Round", CapForm @ "Round",
      outlineColor, absThickness[pathThickness * 1.5]
    ];
  ];

  (* unfortunately CapForm doesn't do anything for Arrow *)
  requirePadding[If[doArrow, 1.2, 1] * thicknessRange / $graphPlotWidth * $GraphPlotEffectiveImageWidth];
  sowHighlight @ Style[
    pathPrimitives,
    JoinForm @ "Round", CapForm @ "Round",
    pathStyle, absThickness @ pathThickness
  ];
];

absThickness[thickness_] := AbsoluteThickness[thickness * $GraphPlotEffectiveImageWidth];

removeThickness[g_] := ReplaceAll[g, _Thickness | _AbsoluteThickness :> Sequence[]];

makeEndDiskPrimitives[indices_, styleName_, radius_] := Scope[
  primitives = ExpandPrimitives @ removeSingleton @ ExtractGraphPlotPrimitives[indices, "VertexPrimitives"];
  If[Length[primitives] =!= Length[indices], ReturnFailed[]];
  frameRadius = radius / $GraphPlotEffectiveImageWidth * 2;
  diskRadius = radius / $GraphPlotEffectiveImageWidth * $graphPlotWidth;
  isDisk = ConstantArray[False, Length @ indices];
  If[StringStartsQ[styleName, "Disk"|"Dot"], Part[isDisk, 1] = True];
  If[StringEndsQ[styleName, "Disk"|"Dot"], Part[isDisk, -1] = True];
  MapThread[makeEndDisk, {primitives, indices, isDisk}]
];

makeEndDisk[i_Inset, _, True] :=
  i /. g_Graphics :> SetFrameColor[g, {$highlightColor, absThickness[frameRadius]}];

makeEndDisk[i_Inset, _, False] := i;

makeEndDisk[other_, index_, True] :=
  Disk[Part[$VertexCoordinates, index], diskRadius];

makeEndDisk[_, _, _] := Nothing;

toRawPrimitive = Case[
  Style[s_, ___]    := % @ s;
  {p_}              := % @ p;
  e_                := e;
];

replaceWithColor[g_, c_, preserveArrowheads_:False] :=
  ReplaceAll[g, {
    t_Text :> t,
    If[preserveArrowheads, a_Arrowheads :> a, Nothing],
    Color3D[_] :> Color3D[c],
    Inset[z_Graphics, args__] :> Inset[SetFrameColor[z, c], args],
    $ColorPattern -> c
  }];

fadeProtectPrimitives[{old_, new_}] :=
  {old, Annotation[new, "Protected"]};

(* we simply delete matching edges, because we will redraw them possibly with adjustments *)
removeHighlightedPathEdges[{old_, new_}] := Scope[
  $firstRemovedArrowheads ^= FirstCase[old, _Arrowheads, None, Infinity];
  If[$cardinalFilter =!= All,
    $filteredEdges = {}; new //= saveAndTrimFilteredEdges;
    old = {old, $filteredEdges};
  ];
  AppendTo[$newEdges, new];
  old
];

(* we will delete matching vertices, but save them to be redrawn with the highlight color *)
removeHighlightedPathVertices[{old_, new_}] := (
  AppendTo[$newVertices, new];
  old
);

filteredArrowheadsQ[Arrowheads[list_List, ___]] :=
  AnyTrue[list, filteredArrowheadSpecQ];

filteredArrowheadSpecQ[{_, _, (Graphics|Graphics3D)[Annotation[_, card_, "Cardinal"], ___]}] /;
  !MemberQ[$cardinalFilter, card | Inverted[card]] := True;

saveAndTrimFilteredEdges[edges_] := Scope[
  edges /. Style[p_, l___, a_Arrowheads ? filteredArrowheadsQ, r___] :> {
    {aNonvis, aVis} = SelectDiscard[First @ a, filteredArrowheadSpecQ];
    AppendTo[$filteredEdges, Style[p, Transparent, l, Arrowheads @ aNonvis, r]];
    Style[p, l, Arrowheads @ aVis, r]
  }
];

(**************************************************************************************************)

setbackArrow[{}, _] := {};

setbackArrow[curve_, 0|0.] := Arrow @ curve;

setbackArrow[curve_, d_] := Scope[
  target = Last @ curve;
  curve = SetbackCoordinates[curve, {0, d}];
  If[curve === {}, Return @ {}];
  last = Last @ curve;
  Arrow @ Append[curve, last + Normalize[target - last] * 1*^-3]
];

setbackLine[curve_, 0|0.] := Line @ curve;

setbackLine[curve_, d_] := Line @ SetbackCoordinates[curve, {0, d}];

joinSegments[{}, _, _] := {};

joinSegments[segments_, adjustments_, shouldJoin_] := Scope[
  numSegments = Length @ segments;
  $offsetVector = 0; isLast = False;
  $segments = segments;
  lineBag = Bag[];
  VectorReplace[adjustments, {
    Rule[{z_, _}, {"Shrink", n_}] :> (Part[$segments, z] //= shrinkSegment[n * bendRange]),
    Rule[{z_, _}, {"Short", n_}] :> (Part[$segments, {z, z + 1}] //= shortSegment[n * bendRange])
  }];
  delta = None;
  Do[
    $i = i;
    isLast = i == numSegments;
    segment = PlusVector[$offsetVector] @ Part[$segments, i];
    applyMod @ Lookup[adjustments, i, None];
    If[shouldJoin,
      StuffBag[lineBag, If[i === 1, Identity, Rest] @ segment, 1],
      StuffBag[lineBag, segment]
    ];
  ,
    {i, 1, numSegments}
  ];
  BagPart[lineBag, All]
];

applyMod = Case[
  None := Null;
  {"Arrowhead", pos_} := (
    AppendTo[$extraArrowheads, {segment, pos}];
  );
  {"Bend", amount_, dtheta_:Automatic} /; !isLast := (
    nextSegment = Part[$segments, $i + 1];
    {segment, nextSegment} = applyBendBetween[segment, nextSegment, 1.5 * amount, dtheta];
    Part[$segments, $i + 1] = nextSegment;
  );
  {"Shoulder", amount_, dtheta_:Automatic} /; !isLast := (
    nextSegment = Part[$segments, $i + 1];
    {segment, nextSegment} = applyShoulderBetween[segment, nextSegment, 1.5 * amount, dtheta];
    Part[$segments, $i + 1] = nextSegment;
  );
  {"Offset", offset:{_, _}} := (
    $offsetVector += offset * bendRange;
  );
  {"Extend", amount_ ? Positive} := (
    {delta, segment} = extendSegment[segment, amount];
    $offsetVector += delta;
  );
  {"Extend", _ ? Negative} := (
    {delta, segment} = truncateSegment[segment, Abs @ Last @ mod];
    If[doArrow && isLast,
      (* this makes sure the arrowhead points at the original target *)
      AppendTo[segment, PointAlongLine[{Last[segment], Part[$segments, -1, -1]}, 1*^-3]]
    ];
    $offsetVector += delta
  );
  other_ := (Message[GraphRegionHighlight::badpadj, other]; Null)
];

shrinkSegment[d_][segment_] := Scope[
  {a, b} = FirstLast @ segment;
  mid = Avg[a, b];
  a2 = PointAlongLine[{a, mid}, d];
  b2 = PointAlongLine[{b, mid}, d];
  scaling = EuclideanDistance[mid, a2] / EuclideanDistance[mid, a];
  translated = PlusVector[segment, -mid];
  segment = PlusVector[translated * scaling, mid];
  Join[{a, a2}, segment, {b2, b}]
];

shrinkSegment[{d1_, d2_}][segment_] := Scope[
  {a, b} = FirstLast @ segment;
  mid = Mean @ segment;
  a2 = PointAlongLine[{a, mid}, d1];
  b2 = PointAlongLine[{b, mid}, d2];
  scaling1 = EuclideanDistance[mid, a2] / EuclideanDistance[mid, a];
  scaling2 = EuclideanDistance[mid, b2] / EuclideanDistance[mid, b];
  translated = PlusVector[segment, -mid];
  segment = MapThread[{t, p} |-> t * p + mid, {
    translated,
    Interpolated[scaling1, scaling2, Length @ translated]
  }];
  Join[{a, a2}, segment, {b2, b}]
];

shortSegment[d_][{segmentA_, segmentB_}] := Scope[
  If[ListQ[d], {d1, d2} = d, d1 = d2 = d];
  segmentA = SetbackCoordinates[segmentA, {0, d1}];
  segmentB = SetbackCoordinates[segmentB, {d2, 0}];
  AppendTo[segmentA, First @ segmentB];
  {segmentA, segmentB}
];

GraphRegionHighlight::badpadj = "PathAdjustments element `` is invalid.";

parseAdjustments = Case[
  z_Integer ? Negative -> other_                          := %[modLen[z] -> other];
  z_Integer -> "Arrowhead"                                := z -> {"Arrowhead", .5};
  z_Integer -> {"Arrowhead", pos_}                        := z -> {"Arrowhead", pos};
  z_Integer -> spec:{"Bend" | "Shoulder" | "Extend", ___}  := z -> spec;
  z_Integer -> spec:{"Offset", {_, _}}                    := modLen[z] -> spec;
  z:{__Integer} -> spec:{"Shrink" | "Short", ___}         := modLen[z] -> spec;
  z_ -> {"Expand", n_}                                    := %[z -> {"Shrink", -n}];
  z_ -> {"Shorten", n_}                                   := %[z -> {"Extend", -n}];
  z_ -> spec_String                                       := %[z -> {spec, 1}];
  other_ := (Message[GraphRegionHighlight::badpadj, other]; {})
];

modLen[z_] := Mod[z, numSegments + 1, 1];

(**************************************************************************************************)

applyBendBetween[segment1_, segment2_, d_, dtheta_] := Scope[
  {delta, truncated1} = truncateSegment[segment1, d];
  {delta, truncated2} = truncateSegment[Reverse @ segment2, d];
  p1 = Last @ truncated1;
  p2 = Last @ truncated2;
  q = Last @ segment1;
  d1 = p1 - q; d2 = p2 - q;
  r = Avg[Norm @ d1, Norm @ d2];
  a1 = ArcTan @@ d1; a2 = ArcTan @@ d2;
  If[dtheta === Automatic,
    While[a2 < a1, a2 += Tau]; as1 = DeleteDuplicates @ Append[a2] @ Range[a1, a2, Tau / 16];
    While[a1 < a2, a1 += Tau]; as2 = Reverse @ DeleteDuplicates @ Append[a1] @ Range[a2, a1, Tau / 16];
    as = MinimumBy[{as1, as2}, Length];
    circlePoints = AngleVector[q, {r, #}]& /@ as;
  ,
    as = Range[a1, a1 + dtheta, Sign[dtheta] * Tau/16];
    circlePoints = AngleVector[q, {r, #}]& /@ as;
    offset = Last[circlePoints] - q;
    offset = VectorReject[offset, Last[segment2] - q];
    $offsetVector ^= $offsetVector + offset;
  ];
  {
    Join[truncated1, circlePoints],
    Reverse @ truncated2
  }
];

(* so $offsetVector is a mess. we need to keep the *targets* -- the vertices, constant.
then reset the offsetvector whenever we hit a target. maybe?

maybe a better thign is that each vertex gets an entry point and a leave point. it is connected
in one of several ways: shoulder, or cicle-bend, or straight. then we can offset the entire length of the
segment perpendicular to its original direction.

a bezier curve plugs the gaps. you can also trim and extend a segment on either side.

ok here's a better idea: the edges are infinite lines basically. then there are 'joint circles' that are created
for each vertex. we wish to follow a chain of intersections between each element and the next. line to join to next line.
if a line skips the join circle, it will eventually hit the next line of course.

 *)

applyShoulderBetween[segment1_, segment2_, d_, dtheta_] := Scope[
  {delta, truncated1} = truncateSegment[segment1, d];
  {delta, truncated2} = truncateSegment[Reverse @ segment2, d];
  p1 = Last @ truncated1;
  p2 = Last @ truncated2;
  q = Last @ segment1;
  
  If[dtheta === Automatic,
    shoulderPoints = DiscretizeCurve[{truncated1, q, Reverse @ truncated2}]
  ,
    d1 = p1 - q; d2 = p2 - q;
    r = Avg[Norm @ d1, Norm @ d2];
    a1 = ArcTan @@ d1;
    shoulderPoints = DiscretizeCurve[{p1, q, AngleVector[q, {r, a1 + dtheta}]}];
    offset = Last[shoulderPoints] - q;
    offset = VectorReject[offset, Last[segment2] - q];
    $offsetVector ^= $offsetVector + offset;
  ];
  {
    Join[truncated1, shoulderPoints],
    Reverse @ truncated2
  }
];

scaleSegment[coords_, n_] := Scope[
  {first, last} = FirstLast @ segment;
  translated = PlusVector[segment, -first];
  dist = EuclideanDistance[first, last];
  margin = n * bendRange * If[isLast, 2, 1.5];
  scaling = (dist + margin) / dist;
  PlusVector[translated * scaling, first]
];

finalDelta[coords_, n_] := Normalize[Part[coords, -1] - Part[coords, -2]] * bendRange * n;

extendSegment[coords_, n_] := Scope[
  delta = finalDelta[coords, n];
  {delta, MapAt[PlusOperator[delta], coords, -1]}
];

truncateSegment[coords_, n_] := Scope[
  delta = finalDelta[coords, -n];
  coords = SetbackCoordinates[coords, {0, bendRange * n}];
  {delta, coords}
];

(**************************************************************************************************)

subgraphCoveringGraphics[r_, vertices_, edgeIndices_, edgeList_, vertexCoords_, edgeCoordsLists_] := Scope[
  vertexPoints = Part[vertexCoords, vertices];
  center = Mean[vertexPoints];
  radius = Max[SquaredDistanceMatrix[vertexPoints, {center}]];
  externalCoords = Part[vertexCoords, Complement[Range @ Length @ vertexCoords, vertices]];
  If[$simplifyRegions && externalCoords =!= {} && (other = Min[SquaredDistanceMatrix[externalCoords, {center}]]) > radius,
    dr = (other - radius) / If[$GraphIs3D, 32, 3];
    Return @ If[$GraphIs3D, Sphere, Disk][center, Sqrt[radius + dr]];
  ];
  edgePoints = DeleteDuplicates @ Catenate @ Map[edgeSpaced] @ Part[edgeCoordsLists, edgeIndices];
  points = ToPacked @ Join[vertexPoints, edgePoints];
  primitives = PointDilationGraphics[points, r];
  If[ContainsQ[primitives, Polygon[_Rule]],
    primitives = primitives /. p:Polygon[_Rule] :> removeTrivialHoles[p, externalCoords];
  ];
  Style[primitives, EdgeForm[None]]
];

containsAnyPointsQ[coords_, points_] := Scope[
  bbox = CoordinateBounds[coords];
  points = Select[points, VectorBetween[bbox]];
  If[points === {}, False,
    memberFunc = RegionMember @ ConvexHullMesh @ coords;
    AnyTrue[points, memberFunc]
  ]
];

removeTrivialHoles[Polygon[coords_ -> holes_], ext2_] := Scope[
  bbox = CoordinateBounds[coords];
  ext = Select[ext2, VectorBetween[bbox]];
  makePolygon[coords, Select[holes, hole |-> containsAnyPointsQ[hole, ext]]]
];

makePolygon[coords_, {}] := Polygon[coords];
makePolygon[coords_, holes_] := Polygon[coords -> holes];

edgeSpaced[{a_, b_}] := Table[i * a + (1-i) * b, {i, .125, .875, .125}];
edgeSpaced[list_List] := Mean[list];
