(* declarePlusTimesDispatch[symbol_Symbol, test_, dispatch_] := (
  symbol /: Plus[objects:Repeated[_symbol ? test, {2, Infinity}]] := dispatch[Plus][objects];
  symbol /: Times[scalar_ /; NumericQ[Unevaluated @ scalar], object_symbol ? test] := dispatch[Times][scalar, object];
  symbol /: Times[object1_symbol ? test, object2_symbol ? test] := dispatch[Times][object1, object2];
)

DefineSimpleMacro[UnpackPathAlgebra,
UnpackPathAlgebra[args___] :> UnpackAssociation[getObjectData @ $PathAlgebra, args]
];

$directionStrings = {"Forward", "Backward", "Symmetric", "Antisymmetric"};

(**************************************************************************************************)

PublicVariable[$PathAlgebra]

SetUsage @ "
$PathAlgebra is the PathAlgebra[$$] used as the default algebra for PathVector and PathElement objects.
"

$PathAlgebra::notset = "$PathAlgebra is not set."
$PathAlgebra = None;
$PathAlgebraQ := If[$PathAlgebra === None, Message[$PathAlgebra::notset]; False, True];
$FieldPlus := $PathAlgebra["FieldPlus"];
$FieldTimes := $PathAlgebra["FieldTimes"];

(**************************************************************************************************)

PublicFunction[PathAlgebra]

SetUsage @ "
PathAlgebra[quiver$, field$] constructs a PathAlgebra[$$] from a given cardinal quiver.
PathAlgebra['lattice$', field$] uses a named lattice quiver as the quiver.
PathAlgebra[spec$] uses %Integers as the field.
* field$ can be an integer for a finite field (or ring), or one of %Integers, %Reals, or %Complexes.
"

Options[PathAlgebra] = {
  PlotRange -> Automatic,
  EdgeSetback -> 0.25,
  ImageSize -> 100,
  VertexSize -> Automatic,
  ArrowheadSize -> 0.13,
  EdgeThickness -> 1,
  "SymbolicEdgeThickness" -> Automatic,
  "SymbolicEdgeStyle" -> Automatic,
  "VertexNudge" -> {0, 0},
  "EdgeNudge" -> {0, 0},
  "DrawSortOrder" -> Inverted["PathLength"]
};

$graphOrLatticeSpec = _Graph | _Str | {_Str, __};

PathAlgebra[quiver:$graphOrLatticeSpec, opts:OptionsPattern[]] ? HoldEntryQ =
  PathAlgebra[quiver, Integers, opts];

PathAlgebra[quiver:$graphOrLatticeSpec, field_, OptionsPattern[]] ? HoldEntryQ := Scope[

  If[MatchQ[quiver, _Str | {_Str, __}],
    quiver = LatticeQuiver[quiver]];
  If[!QuiverQ[quiver], ReturnFailed[]];

  If[!MatchQ[field, _Int | Integers | Reals | Complexes], ReturnFailed[]];

  UnpackOptions[plotRange, edgeSetback, imageSize, vertexSize, arrowheadSize,
    edgeThickness, symbolicEdgeThickness, symbolicEdgeStyle,
    vertexNudge, edgeNudge, drawSortOrder];
  If[NumericQ[plotRange], plotRange *= {{-1, 1}, {-1, 1}}];

  data = <||>;
  {vertexCoords, edgeCoords} = ExtractGraphPrimitiveCoordinates @ quiver;

  data["Quiver"] = quiver;
  data["IndexQuiver"] = ToIndexGraph @ quiver;
  data["VertexList"] = vertexList = VertexList @ quiver;

  data["VertexCoordinates"] = vertexCoords;
  data["EdgeCoordinateLists"] = edgeCoords;
  data["EdgeCoordinates"] = edgeCoords;
  data["PlotRange"] = SubAuto[plotRange, CoordinateBounds[vertexCoords]];
  data["EdgeSetback"] = edgeSetback;
  data["ImageSize"] = imageSize;
  data["VertexSize"] = vertexSize;
  data["ArrowheadSize"] = arrowheadSize;
  data["EdgeThickness"] = edgeThickness;
  data["SymbolicEdgeThickness"] = symbolicEdgeThickness;
  data["SymbolicEdgeStyle"] = symbolicEdgeStyle;
  data["VertexNudge"] = vertexNudge;
  data["EdgeNudge"] = edgeNudge;
  data["DrawSortOrder"] = drawSortOrder;

  data["Field"] = field;
  data["FieldModulus"] = mod = Match[field, n_Int :> n, Infinity];
  data["FieldPlus"] = PlusModOperator[mod];
  data["FieldTimes"] = TimesModOperator[mod];
  data["FieldMinus"] = MinusModOperator[mod];
  data["FieldSubtract"] = SubtractModOperator[mod];

  {pos, neg} = Match[field, n_Int :> {1, n-1}, {1, -1}];
  data["Pos"] = pos;
  data["Neg"] = neg;
  data["RandomFieldElement"] = Match[field, n_Int :> Fn[RandomInteger[n-1]], RandomInteger[{-2, 2}]&];

  data["FieldColorFunction"] = If[IntegerQ[mod],
    AssociationThread[Range[0, mod-1], fieldColors @ mod],
    $complexColorFunction
  ];

  data["VertexCount"] = vertexCount = VertexCount @ quiver;
  data["EdgeCount"] = edgeCount = EdgeCount @ quiver;
  data["VertexRange"] = vertexRange = Range @ vertexCount;
  data["EdgeRange"] = edgeRange = Range @ edgeCount;
  data["Cardinals"] = CardinalList @ quiver;

  data["VertexDegreeList"] = vertexDegrees = VertexDegree @ quiver;
  origin = LookupExtendedOption[quiver, GraphOrigin];
  If[origin =!= None,
    origin = IndexOf[vertexList, origin];
    data["Origin"] = origin;
    data["OriginDegree"] = originDegree = Part[vertexDegrees, origin];
    data["DegreePerfectVertices"] = SelectIndices[vertexDegrees, EqualTo[originDegree]];
    data["DegreeDefectiveVertices"] = SelectIndices[vertexDegrees, UnequalTo[originDegree]];
  ,
    data["Origin"] = None;
    data["OriginDegree"] = Indeterminate;
    data["DegreePerfectVertices"] = vertexRange;
    data["DegreeDefectiveVertices"] = {};
  ];


  ct = Lookup[LookupAnnotation[quiver, EdgeAnnotations, <||>], "CardinalTransitions", <||>];
  data["CardinalTransitions"] = Join[ct, KeyMap[Inverted] @ MatrixMap[reverseTransition, ct]];

  pairs = EdgePairs @ quiver;
  tailVertices = Col1[pairs];
  headVertices = Col2[pairs];
  tailAssoc = AssociationThread[edgeRange, tailVertices];
  headAssoc = AssociationThread[edgeRange, headVertices];

  data["EdgeTails"] = tailVertices;
  data["EdgeHeads"] = headVertices;
  data["EdgeToTail"] = Join[tailAssoc, KeyMap[Inverted, headAssoc]];
  data["EdgeToHead"] = Join[headAssoc, KeyMap[Inverted, tailAssoc]];

  data["NullVertex"] = nullVertex = vertexCount + 1;
  tagOutTable = Append[nullVertex] /@ TagVertexOutTable[quiver, nullVertex];
  data["TagOutTable"] = tagOutTable;

  data["NullEdge"] = nullEdge = edgeCount + 1;
  tagOutEdgeTable = Append[nullEdge] /@ TagVertexAdjacentEdgeTable[quiver, nullEdge, Signed -> True];
  data["TagOutEdgeTable"] = tagOutEdgeTable;

  data["OutEdgeTable"] = VertexAdjacentEdgeTable[quiver, Signed -> True];

  data["VertexTags"] = VertexTagTable @ quiver;

  edgeTags = EdgeTags @ quiver;
  edgeToCard = AssociationThread[edgeRange, edgeTags];
  data["EdgeToCardinal"] = Join[edgeToCard, KeyMap[Inverted] @ Map[Inverted] @ edgeToCard];

  ConstructNoEntry[PathAlgebra, data]
];

$complexColorFunction = ComplexHue;

reverseTransition[a_ -> Inverted[b_]] := b -> Inverted[a];
reverseTransition[a_ -> b_] := b -> a;

(**************************************************************************************************)

PublicFunction[PathAlgebra]

DefineObjectPropertyDispatch[PathAlgebra, pathAlgebraProperty];

MakeBoxes[pa_PathAlgebra ? HoldNoEntryQ, form_] :=
  PathAlgebraBoxes[pa, form];

PathAlgebraBoxes[object:PathAlgebra[data_], form_] := Scope[
  UnpackAssociation[data, vertexCount, field, cardinals];
  BoxForm`ArrangeSummaryBox[
    PathAlgebra, object, None,
    (* Always displayed *)
    {
     {summaryItem["Vertices", vertexCount], SpanFromLeft},
     {summaryItem["Cardinals", Row[cardinals, ","]]},
     {summaryItem["Field", If[IntegerQ[field], Subscript["\[DoubleStruckCapitalF]", field], field]]}
     },
    (* Displayed on request *)
    {},
    form,
    "Interpretable" -> Automatic
  ]
];

(**************************************************************************************************)

PublicHead[PathVector]

SetUsage @ "
PathVector[$$] represents a path vector on a cardinal quiver.
"

declarePlusTimesDispatch[PathVector, PathVectorQ, $pathVectorDispatch]

$pathVectorDispatch = <|
  Plus -> PathVectorPlus,
  Times -> PathVectorTimes
|>;

PublicFunction[PathVectorPlus, PathVectorTimes]

NumericOrSymbolicQ[a_] := NumericQ[a] || MatchQ[a, _Subscript];

PathVectorPlus[__] := $Failed;
PathVectorPlus[v__PathVector] := PathVectorElementwise[Apply @ $FieldPlus, 0, {v}];

PathVectorTimes[__] := $Failed;
PathVectorTimes[v__PathVector] := PathVectorElementwise[Apply @ $FieldTimes, 0, {v}];
PathVectorTimes[n_ ? NumericOrSymbolicQ, p_PathVector] := If[n === 1 || n === 1.0, p, With[{t = $FieldTimes}, PathVectorMap[t[n, #]&, p]]];

PathVectorMap[f_, PathVector[assoc_]] :=
  PathVector @ Map[f, assoc];

PathVectorElementwise[_, _, {vec_}] := vec;

PathVectorElementwise[f_, id_, vecs_] := Scope[
  assocs = Col1[vecs];
  constructPathVector @ Merge[KeyUnion[assocs, id&], f]
];

mergeWeights[rules_, id_, f_] :=
  Merge[KeyUnion[Flatten @ rules, id&], f];

constructPathVector[list_List] := constructPathVector @ Assoc @ list;

rulesToPathVector[list_List] := constructPathVector @ Merge[Flatten @ list, Apply @ $FieldPlus];

constructPathVector[assoc_Assoc] :=
  PathVector @ Assoc @ KeySort @ KeyDrop[NullElement] @ DeleteCases[assoc, 0|0.];

(**************************************************************************************************)

PublicFunction[PathVectorElements]

SetUsage @ "PathVectorElements[path$] returns the %PathElements present in a %PathVector.
"

PathVectorElements[PathVector[assoc_]] := Keys @ assoc;

(**************************************************************************************************)

PublicFunction[PathVectorQ]

SetUsage @ "
PathVectorQ[expr$] gives True if expr$ is a valid PathVector.
"

PathVectorQ[PathVector[_Assoc]] := $PathAlgebraQ;
PathVectorQ[_] := False;


PathVectorVectorQ[{PathVector[_Assoc]...}] := $PathAlgebraQ;
PathVectorVectorQ[_] := False;

(**************************************************************************************************)

SetHoldAllComplete[ValidPathAssociationQ];

ValidPathAssociationQ[assoc_Assoc] /; AssocQ[Unevaluated[assoc]] :=
  MatchQ[Keys @ assoc, {___PathElement}];

ValidPathAssociationQ[_] := False;

declareFormatting[
  pv:PathVector[_Assoc ? ValidPathAssociationQ] /; $PathAlgebraQ :>
    formatPathVector[pv]
];


(**************************************************************************************************)

PublicFunction[PathVectorPlot]

PathVectorPlot[pv:PathVector[_Assoc ? ValidPathAssociationQ], opts___Rule] /; $PathAlgebraQ :=
  formatPathVector[pv, opts];

(**************************************************************************************************)

notOverlappingPathsQ[PathVector[assoc_]] := Scope[
  edges = Col2[Keys @ assoc];
  DuplicateFreeQ @ Catenate @ Ma[edges /. Inverted[e_] :> e, 1]
]

pathFieldQ[PathVector[assoc_]] := DuplicateFreeQ @ PathTailVertex @ Keys[assoc];

notBothwaysQ[_] := False;

formatPathVector[pv_, opts___] := If[notOverlappingPathsQ[pv] && pathFieldQ[pv],
  iFormatPathVector[pv, False, opts],
  Mouseover[
    iFormatPathVector[pv, False, opts],
    iFormatPathVector[pv, True, opts]
  ]
]

formatPathVector[pv_] :=
  iFormatPathVector[pv, False];

iFormatPathVector[PathVector[paths_Assoc], transparency_, opts___Rule] := Scope[
  UnpackPathAlgebra[
    vertexCoordinates, edgeCoordinateLists, plotRange, fieldColorFunction,
    vertexRange, vertexList, vertexSize, edgeSetback, imageSize, edgeToCardinal,
    arrowheadSize, edgeThickness,
    symbolicEdgeThickness, symbolicEdgeStyle,
    vertexNudge, edgeNudge, drawSortOrder
  ];
  $sb = edgeSetback; $transparency = transparency; $arrowheadSize = arrowheadSize;
  SetAuto[symbolicEdgeThickness, edgeThickness];
  SetAuto[symbolicEdgeStyle, {}];
  pathRules = Normal @ paths;
  pathRules = Values @ GroupBy[pathRules, canonPathElement, combineReversed];
  drawSorter = toDrawSorter @ drawSortOrder;
  If[drawSorter =!= None, pathRules = SortBy[pathRules, drawSorter]];
  pathPrimitives = drawWeightedElement @@@ pathRules;
  pathPrimitives = {
    AbsoluteThickness[edgeThickness],
    AbsolutePointSize[Replace[vertexSize, Automatic -> 4]],
    pathPrimitives
  };
  initialPathVertices = Cases[pathRules, PathElement[t_, _, _] :> t];
  remainingVertices = Complement[vertexRange, initialPathVertices];
  vertexPrimitives = {
    AbsolutePointSize[Replace[vertexSize, Automatic -> 3]], GrayLevel[0.9],
    Point @ Part[vertexCoordinates, remainingVertices]
  };
  primitives = {vertexPrimitives, pathPrimitives};
  primitives = doNudge[primitives, vertexNudge, edgeNudge];
  Graphics[
    primitives, opts,
    Frame -> True, FrameTicks -> False, ImageSize -> imageSize,
    FrameStyle -> LightGray,
    PlotRange -> plotRange, PlotRangePadding -> Scaled[0.15],
    PlotRangeClipping -> True
  ]
];

numericOrVectorQ[_ ? NumericQ | {_ ? NumericQ, _ ? NumericQ}] := True;
numericOrVectorQ[_] := False;

toDrawSorter = Case[
  "PathLength"    := P1 /* PathLength;
  "SymbolicQ"     := PN /* numericOrVectorQ /* Not;
  "NumericQ"      := PN /* numericOrVectorQ;
  Inverted[e_]     := (% @ e) /* Not;
  list_List       := ApplyThrough[% /@ list];
  None            := None;
];

doNudge[g_, {0, 0}, {0, 0}] := g;
doNudge[g_, vertexNudge_, _] :=
  g /. Point[p_] :> If[CoordinateVectorQ[p],
    Point[Offset[nudge, p]],
    Point[Offset[nudge, #]]& /@ p
  ];

canonPathElement[p:PathElement[t_, edges_, h_] -> w_] :=
  If[t < h, PathElement[h, InvertReverse @ edges, t], p];

combineReversed[{r_Rule}] := r;
combineReversed[{a_ -> wf_, b_ -> wb_}] := ReversedGroup[a, b] -> {wf, wb};

PathLength[ReversedGroup[a_, b_]] := PathLength[a];

pathElementForm[PathElement[t_, e_, h_]] :=
  PathWordForm[
    Part[vertexList, t],
    Lookup[edgeToCardinal, e],
    Part[vertexList, h]
  ];

$reverseColor = None;
drawWeightedElement[ReversedGroup[p_, q_], {wf_, wb_}] := Block[
  {$reverseColor = fieldColorFunction @ wb},
  drawWeightedElement[p, wf]
];

drawWeightedElement[p:PathElement[t_, e_, h_], weight_] := Scope[
  isNumeric = NumericQ[weight];
  graphics = drawStyledPath[
    If[e === {},
      Part[vertexCoordinates, List @ t],
      getEdgeCoords[e]
    ],
    If[isNumeric, fieldColorFunction @ weight, $Teal]
  ];
  If[!isNumeric, graphics //= StyleOperator[AbsoluteThickness[symbolicEdgeThickness], symbolicEdgeStyle]];
  NiceTooltip[graphics, Column[{pathElementForm[p], weight}]]
];

getEdgeCoords[e_] := Scope[
  coords = Part[edgeCoordinateLists, StripInverted /@ e];
  segments = MapIndices[Rev, SelectIndices[e, InvertedQ], coords];
  If[Len[segments] > 1,
    $n = Len[segments]; $sb = LineLength[P1 @ segments] / 3.5;
    segments //= MapIndexed[setbackSegment];
  ];
  Flatten[segments, 1] //. {l___, a_List, a_List, r___} :> {l, a, r}
];

setbackSegment[a_List, {1}] := SetbackCoordinates[a, {0, $sb}];
setbackSegment[a_List, {n_} /; n === $n] := SetbackCoordinates[a, {$sb, 0}];
setbackSegment[a_List, _] := SetbackCoordinates[a, {$sb, $sb}];

(* mergeSegments[{segment_}] := segment;
mergeSegments[segments_List] :=
  ApplyWindowed[segmentJoin, segments]

segmentJoin[a_, b_] := If[Abs[segmentAngle[a] - segmentAngle[b]] > 0.01,
  Echo @ applyBendBetween[a, b, 0.1],
  a
];
 *)
segmentAngle[segment_] := ArcTan @@ (PN[segment] - P1[segment]);

drawStyledPath[vertices_, style_] /; $transparency :=
  Mouseover[
    drawPathPrimitives[vertices, Opacity[.1, style]],
    drawPathPrimitives[vertices, style]
  ];

drawStyledPath[vertices_, style_] :=
  drawPathPrimitives[vertices, style]

drawPathPrimitives[{} | Nothing, _] :=
  {};

drawPathPrimitives[{vertices_}, style_] :=
  Style[Point @ vertices, style];

drawPathPrimitives[vertices_, style_] /; $reverseColor =!= None := Scope[
  opacity = ExtractFirstOpacity @ style;
  Style[
    Arrow[
      Line[
        SetbackCoordinates[vertices, {1,1} * .5 * $sb],
        VertexColors -> SetColorOpacity[
          lineColorRange[$reverseColor, style, Len @ vertices],
          opacity
        ]
      ],
      {.05, .05}
    ],
    Arrowheads @ {
      {$arrowheadSize, 1, ArrowheadData["Line", SetColorOpacity[style, opacity]]},
      {-$arrowheadSize, 0, ArrowheadData["Line", SetColorOpacity[$reverseColor, opacity]]}
    }
  ]
];

lineColorRange[a_, b_, 2] := {a, b};
lineColorRange[a_, b_, n_] :=
  Join[Repeat[a, Floor[n / 2]], Repeat[b, Ceiling[n / 2]]];

drawPathPrimitives[vertices_, style_] :=
  Style[
    {Point @ F[vertices],
     Arrow @ SetbackCoordinates[vertices, {0, $sb}]}, style,
    Arrowheads[{{$arrowheadSize, 1, ArrowheadData["Line", style]}}]
  ]

fieldColors = MatchValues[
  2 := {$Gray, Black};
  3 := {$Gray, $Red, $Blue};
  4 := {$Gray, $Red, $Purple, $Blue};
  5 := {$Gray, $Red, $DarkRed, $DarkBlue, $Blue};
  n_ := Take[Prepend[$ColorPalette, White], n];
];

(**************************************************************************************************)

PublicFunction[PathHeadVertex, PathTailVertex]

PathTailVertex[p_PathElement] := P1[p];
PathTailVertex[list_List] := Col1[list];

PathHeadVertex[p_PathElement] := P3[p];
PathHeadVertex[list_List] := Col3[list];

(**************************************************************************************************)

PublicFunction[PathHeadVector, PathTailVector]

PathHeadVector[PathElement[_, _, h_]] := PathElement[h, {}, h];

PathHeadVector[PathVector[assoc_]] :=
  rulesToPathVector @ MapAt[PathHeadVector, Normal @ assoc, {All, 1}];

PathTailVector[PathElement[t_, _, _]] := PathElement[t, {}, t];

PathTailVector[PathVector[assoc_]] :=
  rulesToPathVector @ MapAt[PathTailVector, Normal @ assoc, {All, 1}];

(**************************************************************************************************)

PathVector /: Proportion[v__PathVector] := PathCompose[v];

(**************************************************************************************************)

PublicFunction[PathCompose]

(* defined on vectors by bilinearity *)

PathCompose[a_PathVector, b_PathVector] :=
  BilinearApply[elementCompose, a -> PathHeadVertex, b -> PathTailVertex];

(* defined on elements *)

PathCompose[a_PathElement, b_PathElement] :=
  elementCompose[a, b];

(* non-binary versions *)

PathCompose[e_] := e;

PathCompose[a_PathVector, b_PathVector, c_PathVector, rest___PathVector] /; $PathAlgebraQ :=
  PathCompose[PathCompose[PathCompose[a, b], c], rest];

(**************************************************************************************************)

elementCompose[_, NullElement] := NullElement;

elementCompose[NullElement, _] := NullElement;

elementCompose[_PathElement, _PathElement] :=
  NullElement;

elementCompose[PathElement[t_, e1_, m_], PathElement[m_, e2_, h_]] :=
  PathElement[t, Join[e1, e2] //. $cancelEdgeRules, h];

$cancelEdgeRules = Dispatch @ {
  {l___, i_, Inverted[i_], r___} :> {l, r},
  {l___, Inverted[i_], i_, r___} :> {l, r}
};

(**************************************************************************************************)

PublicFunction[ToPathVector]

SetUsage @ "
ToPathVector[region$] constructs a %PathVector based on the given graph region.
* region$ should be a path region.
"

ToPathVector[region_] /; $PathAlgebraQ := Scope[
  UnpackPathAlgebra[quiver];
  regionData = GraphRegion[quiver, attachWeightedData @ region];
  $w = 1;
  rules = extractWeightedPaths @ regionData;
  rulesToPathVector @ rules
];

extractWeightedPaths = Case[
  list_List :=
    Map[%, list];
  GraphPathData[vertices_, edges_, inversions_] :=
    attachPathElementWeight[
      PathElement[
        P1 @ vertices,
        MapIndices[Inverted, inversions, edges],
        PN @ vertices
      ],
      $w
    ];
  GraphRegionAnnotation[r_, <|"Weight" -> w_|>] :=
    Block[{$w = w}, % @ r];
  _ := {};
];

attachWeightedData = Case[
  r_ -> w_    := Weighted[r, w];
  list_List   := Map[%, list];
  other_      := other
];

attachPathElementWeight[pe_, w_] :=
  pe -> w;

attachPathElementWeight[pe_, c_Complex] := Scope[
  w = Abs[c];
  Switch[Arg[c],
    Pi/2,   PathReverse[pe] -> -w,
    -Pi/2,  PathReverse[pe] -> w,
    0,      pe -> w,
    Pi,     pe -> -w
  ]
];


(**************************************************************************************************)

PublicFunction[BasisWordVectors]

SetUsage @ "
BasisWordVectors[] returns a list of %WordVector[c$i] for each cardinal c$i in the quiver.
"

BasisWordVectors[] /; $PathAlgebraQ := Scope[
  UnpackPathAlgebra[cardinals];
  WordVector /@ cardinals
];

DefineFunctionAutocomplete[BasisWordVectors, {$directionStrings}];

BasisWordVectors[type_Str] /; $PathAlgebraQ := Scope[
  f = BasisWordVectors[];
  b = PathReverse /@ f;
  Switch[type,
    "Forward", f,
    "Backward", b,
    "Symmetric", f + b,
    "Antisymmetric", f - b,
    _, $Failed
  ]
];

(**************************************************************************************************)

PublicFunction[WordVector]

SetUsage @ "
WordVector['word$'] constructs a %PathVector consisting of all paths that have path word word$.
WordVector['word$' -> w$] uses weight w$ for the basis paths.
WordVector[{spec$1, spec$2, $$}] constructs a sum of word vectors.
WordVector[spec$, type$] constructs a vector of a specific type.
* The default weight is 1.
* 'word$' should consist of cardinals, or their inversions (indicated by uppercase letters).
* type$ can be one of 'Forward', 'Backward', 'Symmetric', and 'Antisymmetric' |
"

DefineFunctionAutocomplete[WordVector, {0, $directionStrings}];

WordVector[spec_, All] := WordVector[spec, #]& /@ $directionStrings;

WordVector[words_List] :=
  PathVectorPlus @@ Map[WordVector, words];

WordVector[spec_, type_Str] :=
  makeTypeVector[WordVector, WordVector[spec], type];

WordVector[word_Str -> n_] :=
  PathVectorTimes[n, WordVector[word]];

General::badpathword = "`` is not a valid path word, which can contain cardinals ``."

WordVector[word_Str] /; $PathAlgebraQ := Scope[

  UnpackPathAlgebra[vertexRange, tagOutTable, tagOutEdgeTable, nullVertex, nullEdge, cardinals];

  word = ToPathWord[word, cardinals, ReturnFailed["badpathword", word, cardinals]];
  headVertices = tailVertices = vertexRange;

  CollectTo[{pathEdges},
  Scan[card |-> (
      edgeList = Part[tagOutEdgeTable @ card, headVertices];
      headVertices = Part[tagOutTable @ card, headVertices];
      StuffBag[pathEdges, edgeList]
    ),
    word
  ]];

  pathEdges = If[pathEdges === {},
    Repeat[{}, Len @ vertexRange],
    Transpose @ pathEdges
  ];

  pathElements = MapThread[toWordVectorPathElement, {tailVertices, pathEdges, headVertices}];

  PathVector @ ConstantAssociation[Sort @ pathElements, 1]
]

toWordVectorPathElement[tail_, edges_, head_] :=
  If[FreeQ[edges, nullEdge], PathElement[tail, edges, head], Nothing];

(**************************************************************************************************)

PublicFunction[PathSplit]

PathSplit[PathVector[assoc_]] := Scope[
  UnpackPathAlgebra[edgeToTail, edgeToHead];
  rulesToPathVector @ KeyValueMap[splitPathElement, assoc]
];

splitPathElement[p:PathElement[_, {}, _], w_] :=
  p -> w;

splitPathElement[PathElement[_, edges_, _], w_] :=
  Map[singleEdgePathElement[#] -> w&, edges];

(**************************************************************************************************)

PublicFunction[PathVectorWeights]

PathVectorWeights[list_List] :=
  Map[PathVectorWeights, list];

PathVectorWeights[PathVector[assoc_]] :=
  Values @ assoc;

PathVectorWeights[PathVector[assoc_], i_] :=
  If[1 <= Abs[i] <= Len[assoc], Part[assoc, i], 0];

PathVectorWeights[list_List, i_] :=
  PathVectorWeights[#, i]& /@ list;

(**************************************************************************************************)

PublicFunction[IgnoreDefectiveDegree]

SetUsage @ "
IgnoreDefectiveDegree is an option to VertexFieldEquationSolve and FindPathVectorZeros.
"

PublicFunction[RHSConstant]

SetUsage @ "
RHSConstant is an option to VertexFieldEquationSolve and FindPathVectorZeros.
"

PublicFunction[BoundaryConditions]

SetUsage @ "
BoundaryConditions is an option to VertexFieldEquationSolve.
"

(**************************************************************************************************)

PublicFunction[FindPathVectorZeros]

Options[FindPathVectorZeros] = {
  Modulus -> Inherited,
  IgnoreDefectiveDegree -> False,
  RHSConstant -> 0,
  MaxItems -> 10
};

FindPathVectorZeros[pv_, OptionsPattern[]] := Scope[
  UnpackPathAlgebra[field, degreePerfectVertices];
  UnpackOptions[modulus, ignoreDefectiveDegree, rHSConstant, maxItems];
  SetInherited[modulus, field];
  weights = PathVectorWeights[pv];
  vars = DeleteDuplicates @ DeepCases[weights, _Subscript];
  If[ignoreDefectiveDegree,
    parter = PartOperator[degreePerfectVertices];
    weights = If[ListQ[pv], Flatten @ Map[parter, weights], parter @ weights]];
  equations = Thread[weights == rHSConstant];
  FindInstance[Evaluate @ equations, vars, Automatic, maxItems, Modulus -> modulus]
]

(**************************************************************************************************)

PublicFunction[SubstituteSymbolicWeights]

SubstituteSymbolicWeights[PathVector[assoc_], subs:{__Rule}] :=
  constructPathVector @ AssociationThread[Keys @ assoc, ReplaceAll[Values @ assoc, subs]]

SubstituteSymbolicWeights[pv_PathVector, subs:{___List}] :=
  SubstituteSymbolicWeights[pv, #]& /@ subs;


(**************************************************************************************************)

PublicFunction[SetPathElementWeights]

SetPathElementWeights[PathVector[assoc_], rules_] :=
  constructPathVector @ Join[assoc, rules];

SetPathElementWeights[rules_][vector_] :=
  SetPathElementWeights[vector, rules];

(**************************************************************************************************)

PublicFunction[VertexFieldWeights]

VertexFieldWeights[PathVector[assoc_]] /; $PathAlgebraQ := Scope[
  UnpackPathAlgebra[vertexCount];
  weightVector = Repeat[0, vertexCount];
  KeyValueScan[setVertexFieldWeight, assoc];
  weightVector
]

setVertexFieldWeight[PathElement[v_, {}, _], w_] := Part[weightVector, v] = w;
setVertexFieldWeight[_, _] := Null;

(**************************************************************************************************)

PublicFunction[SetVertexFieldWeights]

SetVertexFieldWeights[PathVector[assoc_], rules:(_Rule | {___Rule})] :=
  constructPathVector @ Append[assoc, MapAt[EmptyPathElement, ToList @ rules, {All, 1}]];

SetVertexFieldWeights[spec_][vector_] := SetVertexFieldWeights[vector, spec];

(**************************************************************************************************)

PublicFunction[VertexFieldEquationSolve]

Options[VertexFieldEquationSolve] = Join[
  {BoundaryConditions -> {}},
  Options @ FindPathVectorZeros
];

VertexFieldEquationSolve::funcmsg = "Function issued messages when applied to a symbolic vertex field."
VertexFieldEquationSolve::funcres = "Function did not return a vertex field."

VertexFieldEquationSolve[f_, opts:OptionsPattern[]] := Scope[
  UnpackOptions[boundaryConditions];
  UnpackPathAlgebra[degreeDefectiveVertices, origin];
  sym = SymbolicVertexField[];
  {boundaryValue, originValue} = Lookup[boundaryConditions, {"Boundary", "Origin"}, None];
  If[boundaryValue =!= None && degreeDefectiveVertices =!= {},
    sym //= SetVertexFieldWeights[Thread[degreeDefectiveVertices -> boundaryValue]]];
  If[originValue =!= None,
    sym //= SetVertexFieldWeights[origin -> originValue]];
  If[ListQ[f], f //= ApplyThrough];
  res = Check[Construct[f, sym], $Failed];
  If[FailureQ[res], ReturnFailed["funcmsg"]];
  If[!PathVectorQ[res] && !VectorQ[res, PathVectorQ], ReturnFailed["funcres"]];
  zeros = FindPathVectorZeros[res, FilterOptions @ opts];
  SubstituteSymbolicWeights[sym, zeros]
]

(**************************************************************************************************)

PublicFunction[ConstantVertexField]

ConstantVertexField[n_:1] := PathVectorTimes[n, VertexField[]];

(**************************************************************************************************)

PublicFunction[PathVectorComponents]

PathVectorComponents[PathVector[assoc_]] :=
  PathVector @ Assoc[#]& /@ Normal[assoc]

(**************************************************************************************************)

PublicFunction[ReversalSymmetryDecompose]

SetUsage @ "
ReversalSymmetryDecompose[vector$] returns {s$, a$} where s$ is the reversal-symmetric part of vector$ \
and a$ is the antisymmetric part.
"

ReversalSymmetryDecompose[v_PathVector] := Scope[
  forward = v * (1/2);
  reverse = PathReverse @ forward;
  {forward + reverse, forward - reverse}
];

(**************************************************************************************************)

PublicFunction[ReversalSymmetricPart]

ReversalSymmetricPart[forward_PathVector] := Scope[
  reverse = PathReverse @ forward;
  forward + reverse
];

PublicFunction[ReversalAntisymmetricPart]

ReversalAntisymmetricPart[forward_PathVector] := Scope[
  reverse = PathReverse @ forward;
  forward - reverse
];

(**************************************************************************************************)

PublicFunction[PathLieDerivative]

PathLieDerivative[a_, b_] := (PathCentralDifference[a, b] - PathCentralDifference[b, a])/4

(**************************************************************************************************)

PublicFunction[ReversalSymmetricQ, ReversalAntisymmetricQ]

ReversalSymmetricQ[p_PathVector] :=
  p === PathReverse[p];

ReversalAntisymmetricQ[p_PathVector] :=
  -p === PathReverse[p];

(**************************************************************************************************)

PublicFunction[WordDelta]

SetUsage @ "
WordDelta['word$', 'type$'] constructs a path vector that when convolved computes the finite difference \
along the word 'word$'.
* 'type$' can be one of:
| 'Forward' | forward finite difference (X - I) |
| 'Backward' | reverse finite difference (X\[Conjugate] - I) |
| 'Symmetric' | X + X\[Conjugate] - 2 I |
| 'Antisymmetric' | X - X\[Conjugate] |
"

DefineFunctionAutocomplete[WordDelta, {0, $directionStrings}];

WordDelta[word_Str, type_:"Forward"] /; $PathAlgebraQ := Scope[
  forward = WordVector @ word;
  reverse := PathReverse @ forward;
  unit := VertexField[];
  Switch[type,
    "Forward",        forward - unit,
    "Backward",       reverse - unit,
    "Symmetric",      forward + reverse + -2 * unit,
    "Antisymmetric",  forward - reverse,
    _,                ReturnFailed[WordDelta::badpvectype, type]
  ]
]

General::badpvectype = "`` is not one of \"Forward\", \"Reverse\", \"Symmetric\", or \"Antisymmetric\"."

makeTypeVector[head_, vector_PathVector, type_] :=
  Switch[type,
    "Forward",        vector,
    "Backward",       PathReverse @ vector,
    "Symmetric",      vector + PathReverse[vector],
    "Antisymmetric",  vector - PathReverse[vector],
    _,                (Message[head::badpvectype, type]; $Failed)
  ];

(**************************************************************************************************)

PublicFunction[PathLength]

SetUsage @ "
PathLength[element$] returns the length of a %PathElement.
"

PathLength[PathElement[_, e_List, _]] := Len @ e;

(**************************************************************************************************)

PublicFunction[PathConjugate]

PathConjugate[pv:PathVector[assoc_]] :=
  If[FreeQ[Values @ assoc, Complex], pv, PathVector[Conjugate /@ assoc]];

(**************************************************************************************************)

PublicFunction[PathPower]

PathPower[PathVector[assoc_], p_] :=
  PathVector[Power[#, p]& /@ assoc];

(**************************************************************************************************)

PublicFunction[PathSqrt]

PathSqrt[PathVector[assoc_]] :=
  PathVector @ Map[Sqrt, assoc];

(**************************************************************************************************)

PublicFunction[PathConjugateTranspose]

PathConjugateTranspose[pv_PathVector] :=
  PathConjugate @ PathReverse @ pv;

(**************************************************************************************************)

PublicFunction[PathReverse]

SetUsage @ "
PathReverse[PathVector[$$]] yields the PathVector[$$] in which all \
paths have been reversed.
PathReverse[PathElement[$$]] yields the reverse of PathElement[$$].
"

PathReverse[PathVector[assoc_]] :=
  PathVector @ KeySort @ KeyMap[PathReverse, assoc]

PathReverse[PathElement[t_, e_, h_]] := PathElement[h, InvertReverse @ e, t];

PathReverse[NullElement] := NullElement;

(**************************************************************************************************)

PublicHead[PathElement]

SetUsage @ "
PathElement[tail$, {edge$1, edge$2, $$}, head$] represents a path starting at vertex tail$, taking \
the given edges, and ending at vertex head$.
"

(**************************************************************************************************)

PublicFunction[EmptyPathElement]

EmptyPathElement[v_] := PathElement[v, {}, v];

(**************************************************************************************************)

PublicFunction[VertexField]

SetUsage @ "
VertexField[] constructs the unit vertex field, containing every empty path with weight 1.
VertexField[n$] constucts the empty path on vertex n$ with weight 1.
VertexField[-n$] constucts the empty path on vertex n$ with weight -1.
VertexField[{n$1, n$2, $$}] constructs the sum of empty paths of on vertices n$i.
"

VertexField[] /; $PathAlgebraQ :=
  PathVector @ ConstantAssociation[
    EmptyPathElement /@ $PathAlgebra["VertexRange"],
    1
  ];

VertexField[ints:{__Int} | _Int] /; $PathAlgebraQ := Scope[
  UnpackPathAlgebra[pos, neg];
  PathVector @ Assoc @ Map[
    i |-> (EmptyPathElement[Abs @ i] -> If[Positive[i], pos, neg]),
    Sort @ ToList @ ints
  ]
]

(**************************************************************************************************)

PublicFunction[RandomVertexField]

SetUsage @ "
RandomVertexField[] constructs a random vertex field, containing every empty path with a random weight.
* The random weights are chosen from the appropriate base field.
* For finite fields, any value is equally likely to be chosen.
* For infinite fields, the range [-2, 2] is used.
"

Options[RandomVertexField] = {RandomSeeding -> Automatic};

RandomVertexField[OptionsPattern[]] /; $PathAlgebraQ := Scope @ RandomSeeded[
  UnpackPathAlgebra[vertexRange, randomFieldElement];
  constructPathVector @ Map[v |-> EmptyPathElement[v] -> randomFieldElement[], vertexRange],
  OptionValue[RandomSeeding]
];

(**************************************************************************************************)

PublicFunction[RandomEdgeField]

SetUsage @ "
RandomEdgeField[] constructs a random edge field, containing every length-1 path with a random weight.
RandomEdgeField['type$'] chooses only specific orientations of length-1 paths.
* The random weights are chosen from the appropriate base field.
* For finite fields, any value is equally likely to be chosen.
* For infinite fields, the range [-2, 2] is used.
* The available orientations are 'Forward', 'Backward', and 'Symmetric' (default).
"

DefineFunctionAutocomplete[RandomEdgeField, {$directionStrings}];

Options[RandomEdgeField] = {RandomSeeding -> Automatic, "Density" -> 1};

RandomEdgeField[type_Str:"Symmetric", OptionsPattern[]] /; $PathAlgebraQ := Scope @ RandomSeeded[
  UnpackPathAlgebra[edgeRange, randomFieldElement, edgeToTail, edgeToHead];
  UnpackOptions[density];
  randomEdgeMaker = Switch[type,
    "Forward",        makeRandomForwardEdge,
    "Backward",       makeRandomBackwardEdge,
    "Symmetric",      makeRandomSymmetricEdge,
    "Antisymmetric",  makeRandomAntisymmetricEdge,
    _,                ReturnFailed["badpvectype", type]
  ];
  If[density < 1, edgeRange = RandomSample[edgeRange, Ceiling[Len[edgeRange] * density]]];
  constructPathVector @ Map[randomEdgeMaker[#1, randomFieldElement[]]&, edgeRange]
,
  OptionValue[RandomSeeding]
];

makeRandomForwardEdge[edge_, w_] :=
  singleEdgePathElement[edge] -> w;

makeRandomBackwardEdge[edge_, w_] :=
  reversedSingleEdgePathElement[edge] -> w;

makeRandomSymmetricEdge[edge_, w_] := {
  singleEdgePathElement[edge] -> w,
  reversedSingleEdgePathElement[edge] -> w
}

makeRandomAntisymmetricEdge[edge_, w_] := {
  singleEdgePathElement[edge] -> w,
  reversedSingleEdgePathElement[edge] -> -w
}

(**************************************************************************************************)

singleEdgePathElement[edge_] :=
  PathElement[edgeToTail @ edge, {edge}, edgeToHead @ edge];

reversedSingleEdgePathElement[edge_] :=
  PathElement[edgeToHead @ edge, {Inverted @ edge}, edgeToTail @ edge];

(**************************************************************************************************)

PublicFunction[RandomPathField]

SetUsage @ "
RandomPathField[] constructs a random path field of paths of length 0 \[LessEqual] n$ \[LessEqual] 2.
RandomPathField[n$] chooses paths to have length n$.
RandomPathField[{n$min, n$max}] chooses paths to have length n$, where n$min \[LessEqual] n$ \[LessEqual] n$max.
* The random weights are chosen from the appropriate base field.
* For finite fields, any value is equally likely to be chosen.
* For infinite fields, the range [-2, 2] is used.
* Only one path will start at a given vertex (hence the name 'field').
* RandomPathField[spec$] is equivalent to RandomPathVector[1, spec$].
"

Options[RandomPathField] = {RandomSeeding -> Automatic};

RandomPathField[opts:OptionsPattern[]] :=
  RandomPathVector[1, opts];

RandomPathField[range:{_Int ? NonNegative, _Int ? NonNegative} | (_Int ? NonNegative), opts:OptionsPattern[]] :=
  RandomPathVector[1, range, opts];

(**************************************************************************************************)

PublicFunction[SymbolicVertexField]

SymbolicVertexField[symbol_:\[FormalV]] := Scope[
  UnpackPathAlgebra[vertexRange];
  PathVector @ Assoc @ Map[EmptyPathElement[#] -> Subscript[symbol, #]&, vertexRange]
];

(**************************************************************************************************)

PublicFunction[SymbolicEdgeField]

SymbolicEdgeField[symbol_:\[FormalE], type_:"Forward"] := Scope[
  UnpackPathAlgebra[edgeRange, edgeToTail, edgeToHead];
  forward = edgeRange;
  reverse = Inverted /@ edgeRange;
  edges = Switch[type,
    "Forward",        forward,
    "Backward",       reverse,
    "Symmetric" |
    "Antisymmetric",  Join[forward, reverse],
    _,                ReturnFailed["badpvectype", type]
  ];
  i = 1;
  PathVector @ Assoc @ KeySort @ Map[edge |-> singleEdgePathElement[edge] -> Subscript[symbol, i++], edges]
];

(**************************************************************************************************)

PublicFunction[SparsifyPathVector]

SetUsage @ "
SparsifyPathVector[vector$, frac$] deletes frac$ of the paths in a %PathVector.
"

SparsifyPathVector[PathVector[assoc_], frac_Real] :=
  PathVector @ Select[assoc, RandomReal[] > frac&]

(**************************************************************************************************)

PublicFunction[ProjectPathLength]

SetUsage @ "
ProjectPathLength[vector$, n$] returns the path vector consisting of paths of length n$.
"

ProjectPathLength[pv_PathVector] :=
  ProjectPathLength[pv, All];

ProjectPathLength[PathVector[assoc_], n_] :=
  PathVector @ KeySelect[assoc, PathLength[#] === n&]

ProjectPathLength[pv:PathVector[assoc_], All] := Scope[
  max = Max @ Map[PathLength, Keys @ assoc];
  Table[ProjectPathLength[pv, n], {n, 0, max}]
];

(**************************************************************************************************)

PublicFunction[RemoveEmptyPaths]

RemoveEmptyPaths[PathVector[assoc_]] :=
  PathVector @ KeySelect[assoc, PathLength[#] > 0&];

(**************************************************************************************************)

PublicFunction[RandomPathVector]

SetUsage @ "
RandomPathVector[m$] constructs a random path vector of paths of length 0 \[LessEqual] n$ \[LessEqual] 2, with m$ starting at each vertex.
RandomPathVector[m$, n$] chooses paths to have length n$.
RandomPathVector[m$, {n$min, n$max}] chooses paths to have length n$, where n$min \[LessEqual] n$ \[LessEqual] n$max.
* Unlike RandomPathField, RandomPathVector explicitly does construct paths that start at the same vertex.
* The random weights are chosen from the appropriate base field.
* For finite fields, any value is equally likely to be chosen.
* For infinite fields, the range [-2, 2] is used.
* RandomPathVector[1, spec$] is equivalent to RandomPathField[spec$].
"

Options[RandomPathVector] = {RandomSeeding -> Automatic};

RandomPathVector[m_Int, opts:OptionsPattern[]] :=
  RandomPathVector[m, {0, 2}, opts];

RandomPathVector[m_Int, range_Int, opts:OptionsPattern[]] :=
  RandomPathVector[m, {range, range}, opts];

RandomPathVector[m_Int, range:{_Int ? NonNegative, _Int ? NonNegative}, OptionsPattern[]] := Scope @ RandomSeeded[
  UnpackPathAlgebra[vertexRange, outEdgeTable, randomFieldElement, edgeToHead];
  constructPathVector @ Map[
    vertex |-> Table[
      makeRandomPathElement[vertex, RandomInteger[range]] -> randomFieldElement[],
      {m}
    ],
    vertexRange
  ],
  OptionValue[RandomSeeding]
];

makeRandomPathElement[tail_, len_] := Scope[
  head = tail;
  edges = Table[
    edge = RandomChoice @ Part[outEdgeTable, head];
    head = edgeToHead @ edge;
    edge
  ,
    {len}
  ];
  PathElement[tail, edges //. $cancelEdgeRules, head]
];


(**************************************************************************************************)

PublicFunction[VertexFieldQ]

VertexFieldQ[PathVector[assoc_Assoc]] := MatchQ[Keys @ assoc, {PathElement[_, {}, _]...}];
VertexFieldQ[_] := False;

(**************************************************************************************************)

PublicFunction[EdgeField]

SetUsage @ "
EdgeField[] gives EdgeField[All].
EdgeField[All] constructs the sum of all length 1 paths (edge paths).
EdgeField[i$] gives the length 1 path on edge 1.
EdgeField[spec$,'type$'] constructs one of the following:
| 'Forward' | construct path in forward direction  (default) |
| 'Backward' | construct path in reverse direction |
| 'Symmetric' | uses weight 1 for forward and reverse directions |
| 'Antisymmetric' | uses weight 1 for forward  and -1 for reverses |
"

DefineFunctionAutocomplete[EdgeField, {0, $directionStrings}];

EdgeField[] := EdgeField[All];

EdgeField[All] /; $PathAlgebraQ := Scope[
  UnpackPathAlgebra[edgeRange, pos, neg, edgeToTail, edgeToHead];
  PathVector @ ConstantAssociation[
    singleEdgePathElement /@ edgeRange,
    pos
  ]
];

EdgeField[ints:{__Int} | _Int] /; $PathAlgebraQ := Scope[
  UnpackPathAlgebra[edgeRange, pos, neg, edgeToTail, edgeToHead];
  ints = ToList @ ints;
  edges = Part[edgeRange, Abs @ ints];
  PathVector @ Assoc @ MapThread[
    {edge, int} |-> edge -> If[Positive[int], pos, neg],
    {singleEdgePathElement /@ edges, ints}
  ]
];

EdgeField[spec_, "Forward"] := EdgeField[spec];
EdgeField[spec_, "Backward"] := PathReverse @ EdgeField[spec];

EdgeField[spec_, "Symmetric"] := symmetricEdgeField[spec, True];
EdgeField[spec_, "Antisymmetric"] := symmetricEdgeField[spec, False];

symmetricEdgeField[All, sym_] := Scope[
  UnpackPathAlgebra[edgeRange, pos, neg, edgeTails, edgeRange, edgeHeads];
  If[sym, neg = pos];
  constructPathVector @ MapThread[
    {tail, edge, head} |-> weightedForwardBackwardEdgeElements[
      tail, edge, head, {pos, neg}
    ],
    {edgeTails, edgeRange, edgeHeads}
  ]
];

symmetricEdgeField[i_Int, sym_] := symmetricEdgeField[{i}, sym];

symmetricEdgeField[ints:{__Int}, sym_] := Scope[
  UnpackPathAlgebra[pos, neg, edgeTails, edgeRange, edgeHeads];
  {edgeTails, edgeRange, edgeHeads} //= PartOperator[All, Abs @ ints];
  If[sym, neg = pos];
  constructPathVector @ MapThread[
    {tail, edge, head, int} |-> weightedForwardBackwardEdgeElements[
      tail, edge, head,
      If[Positive[int], {pos, neg}, {neg, pos}]
    ],
    {edgeTails, edgeRange, edgeHeads, ints}
  ]
];

weightedForwardBackwardEdgeElements[tail_, edge_, head_, {fweight_, bweight_}] := {
  PathElement[tail, {edge}, head] -> fweight,
  PathElement[head, {Inverted @ edge}, tail] -> bweight
};


      
(**************************************************************************************************)

PublicFunction[EdgeFieldQ]

EdgeFieldQ[PathVector[assoc_Assoc]] := MatchQ[Keys @ assoc, {PathElement[_, {_}, _]...}];
EdgeFieldQ[_] := False;

(**************************************************************************************************)

PublicFunction[PathElementToWord]

SetUsage @ "
PathElementToWord[PathElement[$$]] returns the cardinal word for a path element.
PathElementToWord[list$] returns the words for a list of path elements.
* These words are not relative to the tail frame.
"

PathElementToWord[PathElement[_, edges_, _]] := Scope[
  UnpackPathAlgebra[edgeToCardinal];
  Lookup[edgeToCardinal, edges]
];

PathElementToWord[list:{___PathElement}] := Scope[
  UnpackPathAlgebra[edgeToCardinal];
  Lookup[edgeToCardinal, #]& /@ Col2[list]
]

(**************************************************************************************************)

PublicFunction[PathElementToTailFrameWord]

SetUsage @ "
PathElementToTailFrameWord[PathElement[$$]] returns the cardinal word for a path element.
PathElementToTailFrameWord[list$] returns the words for a list of path elements.
* These words are relative to the tail frame, taking into account cardinal transport.
"

PathElementToTailFrameWord[p_PathElement | p:{___PathElement}] := Scope[
  UnpackPathAlgebra[edgeToCardinal, vertexTags, cardinalTransitions];

  If[ListQ[p], tailFrameWord /@ p, tailFrameWord @ p]
];

tailFrameWord[p_PathElement] :=
  P2[elementFrameData @ p];

(**************************************************************************************************)

elementFrameData[PathElement[tail_, edges_, _]] := Scope[
  vertex = tail;
  iframe = frame = Part[vertexTags, tail];
  word = Map[edge |-> (
    transitions = Lookup[cardinalTransitions, edge, {}];
    frame //= ReplaceAll[transitions];
    index = IndexOf[frame, Lookup[edgeToCardinal, edge]];
    (* why can this fail? loop = PathCompose[a, b, c]; TranslationCommutator[loop, PathReverse@loop]*)
    If[MissingQ[index], Return[$Failed, Block]];
    Part[iframe, index]),
    edges
  ];
  {iframe, word, frame}
]

(**************************************************************************************************)

PublicFunction[PathElementFromTailFrameWord]

SetUsage @ "
PathElementFromTailFrameWord[tail$, word$] returns the PathElement starting at vertex tail$ and using word$.
* The word$ should use cardinals present in the tail frame at tail$ only, they will be transported as necessary.
"

PathElementFromTailFrameWord[tail_, word_] := Scope[

  UnpackPathAlgebra[vertexTags, cardinalTransitions, tagOutEdgeTable, nullEdge, edgeToHead];

  tailFrameWordToElement[tail, word];
];

(**************************************************************************************************)

tailFrameWordToElement[tail_, word_] := Scope[
  iframe = frame = Part[vertexTags, tail];
  head = tail;

  edges = Map[
    card |-> (
      cardIndex = IndexOf[iframe, card, Return[NullElement, Block]];
      localCard = Part[frame, cardIndex];
      edge = Part[tagOutEdgeTable @ localCard, head];
      If[edge === nullEdge, Return[NullElement, Block]];
      transitions = Lookup[cardinalTransitions, edge, {}];
      frame //= ReplaceAll[transitions];
      head = edgeToHead @ edge;
      edge
    ),
    word
  ];

  PathElement[tail, edges, head]
];

DefineSimpleMacro[setupForTranslation,
  setupForTranslation[] :> (
    UnpackPathAlgebra[
      edgeToCardinal, (* elementFrameData *)
      vertexTags, cardinalTransitions, tagOutEdgeTable, nullEdge, edgeToHead (* tailFrameWordToElement *)
    ];
    $frameDataCache = UAssoc[];
  )
];

DefineSimpleMacro[setupForShortestPaths,
  setupForShortestPaths[] :> (
    UnpackPathAlgebra[indexQuiver];
    undirectedIndexQuiver = Graph[VertexList @ indexQuiver, (EdgeList @ indexQuiver) /. DirectedEdge -> UndirectedEdge];
    shortestPath = FindShortestPath[undirectedIndexQuiver, All, All];
    edgeIndex = AssociationRange @ EdgePairs @ indexQuiver;
    edgeIndex = Join[edgeIndex, Map[Inverted] @ KeyMap[Rev] @ edgeIndex];
  )
];


(**************************************************************************************************)

PublicFunction[PathTranslate]

PathTranslate[t_PathVector, p_PathVector] /; $PathAlgebraQ := Scope[

  setupForTranslation[];

  BilinearApply[elementTranslate, t -> PathTailVertex, p -> PathTailVertex]
]

PathTranslate[t_PathVector][p_PathVector] := PathTranslate[t, p];

elementTranslate[t_PathElement, p_PathElement, anti_:False] := Scope[

  If[PathTailVertex[t] =!= PathTailVertex[p], $Unreachable];

  tFrameData = CacheTo[$frameDataCache, t, elementFrameData @ t];
  pFrameData = CacheTo[$frameDataCache, p, elementFrameData @ p];

  If[FailureQ[tFrameData] || FailureQ[pFrameData], Return @ NullElement];

  pWord = P2[pFrameData];
  tTransport = RuleThread[P1 @ tFrameData, PN @ tFrameData];

  pWordTransported = Replace[pWord, tTransport, {1, 2}];

  If[anti, pWordTransported = Inverted /@ pWordTransported];

  tailFrameWordToElement[
    PathHeadVertex @ t,
    pWordTransported
  ]
]

elementTranslateHead[t_PathElement, p_PathElement, anti_:False] :=
  PathReverse @ elementTranslate[t, PathReverse @ p, anti];

PathVector /: UpArrow[t_PathVector, p_PathVector] :=
  PathTranslate[t, p];

(**************************************************************************************************)

PublicFunction[PathReverseTranslate]

PathReverseTranslate[t_PathVector, p_PathVector] /; $PathAlgebraQ :=
  PathTranslate[PathReverse @ t, p];

PathVector /: DownArrow[t_PathVector, p_PathVector] :=
  PathReverseTranslate[t, p];

(**************************************************************************************************)

PublicFunction[TranslateAdd]

SetUsage @ "
TranslateAdd[t$1, t$2] composes %PathVector t$2 with t$1, after translating \
path elements of t$2 to the head of path elements of t$1.
TranslateAdd[t$1, t$2, t$3, $$] chains together several %TranslateAdd operations.
* t$1 \[CirclePlus] t$2 is infix syntax for TranslateAdd.
"

TranslateAdd[p__PathVector] := Scope[
  setupForTranslation[];
  iTranslateAdd[p]
];

iTranslateAdd[t_PathVector, p_PathVector, rest___] :=
  iTranslateAdd[
    BilinearApply[elementTranslateAdd, t -> PathTailVertex, p -> PathTailVertex],
    rest
  ]

iTranslateAdd[p_PathVector] := p;

elementTranslateAdd[t_PathElement, p_PathElement] :=
  elementCompose[t, elementTranslate[t, p]]

PathVector /: CirclePlus[p__PathVector] :=
  TranslateAdd[p];

(**************************************************************************************************)

PublicFunction[TranslateSubtract]

SetUsage @ "
TranslateSubtract[t$1, t$2] composes the reverse of %PathVector t$2 with t$1, after translating \
path elements of t$2 to the head of path elements of t$1.
* t$1 \[CircleMinus] t$2 is infix syntax for TranslateSubtract.
"

TranslateSubtract[a_PathVector, b_PathVector] := Scope[
  setupForTranslation[];
  BilinearApply[elementTranslateSubtract, a -> PathTailVertex, b -> PathTailVertex]
];

elementTranslateSubtract[t_PathElement, p_PathElement] :=
  elementCompose[t, elementTranslate[t, p, True]]

PathVector /: CircleMinus[a_PathVector, b_PathVector] :=
  TranslateSubtract[a, b];

(**************************************************************************************************)

PublicFunction[TranslateHeadSubtract]

TranslateHeadSubtract[a_PathVector, b_PathVector] := Scope[
  setupForTranslation[];
  BilinearApply[elementTranslateHeadSubtract, a -> PathTailVertex, b -> PathHeadVertex]
];

elementTranslateHeadSubtract[t_PathElement, p_PathElement] :=
  elementCompose[t, PathReverse @ elementTranslateHead[t, p]]

TranslateHeadSubtract[a_PathVector, b_PathVector] :=
  PathReverse @ TranslateSubtract[a, b];

(**************************************************************************************************)

PublicFunction[TranslationDifference]

TranslationDifference[a_PathVector, b_PathVector] := Scope[
  setupForTranslation[];
  setupForShortestPaths[];
  BilinearApply[elementTranslationDifference, a -> PathTailVertex, b -> PathTailVertex]
]

elementTranslationDifference[a_PathElement, b_PathElement] := Scope[
  ab = elementTranslate[a, b];
  ba = elementTranslate[b, a];
  If[ab === NullElement || ba === NullElement, Return @ NullElement];
  t = PathHeadVertex @ ab; h = PathHeadVertex @ ba;
  shortest = shortestPath[t, h];
  PathElement[t, edgeIndex /@ Partition[shortest, 2, 1], h]
];

(**************************************************************************************************)

PublicFunction[AntisymmetrizedTranslationDifference]

AntisymmetrizedTranslationDifference[a_, b_] := Scope[
  res = TranslationDifference[a, b]/2;
  res - PathReverse[res]
];

(**************************************************************************************************)

PublicFunction[PathInvert]

SetUsage @ "
PathInvert[vector$] inverts a %PathVector by negating the path word of each of its elements.
"

PathInvert[PathVector[assoc_]] := Scope[
  setupForTranslation[];
  constructPathVector @ KeyMap[invertElement, assoc]
];

invertElement[p:PathElement[t_, edges_, h_]] :=
  tailFrameWordToElement[t, Inverted /@ tailFrameWord[p]]

(**************************************************************************************************)

PublicFunction[ShortestPathVector]

SetUsage @ "
ShortestPathVector[vector$] replaces the %PathElement in vector$ with their shortest path equivalents.
"

ShortestPathVector[PathVector[assoc_]] := Scope[
  setupForShortestPaths[];
  constructPathVector @ KeyMap[shortestPathElement, assoc]
];

shortestPathElement[p:PathElement[t_, e_, h_]] := Scope[
  shortest = shortestPath[t, h];
  len = Len @ shortest;
  If[len < Len[e] + 1,
    PathElement[t, edgeIndex /@ Partition[shortest, 2, 1], h],
    p
  ]
];

(**************************************************************************************************)

PublicFunction[BilinearApply]

BilinearApply[monoid_, PathVector[a_] -> f_, PathVector[b_] -> g_] /; $PathAlgebraQ := Scope[
  {ak, av} = KeysValues @ a; {bk, bv} = KeysValues @ b;
  af = Map[f, ak]; bf = Map[g, bk];
  ik = Intersection[af, bf];
  ap = PositionIndex @ af; bp = PositionIndex @ bf;
  UnpackPathAlgebra[fieldTimes, fieldPlus]; $monoid = monoid;
  rules = MapThread[
    {iset, jset} |-> Outer[weightedApply, iset, jset, 1],
    Lookup[{ap, bp}, ik]
  ];
  constructPathVector @ Merge[Flatten @ rules, Apply @ fieldPlus]
];

weightedApply[i_, j_] :=
  flattenWeights[$monoid[Part[ak, i], Part[bk, j]], fieldTimes[Part[av, i], Part[bv, j]]];

flattenWeights[result_, w_] := result -> w;
flattenWeights[results_List, w_] := #1 -> w& /@ results;
flattenWeights[rules:{__Rule}, w_] := #1 -> fieldTimes[#2, w]& @@@ rules;

(**************************************************************************************************)

PublicFunction[TorsionVector]

SetUsage @ "
TorsionVector[a$, b$] returns (b$ \[CirclePlus] a$) \[CircleMinus] (a$ \[CirclePlus] b$).
* \[CirclePlus] is TranslateAdd.
* \[CircleMinus] is TranslateSubtract.
"

TorsionVector[a_, b_] := Scope[
  ab = TranslateAdd[b, a];
  ba = TranslateAdd[a, b];
  TranslateSubtract[ab, ba]
];

(**************************************************************************************************)

PublicFunction[PathForwardDifference]

PathForwardDifference[list:{__List}, target_] :=
  Apply[PathVectorPlus, PathForwardDifference[#, target]& /@ list];

PathForwardDifference[{v1_, vn___}, target_] :=
  PathForwardDifference[v1, PathForwardDifference[{vn}, target]];

PathForwardDifference[{}, target_] := target;

PathForwardDifference[flow_, target_] :=
  PathTranslate[PathTailVector[flow] - flow, target];

PathForwardDifference[v_][t_] := PathForwardDifference[v, t];

(**************************************************************************************************)

PublicFunction[PathBackwardDifference]

PathBackwardDifference[list:{__List}, target_] :=
  Apply[PathVectorPlus, PathBackwardDifference[#, target]& /@ list];

PathBackwardDifference[flow_, target_] :=
  PathTranslate[PathReverse[flow] - PathHeadVector[flow], target];

PathBackwardDifference[{v1_, vn___}, target_] :=
  PathBackwardDifference[v1, PathBackwardDifference[{vn}, target]];

PathBackwardDifference[{}, target_] := target;

PathBackwardDifference[v_][t_] := PathBackwardDifference[v, t];

(**************************************************************************************************)

PublicFunction[PathCentralDifference]

PathCentralDifference[list:{__List}, target_] :=
  Apply[PathVectorPlus, PathCentralDifference[#, target]& /@ list];

PathCentralDifference[flow_, target_] :=
  PathTranslate[-flow + PathReverse[flow], target];

PathCentralDifference[{v1_, vn___}, target_] :=
  PathCentralDifference[v1, PathCentralDifference[{vn}, target]];

PathCentralDifference[{}, target_] := target;

PathCentralDifference[v_][t_] := PathCentralDifference[v, t];

(**************************************************************************************************)

PublicFunction[LaplacianOperator]

LaplacianOperator[] := Scope[
  basis = BasisWordVectors[];
  basis = Join[basis, PathReverse /@ basis];
  PathTranslate[Apply[PathVectorPlus, basis] - VertexField[]]
];

LaplacianOperator[pv_PathVector] :=
  LaplacianOperator[][pv];

(**************************************************************************************************)

PublicFunction[SymmetricPathFiniteDifference]

SymmetricPathFiniteDifference[flow_, target_] :=
  PathTranslate[PathReverse[flow] - flow, target];

SymmetricPathFiniteDifference[v_][t_] := SymmetricPathFiniteDifference[v, t];

(**************************************************************************************************)

PublicFunction[PathGradient]

PathGradient[pv_PathVector] /; $PathAlgebraQ := Scope[
  UnpackPathAlgebra[edgeTails, edgeRange, edgeHeads, vertexRange, fieldSubtract, fieldMinus];
  weights = VertexFieldWeights @ pv;
  constructPathVector @ MapThread[
    {tail, edge, head} |-> weightedForwardBackwardEdgeElements[
      tail, edge, head,
      w = fieldSubtract[Part[weights, head], Part[weights, tail]];
      {w, fieldMinus @ w}
    ],
    {edgeTails, edgeRange, edgeHeads}
  ]
]

(**************************************************************************************************)

PublicFunction[PathDivergence]

PathDivergence[pv_PathVector] /; $PathAlgebraQ :=
  (PathHeadVector[pv] - PathTailVector[pv]) / 2;

(**************************************************************************************************)

PublicFunction[PathLaplacian]

PathLaplacian[pv_PathVector] /; $PathAlgebraQ :=
  PathDivergence @ PathGradient @ pv;

(**************************************************************************************************)

PublicFunction[PathDot]

PathDot[a_PathVector, b_PathVector] /; $PathAlgebraQ := Scope[
  assocs = Col1[{a, b}];
  {a1, a2} = KeyIntersection[assocs];
  Total[a1 * a2]
];
 *)