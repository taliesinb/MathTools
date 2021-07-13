declarePlusTimesDispatch[symbol_Symbol, test_, dispatch_] := (
  symbol /: Plus[objects:Repeated[_symbol ? test, {2, Infinity}]] := dispatch[Plus][objects];
  symbol /: Times[scalar_ /; NumericQ[Unevaluated @ scalar], object_symbol ? test] := dispatch[Times][scalar, object];
  symbol /: Times[object1_symbol ? test, object2_symbol ? test] := dispatch[Times][object1, object2];
)

DefineMacro[UnpackPathAlgebra,
UnpackPathAlgebra[args___] := Quoted @ UnpackAssociation[getObjectData @ $PathAlgebra, args]
];

$directionStrings = {"Forward", "Reverse", "Symmetric", "Antisymmetric"};

(**************************************************************************************************)

PackageExport["$PathAlgebra"]

SetUsage @ "
$PathAlgebra is the PathAlgebra[$$] used as the context for PathVector and PathElement objects.
"

$PathAlgebra::notset = "$PathAlgebra is not set."
$PathAlgebra = None;
$PathAlgebraQ := If[$PathAlgebra === None, Message[$PathAlgebra::notset]; False, True];
$FieldPlus := $PathAlgebra["FieldPlus"];
$FieldTimes := $PathAlgebra["FieldTimes"];

PackageExport["ModOperator"]
PackageExport["PlusModOperator"]
PackageExport["TimesModOperator"]

ModOperator[n_][e_] := Mod[e, n, 0];
ModOperator[Infinity] = Identity;
PlusModOperator[n_] := Plus /* ModOperator[n];
TimesModOperator[n_] := Times /* ModOperator[n];

(**************************************************************************************************)

PackageExport["PathAlgebra"]

SetUsage @ "
PathAlgebra[quiver$, field$] constructs a PathAlgebra[$$] from a given cardinal quiver.
PathAlgebra['lattice$', field$] uses a named lattice quiver as the quiver.
PathAlgebra[spec$] uses %Integers as the field.
* field$ can be an integer for a finite field (or ring), or one of %Integers, %Reals, or %Complexes.
"

Options[PathAlgebra] = {
  PlotRange -> Automatic,
  EdgeSetback -> 0.25,
  ImageSize -> 100
};

$graphOrLatticeSpec = _Graph | _String | {_String, __};

PathAlgebra[quiver:$graphOrLatticeSpec, opts:OptionsPattern[]] ? System`Private`HoldEntryQ =
  PathAlgebra[quiver, Integers, opts];

PathAlgebra[quiver:$graphOrLatticeSpec, field_, OptionsPattern[]] ? System`Private`HoldEntryQ := Scope[

  If[MatchQ[quiver, _String | {_String, __}],
    quiver = LatticeQuiver[quiver]];
  If[!QuiverQ[quiver], ReturnFailed[]];

  If[!MatchQ[field, _Integer | Integers | Reals | Complexes], ReturnFailed[]];

  UnpackOptions[plotRange, edgeSetback, imageSize];
  If[NumericQ[plotRange], plotRange *= {{-1, 1}, {-1, 1}}];

  data = <||>;
  {vertexCoords, edgeCoords} = ExtractGraphPrimitiveCoordinates @ quiver;

  data["Quiver"] = quiver;
  data["IndexQuiver"] = ToIndexGraph @ quiver;
  data["VertexList"] = VertexList @ quiver;

  data["VertexCoordinates"] = vertexCoords;
  data["EdgeCoordinateLists"] = edgeCoords;
  data["EdgeCoordinates"] = edgeCoords;
  data["PlotRange"] = Replace[plotRange, Automatic :> CoordinateBounds[vertexCoords]];
  data["EdgeSetback"] = edgeSetback;
  data["ImageSize"] = imageSize;

  data["Field"] = field;
  data["FieldModulus"] = mod = Match[field, n_Integer :> n, Infinity];
  data["FieldPlus"] = PlusModOperator[mod];
  data["FieldTimes"] = TimesModOperator[mod];
  {pos, neg} = Match[field, n_Integer :> {1, n-1}, {1, -1}];
  data["Pos"] = pos;
  data["Neg"] = neg;
  data["RandomFieldElement"] = Match[field, n_Integer :> Function[RandomInteger[n-1]], RandomInteger[{-2, 2}]&];

  data["FieldColorFunction"] = If[IntegerQ[mod],
    AssociationThread[Range[0, mod-1], fieldColors @ mod],
    $complexColorFunction
  ];

  data["VertexCount"] = vertexCount = VertexCount @ quiver;
  data["EdgeCount"] = edgeCount = EdgeCount @ quiver;
  data["VertexRange"] = Range @ vertexCount;
  data["EdgeRange"] = edgeRange = Range @ edgeCount;
  data["Cardinals"] = CardinalList @ quiver;

  ct = Lookup[LookupAnnotation[quiver, EdgeAnnotations, <||>], "CardinalTransitions", <||>];
  data["CardinalTransitions"] = Join[ct, KeyMap[Negated] @ Map[reverseTransition, ct, {2}]];

  pairs = EdgePairs @ quiver;
  tailVertices = Part[pairs, All, 1];
  headVertices = Part[pairs, All, 2];
  tailAssoc = AssociationThread[edgeRange, tailVertices];
  headAssoc = AssociationThread[edgeRange, headVertices];

  data["EdgeToTail"] = Join[tailAssoc, KeyMap[Negated, headAssoc]];
  data["EdgeToHead"] = Join[headAssoc, KeyMap[Negated, tailAssoc]];

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
  data["EdgeToCardinal"] = Join[edgeToCard, KeyMap[Negated] @ Map[Negated] @ edgeToCard];

  System`Private`ConstructNoEntry[PathAlgebra, data]
];

$complexColorFunction = ComplexHue;

ComplexHue[c_] := Hue[Arg[c]/Tau+.05, Min[Sqrt[Abs[c]]/1.2,1]]

reverseTransition[a_ -> Negated[b_]] := b -> Negated[a];
reverseTransition[a_ -> b_] := b -> a;

(**************************************************************************************************)

PackageExport["PathAlgebra"]

declareObjectPropertyDispatch[PathAlgebra, pathAlgebraProperty];

MakeBoxes[pa_PathAlgebra ? System`Private`HoldNoEntryQ, form_] :=
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

PackageExport["PathVector"]

SetUsage @ "
PathVector[$$] represents a path vector on a cardinal quiver.
"

declarePlusTimesDispatch[PathVector, PathVectorQ, $pathVectorDispatch]

$pathVectorDispatch = <|
  Plus -> PathVectorPlus,
  Times -> PathVectorTimes
|>;

PackageExport["PathVectorPlus"]
PackageExport["PathVectorTimes"]

PathVectorPlus[v__PathVector] := PathVectorElementwise[Apply @ $FieldPlus, 0, {v}];
PathVectorTimes[v__PathVector] := PathVectorElementwise[Apply @ $FieldTimes, 0, {v}];
PathVectorTimes[n_ ? NumericQ, p_PathVector] := With[{t = $FieldTimes}, PathVectorMap[t[n, #]&, p]];

PathVectorMap[f_, PathVector[assoc_]] :=
  PathVector @ Map[f, assoc];

PathVectorElementwise[_, _, {vec_}] := vec;

PathVectorElementwise[f_, id_, vecs_] := Scope[
  assocs = Part[vecs, All, 1];
  constructPathVector @ Merge[KeyUnion[assocs, id&], f]
];

mergeWeights[rules_, id_, f_] :=
  Merge[KeyUnion[Flatten @ rules, id&], f];

constructPathVector[list_List] := constructPathVector @ Association @ list;

rulesToPathVector[list_List] := constructPathVector @ Merge[Flatten @ list, Apply @ $FieldTimes];

constructPathVector[assoc_Association] :=
  PathVector @ Association @ KeySort @ KeyDrop[NullElement] @ DeleteCases[assoc, 0|0.];

(**************************************************************************************************)

PackageExport["PathVectorElements"]

SetUsage @ "PathVectorElements[path$] returns the %PathElements present in a %PathVector.
"

PathVectorElements[PathVector[assoc_]] := Keys @ assoc;

(**************************************************************************************************)

PackageExport["PathVectorQ"]

SetUsage @ "
PathVectorQ[expr$] gives True if expr$ is a valid PathVector.
"

PathVectorQ[PathVector[_Association]] := $PathAlgebraQ;
PathVectorQ[_] := False;


PathVectorVectorQ[{PathVector[_Association]...}] := $PathAlgebraQ;
PathVectorVectorQ[_] := False;

(**************************************************************************************************)

SetHoldAllComplete[ValidPathAssociationQ];

ValidPathAssociationQ[assoc_Association] /; AssociationQ[Unevaluated[assoc]] :=
  MatchQ[Keys @ assoc, {___PathElement}];

ValidPathAssociationQ[_] := False;

declareFormatting[
  pv:PathVector[_Association ? ValidPathAssociationQ] /; $PathAlgebraQ :>
    formatPathVector[pv]
];

notOverlappingPathsQ[PathVector[assoc_]] := Scope[
  edges = Part[Keys @ assoc, All, 2];
  DuplicateFreeQ @ Flatten[edges /. Negated[e_] :> e, 1]
]

pathFieldQ[PathVector[assoc_]] := DuplicateFreeQ @ PathTailVertex @ Keys[assoc];

notBothwaysQ[_] := False;

formatPathVector[pv_] := If[notOverlappingPathsQ[pv] && pathFieldQ[pv],
  iFormatPathVector[pv, False],
  Mouseover[
    iFormatPathVector[pv, False],
    iFormatPathVector[pv, True]
  ]
]

iFormatPathVector[PathVector[paths_Association], transparency_] := Scope[
  UnpackPathAlgebra[
    vertexCoordinates, edgeCoordinateLists, plotRange, fieldColorFunction,
    vertexRange, vertexList, edgeSetback, imageSize, edgeToCardinal
  ];
  $sb = edgeSetback; $transparency = transparency;
  pathPrimitives = drawWeightedElement @@@ SortBy[
    Normal @ paths,
    -PathLength[First @ #]&
  ];
  pathPrimitives = {
    AbsolutePointSize[4], AbsoluteThickness[1.5],
    pathPrimitives
  };
  initialPathVertices = Keys[paths][[All, 1]];
  remainingVertices = Complement[vertexRange, initialPathVertices];
  vertexPrimitives = {
    AbsolutePointSize[3], $LightGray,
    Point @ Part[vertexCoordinates, remainingVertices]
  };
  Graphics[
    {vertexPrimitives, pathPrimitives},
    Frame -> True, FrameTicks -> False, ImageSize -> imageSize,
    FrameStyle -> LightGray,
    PlotRange -> plotRange, PlotRangePadding -> Scaled[0.15],
    PlotRangeClipping -> True
  ]
];

pathElementForm[PathElement[t_, e_, h_]] :=
  PathWordForm[
    Part[vertexList, t],
    Lookup[edgeToCardinal, e],
    Part[vertexList, h]
  ];

drawWeightedElement[p:PathElement[t_, e_, h_], weight_] :=
  drawStyledPath[
    If[e === {},
      Part[vertexCoordinates, List @ t],
      getEdgeCoords[e]
    ],
    fieldColorFunction @ weight
  ] ~NiceTooltip~ pathElementForm[p];

getEdgeCoords[e_] := Scope[
  coords = Part[edgeCoordinateLists, StripNegated /@ e];
  orientedCoords = MapAt[Reverse, coords, List /@ SelectIndices[e, NegatedQ]];
  Flatten[orientedCoords, 1] //. {l___, a_List, a_List, r___} :> {l, a, r}
];

(* mergeSegments[{segment_}] := segment;
mergeSegments[segments_List] :=
  MapStaggered[segmentJoin, segments]

segmentJoin[a_, b_] := If[Abs[segmentAngle[a] - segmentAngle[b]] > 0.01,
  Echo @ applyBendBetween[a, b, 0.1],
  a
];
 *)
segmentAngle[segment_] := ArcTan @@ (Last[segment] - First[segment]);

drawStyledPath[vertices_, style_] /; $transparency :=
  Mouseover[
    drawSingleStyledPath[vertices, Opacity[.25, style]],
    drawSingleStyledPath[vertices, style]
  ];

drawStyledPath[vertices_, style_] :=
  drawSingleStyledPath[vertices, style]

drawSingleStyledPath[{vertices_}, style_] :=
  Style[Point @ vertices, style];

drawSingleStyledPath[vertices_, style_] :=
  Style[
    {Point @ Part[vertices, 1], myArrow @ SetbackCoordinates[vertices, {0, $sb}]}, style,
    Arrowheads[{{.13, 1, ArrowheadData["Line", style]}}]
  ]

myArrow[{}] = Nothing;
myArrow[Nothing] = Nothing;
myArrow[e_] := Arrow[e];

fieldColors = MatchValues[
  2 := {$Gray, $DarkGray};
  3 := {$Gray, $Red, $Blue};
  4 := {$Gray, $Red, $Purple, $Blue};
  4 := {$Gray, $Red, $DarkRed, $DarkBlue, $Blue};
  n_ := Take[Prepend[$ColorPalette, White], n];
];

(**************************************************************************************************)

PackageExport["PathHeadVertex"]
PackageExport["PathTailVertex"]

PathTailVertex[p_PathElement] := Part[p, 1];
PathTailVertex[list_List] := Part[list, All, 1];

PathHeadVertex[p_PathElement] := Part[p, 3];
PathHeadVertex[list_List] := Part[list, All, 3];

(**************************************************************************************************)

PackageExport["PathHeadVector"]
PackageExport["PathTailVector"]

PathHeadVector[PathElement[_, _, h_]] := PathElement[h, {}, h];

PathHeadVector[PathVector[assoc_]] :=
  rulesToPathVector @ MapAt[PathHeadVector, Normal @ assoc, {All, 1}];

PathTailVector[PathElement[t_, _, _]] := PathElement[t, {}, t];

PathTailVector[PathVector[assoc_]] :=
  rulesToPathVector @ MapAt[PathTailVector, Normal @ assoc, {All, 1}];

(**************************************************************************************************)

PackageExport["NullPath"]
PackageExport["NullElement"]

declareFormatting[
  NullPath :> "\[UpTee]"
  NullElement :> "\[UpTee]"
];


(**************************************************************************************************)

PathVector /: CenterDot[v__PathVector] := PathCompose[v];

(**************************************************************************************************)

PackageExport["PathCompose"]

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
  {l___, i_, Negated[i_], r___} :> {l, r},
  {l___, Negated[i_], i_, r___} :> {l, r}
};

(**************************************************************************************************)

PackageExport["ToPathVector"]

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
  GraphPathData[vertices_, edges_, negations_] :=
    PathElement[
      First @ vertices,
      ApplyNegated[edges, negations],
      Last @ vertices
    ] -> $w;
  GraphRegionAnnotation[r_, <|"Weight" -> w_|>] :=
    Block[{$w = w}, % @ r];
  _ := {};
];

attachWeightedData = Case[
  r_ -> w_    := Weighted[r, w];
  list_List   := Map[%, list];
  other_      := other
];

(**************************************************************************************************)

PackageExport["BasisWordVectors"]

SetUsage @ "
BasisWordVectors[] returns a list of %WordVector[c$i] for each cardinal c$i in the quiver.
"

BasisWordVectors[] /; $PathAlgebraQ := Scope[
  UnpackPathAlgebra[cardinals];
  WordVector /@ cardinals
];

(**************************************************************************************************)

PackageExport["WordVector"]

SetUsage @ "
WordVector['word$'] constructs a %PathVector consisting of all paths that have path word word$.
* The weights are all 1.
* 'word$' should consist of cardinals, or their negations (indicated by uppercase letters).
"

WordVector[words:{__String}] := 
  PathVectorPlus @@ Map[WordVector, words];

WordVector[word_String] /; $PathAlgebraQ := Scope[

  UnpackPathAlgebra[vertexRange, tagOutTable, tagOutEdgeTable, nullVertex, nullEdge];

  word = ParseCardinalWord @ word;
  headVertices = tailVertices = vertexRange;

  CollectTo[{pathEdges},
  Scan[card |-> (
      edgeList = Part[tagOutEdgeTable @ card, headVertices];
      headVertices = Part[tagOutTable @ card, headVertices];
      Internal`StuffBag[pathEdges, edgeList]
    ),
    word
  ]];

  pathEdges = If[pathEdges === {},
    ConstantArray[{}, Length @ vertexRange],
    Transpose @ pathEdges
  ];

  pathElements = MapThread[toWordVectorPathElement, {tailVertices, pathEdges, headVertices}];

  PathVector @ ConstantAssociation[Sort @ pathElements, 1]
]

toWordVectorPathElement[tail_, edges_, head_] :=
  If[FreeQ[edges, nullEdge], PathElement[tail, edges, head], Nothing];

(**************************************************************************************************)

PackageExport["PathVectorComponents"]

PathVectorComponents[PathVector[assoc_]] :=
  PathVector @ Association[#]& /@ Normal[assoc]

PackageExport["PathVectorComponents"]

PathVectorComponents[PathVector[assoc_], parts_] :=
  PathVector @ Association @ Part[Normal[assoc], parts];

(**************************************************************************************************)

PackageExport["ReversalSymmetryDecompose"]

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

PackageExport["ReversalSymmetricQ"]
PackageExport["ReversalAntisymmetricQ"]

ReversalSymmetricQ[p_PathVector] :=
  p === PathReverse[p];

ReversalAntisymmetricQ[p_PathVector] :=
  -p === PathReverse[p];

(**************************************************************************************************)

PackageExport["WordDelta"]

SetUsage @ "
WordDelta['word$', 'type$'] constructs a path vector that when convolved computes the finite difference \
along the word 'word$'.
* 'type$' can be one of:
| 'Forward' | forward finite difference (X - I) |
| 'Reverse' | reverse finite difference (X\[Conjugate] - I) |
| 'Symmetric' | X + X\[Conjugate] - 2 I |
| 'Antisymmetric' | X - X\[Conjugate] |
"

declareFunctionAutocomplete[WordDelta, {0, $directionStrings}];

WordDelta[word_String, type_:"Forward"] /; $PathAlgebraQ := Scope[
  forward = WordVector @ word;
  reverse := PathReverse @ forward;
  unit := VertexField[];
  Switch[type,
    "Forward", forward - unit,
    "Reverse", reverse - unit,
    "Symmetric", forward + reverse + -2 * unit,
    "Antisymmetric", forward - reverse
  ]
]

(**************************************************************************************************)

PackageExport["PathLength"]

SetUsage @ "
PathLength[element$] returns the length of a %PathElement.
"

PathLength[PathElement[_, e_List, _]] := Length @ e;

(**************************************************************************************************)

PackageExport["PathConjugate"]

PathConjugate[pv:PathVector[assoc_]] :=
  If[FreeQ[Values @ assoc, Complex], pv, PathVector[Conjugate /@ assoc]];

(**************************************************************************************************)

PackageExport["PathPower"]

PathPower[PathVector[assoc_], p_] :=
  PathVector[Power[#, p]& /@ assoc];

(**************************************************************************************************)

PackageExport["PathSqrt"]

PathSqrt[PathVector[assoc_]] :=
  PathVector @ Map[Sqrt, assoc];

(**************************************************************************************************)

PackageExport["PathConjugateTranspose"]

PathConjugateTranspose[pv_PathVector] :=
  PathConjugate @ PathReverse @ pv;

(**************************************************************************************************)

PackageExport["PathReverse"]

SetUsage @ "
PathReverse[PathVector[$$]] yields the PathVector[$$] in which all \
paths have been reversed.
PathReverse[PathElement[$$]] yields the reverse of PathElement[$$].
"

PathReverse[PathVector[assoc_]] :=
  PathVector @ KeySort @ KeyMap[PathReverse, assoc]

PathReverse[PathElement[t_, e_, h_]] := PathElement[h, Map[Negated, Reverse @ e], t];

PathReverse[NullElement] := NullElement;

(**************************************************************************************************)

PackageExport["PathElement"]

SetUsage @ "
PathElement[tail$, {edge$1, edge$2, $$}, head$] represents a path starting at vertex tail$, taking \
the given edges, and ending at vertex head$.
"

(**************************************************************************************************)

PackageExport["EmptyPathElement"]

EmptyPathElement[v_] := PathElement[v, {}, v];

(**************************************************************************************************)

PackageExport["VertexField"]

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

VertexField[ints:{__Integer} | _Integer] /; $PathAlgebraQ := Scope[
  UnpackPathAlgebra[pos, neg];
  PathVector @ Association @ Map[
    i |-> (EmptyPathElement[Abs @ i] -> If[Positive[i], pos, neg]),
    Sort @ ToList @ ints
  ]
]

(**************************************************************************************************)

PackageExport["RandomVertexField"]

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

PackageExport["RandomEdgeField"]

SetUsage @ "
RandomEdgeField[] constructs a random edge field, containing every length-1 path with a random weight.
RandomEdgeField['type$'] chooses only specific orientations of length-1 paths.
* The random weights are chosen from the appropriate base field.
* For finite fields, any value is equally likely to be chosen.
* For infinite fields, the range [-2, 2] is used.
* The available orientations are 'Forward', 'Reverse', and 'Symmetric' (default).
"

declareFunctionAutocomplete[RandomEdgeField, {$directionStrings}];

Options[RandomEdgeField] = {RandomSeeding -> Automatic};

RandomEdgeField[type_String:"Symmetric", OptionsPattern[]] /; $PathAlgebraQ := Scope @ RandomSeeded[
  UnpackPathAlgebra[edgeRange, randomFieldElement];
  forward = edgeRange;
  reverse = Negated /@ edgeRange;
  edges = PathElement /@ Switch[type,
    "Forward", forward,
    "Reverse", reverse,
    "Symmetric" | "Antisymmetric", Join[forward, reverse]
  ];
  constructPathVector @ Map[edge |-> singleEdgePathElement[edge] -> randomFieldElement[], edges],
  OptionValue[RandomSeeding]
];

(**************************************************************************************************)

singleEdgePathElement[edge_] :=
  PathElement[edgeToTail @ edge, {edge}, edgeToHead @ edge];

(**************************************************************************************************)

PackageExport["RandomPathField"]

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

RandomPathField[range:{_Integer ? NonNegative, _Integer ? NonNegative} | (_Integer ? NonNegative), opts:OptionsPattern[]] :=
  RandomPathVector[1, range, opts];

(**************************************************************************************************)

PackageExport["SparsifyPathVector"]

SetUsage @ "
SparsifyPathVector[vector$, frac$] deletes frac$ of the paths in a %PathVector.
"

SparsifyPathVector[PathVector[assoc_], frac_Real] :=
  PathVector @ Select[assoc, RandomReal[] > frac&]

(**************************************************************************************************)

PackageExport["ProjectPathLength"]

SetUsage @ "
ProjectPathLength[vector$, n$] returns the path vector consisting of paths of length n$.
"

ProjectPathLength[PathVector[assoc_], n_] :=
  PathVector @ KeySelect[assoc, PathLength[#] === n&]

ProjectPathLength[pv:PathVector[assoc_], All] := Scope[
  max = Max @ Map[PathLength, Keys @ assoc];
  Table[ProjectPathLength[pv, n], {n, 0, max}]
];

(**************************************************************************************************)

PackageExport["RandomPathVector"]

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

RandomPathVector[m_Integer, opts:OptionsPattern[]] :=
  RandomPathVector[m, {0, 2}, opts];

RandomPathVector[m_Integer, range_Integer, opts:OptionsPattern[]] :=
  RandomPathVector[m, {range, range}, opts];

RandomPathVector[m_Integer, range:{_Integer ? NonNegative, _Integer ? NonNegative}, OptionsPattern[]] := Scope @ RandomSeeded[
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

PackageExport["VertexFieldQ"]

VertexFieldQ[PathVector[assoc_Association]] := MatchQ[Keys @ assoc, {PathElement[_, {}, _]...}];
VertexFieldQ[_] := False;

(**************************************************************************************************)

PackageExport["EdgeField"]

SetUsage @ "
EdgeField[] gives EdgeField[All].
EdgeField[All] constructs the sum of all length 1 paths (edge paths).
EdgeField[i$] gives the length 1 path on edge 1.
EdgeField[spec$,'type$'] constructs one of the following:
| 'Forward' | construct path in forward direction  (default) |
| 'Reverse' | construct path in reverse direction |
| 'Symmetric' | uses weight 1 for forward and reverse directions |
| 'Antisymmetric' | uses weight 1 for forward  and -1 for reverses |
"

declareFunctionAutocomplete[EdgeField, {0, $directionStrings}];

EdgeField[] := EdgeField[All];

EdgeField[All] /; $PathAlgebraQ := Scope[
  UnpackPathAlgebra[edgeRange, pos, neg, edgeToTail, edgeToHead];
  PathVector @ ConstantAssociation[
    singleEdgePathElement /@ edgeRange,
    pos
  ]
];

EdgeField[ints:{__Integer} | _Integer] /; $PathAlgebraQ := Scope[
  UnpackPathAlgebra[edgeRange, pos, neg, edgeToTail, edgeToHead];
  ints = ToList @ ints;
  edges = Part[edgeRange, Abs @ ints];
  PathVector @ Association @ MapThread[
    {edge, int} |-> edge -> If[Positive[int], pos, neg],
    {singleEdgePathElement /@ edges, ints}
  ]
];

EdgeField[spec_, "Forward"] := EdgeField[spec];
EdgeField[spec_, "Reverse"] := PathReverse @ EdgeField[spec];

(* EdgeField[spec_, "Symmetric"] := symmetricEdgeField[spec, True];
EdgeField[spec_, "Antisymmetric"] := symmetricEdgeField[spec, False];

symmetricEdgeField[All, sym_] := Scope[
  UnpackPathAlgebra[edgePairs, pos, neg];
  If[sym, neg = pos];
  PathVector @ Association[ (* TODO *)
    {PathElement[#] -> pos, PathElement[Reverse @ #] -> neg}& /@ edgePairs
  ]
];

symmetricEdgeField[i_Integer, sym_] := symmetricEdgeField[{i}, sym];

symmetricEdgeField[ints:{__Integer}, sym_] := Scope[
  UnpackPathAlgebra[edgePairs, pos, neg];
  edges = Part[edgePairs, Abs @ ints];
  If[sym, neg = pos];
  PathVector @ Association @ MapThread[
    {pEdge, nEdge, int} |-> If[Positive[int],
      {pEdge -> pos, nEdge -> neg},
      {pEdge -> neg, nEdge -> pos}
    ],
    {PathElement /@ edges, PathElement /@ Reverse[edges, 2], ints}
  ]
];
 *)
(**************************************************************************************************)

PackageExport["EdgeFieldQ"]

EdgeFieldQ[PathVector[assoc_Association]] := MatchQ[Keys @ assoc, {PathElement[_, {_}, _]...}];
EdgeFieldQ[_] := False;

(**************************************************************************************************)

PackageExport["PathElementToWord"]

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
  Lookup[edgeToCardinal, #]& /@ Part[list, All, 2]
]

(**************************************************************************************************)

PackageExport["PathElementToTailFrameWord"]

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
  Part[elementFrameData @ p, 2];

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

PackageExport["PathElementFromTailFrameWord"]

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

DefineLiteralMacro[setupForTranslation,

  setupForTranslation[] := (
    UnpackPathAlgebra[
      edgeToCardinal, (* elementFrameData *)
      vertexTags, cardinalTransitions, tagOutEdgeTable, nullEdge, edgeToHead (* tailFrameWordToElement *)
    ];
    $frameDataCache = Data`UnorderedAssociation[];
  )
];


(**************************************************************************************************)

PackageExport["PathTranslate"]

PathTranslate[t_PathVector, p_PathVector] /; $PathAlgebraQ := Scope[

  setupForTranslation[];

  BilinearApply[elementTranslate, t -> PathTailVertex, p -> PathTailVertex]
]

elementTranslate[t_PathElement, p_PathElement, anti_:False] := Scope[

  If[PathTailVertex[t] =!= PathTailVertex[p], $Unreachable];

  tFrameData = CacheTo[$frameDataCache, t, elementFrameData @ t];
  pFrameData = CacheTo[$frameDataCache, p, elementFrameData @ p];

  If[FailureQ[tFrameData] || FailureQ[pFrameData], Return @ NullElement];

  pWord = Part[pFrameData, 2];
  tTransport = RuleThread[First @ tFrameData, Last @ tFrameData];

  pWordTransported = Replace[pWord, tTransport, {1, 2}];

  If[anti, pWordTransported = Negated /@ pWordTransported];

  tailFrameWordToElement[
    PathHeadVertex @ t,
    pWordTransported
  ]
]

elementTranslateHead[t_PathElement, p_PathElement, anti_:False] :=
  PathReverse @ elementTranslate[t, PathReverse @ p, anti];

(**************************************************************************************************)

PackageExport["TranslateAdd"]

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

PackageExport["TranslateSubtract"]

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

PackageExport["TranslateHeadSubtract"]

TranslateHeadSubtract[a_PathVector, b_PathVector] := Scope[
  setupForTranslation[];
  BilinearApply[elementTranslateHeadSubtract, a -> PathTailVertex, b -> PathHeadVertex]
];

elementTranslateHeadSubtract[t_PathElement, p_PathElement] :=
  elementCompose[t, PathReverse @ elementTranslateHead[t, p]]

TranslateHeadSubtract[a_PathVector, b_PathVector] :=
  PathReverse @ TranslateSubtract[a, b];

(**************************************************************************************************)

PackageExport["PathInvert"]

SetUsage @ "
PathInvert[vector$] inverts a %PathVector by negating the path word of each of its elements.
"

PathInvert[PathVector[assoc_]] := Scope[
  setupForTranslation[];
  constructPathVector @ KeyMap[invertElement, assoc]
];

invertElement[p:PathElement[t_, edges_, h_]] :=
  tailFrameWordToElement[t, Negated /@ tailFrameWord[p]]

(**************************************************************************************************)

PackageExport["ShortestPathVector"]

SetUsage @ "
ShortestPathVector[vector$] replaces the %PathElement in vector$ with their shortest path equivalents.
"

ShortestPathVector[PathVector[assoc_]] := Scope[
  UnpackPathAlgebra[indexQuiver];
  shortestPath = FindShortestPath[UndirectedGraph @ indexQuiver, All, All];
  edgeIndex = AssociationRange @ EdgePairs @ indexQuiver;
  edgeIndex = Join[edgeIndex, Map[Negated] @ KeyMap[Reverse] @ edgeIndex];
  constructPathVector @ KeyMap[shortestPathElement, assoc]
];

shortestPathElement[PathElement[t_, _, h_]] :=
  PathElement[t, edgeIndex /@ Partition[shortestPath[t, h], 2, 1], h];

(**************************************************************************************************)

PackageExport["BilinearApply"]

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

PackageExport["TranslationCommutator"]

SetUsage @ "
TranslationCommutator[a$, b$] returns (a$ \[CirclePlus] b$) \[CircleMinus] (a$ \[CirclePlus] b$).
* \[CirclePlus] is TranslateAdd.
* \[CircleMinus] is TranslateSubtract.
"

TranslationCommutator[a_, b_] := Scope[
  ab = TranslateAdd[a, b];
  TranslateSubtract[ab, ab]
];

(**************************************************************************************************)

PackageExport["PathForwardDifference"]

PathForwardDifference[flow_, target_] :=
  PathTranslate[flow - PathTailVector[flow], target];

PathForwardDifference[v_][t_] := PathForwardDifference[v, t];

(**************************************************************************************************)

PackageExport["PathBackwardDifference"]

PathBackwardDifference[flow_, target_] :=
  PathTranslate[flow - PathTailVector[flow], target];

PathBackwardDifference[v_][t_] := PathBackwardDifference[v, t];

(**************************************************************************************************)

PackageExport["PathEtaDifference"]

PathEtaDifference[PathVector[flow_], PathVector[target_]] :=
  etaEpsilonDifference[flow, target, False];

PathEtaDifference[v_][t_] := PathEtaDifference[v, t];

(**************************************************************************************************)

PackageExport["PathEpsilonDifference"]

PathEpsilonDifference[PathVector[flow_], PathVector[target_]] :=
  etaEpsilonDifference[flow, target, True];

PathEpsilonDifference[v_][t_] := PathEpsilonDifference[v, t];

(**************************************************************************************************)

etaEpsilonDifference[flow_, target_, isEps_] := Scope[

  setupForTranslation[];

  targets = target;
  {targetPaths, targetWeights} = KeysValues @ Normal @ target;
  tailIndex = PositionIndex @ PathTailVertex @ targetPaths;
  headIndex = If[!isEps, tailIndex, PositionIndex @ PathHeadVertex @ targetPaths];
  $isEps = isEps;
  PathVector @ DeleteCases[0|0.] @ MapIndexed[etaEpsilonElementDifference, flow]
];

etaEpsilonElementDifference[w_, {Key[tpath_PathElement]}] := Scope[
  rpath = PathReverse @ tpath;
  tail = PathTailVertex @ tpath;
  head = PathHeadVertex @ tpath;
  tailTargets = Lookup[tailIndex, tail, {}];
  headTargets = Lookup[headIndex, head, {}];
  If[headTargets === {} && tailTargets === {}, Return @ 0];
  forwardTranslated = Flatten @ Map[elementTranslate[tpath, #]&, Part[targetPaths, tailTargets]];
  elementTranslate2 = If[$isEps, elementTranslateBack, elementTranslate];
  reverseTranslated = Flatten @ Map[elementTranslate2[rpath, #]&, Part[targetPaths, headTargets]];
  {fWeight, bWeight} = Total @ Lookup[targets, #, 0]& /@ {forwardTranslated, reverseTranslated};
  {hWeight, tWeight} = Total @ Part[targetWeights, #]& /@ {headTargets, tailTargets};
  w * ((fWeight - tWeight) - (bWeight - hWeight))
]

elementTranslateBack[p_, q_] := PathReverse @ elementTranslate[p, PathReverse @ q];

(**************************************************************************************************)

PackageExport["SymmetricPathFiniteDifference"]

SymmetricPathFiniteDifference[flow_, target_] :=
  PathTranslate[PathReverse[flow] - flow, target];

SymmetricPathFiniteDifference[v_][t_] := SymmetricPathFiniteDifference[v, t];
