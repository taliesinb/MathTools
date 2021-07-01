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

$PathAlgebra = None;
$PathAlgebraQ := $PathAlgebra =!= None;
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
* field$ can be an integer for a finite field (or ring), or one of %Integers, %Reals, or %Complexes.
"

Options[PathAlgebra] = {
  PlotRange -> Automatic,
  EdgeSetback -> 0.15,
  ImageSize -> 100
};

PathAlgebra[quiver_?GraphQ, field_, OptionsPattern[]] ? System`Private`HoldEntryQ := Scope[
  If[!QuiverQ[quiver], ReturnFailed[]];
  If[!MatchQ[field, _Integer | Integers | Reals | Complexes], ReturnFailed[]];

  UnpackOptions[plotRange, edgeSetback, imageSize];
  If[NumericQ[plotRange], plotRange *= {{-1, 1}, {-1, 1}}];

  data = <||>;
  {vertexCoords, edgeCoords} = ExtractGraphPrimitiveCoordinates @ quiver;

  data["VertexCoordinates"] = vertexCoords;
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

  data["VertexCount"] = count = VertexCount @ quiver;
  data["VertexList"] = VertexRange @ quiver;
  data["Cardinals"] = CardinalList @ quiver;
  data["EdgePairs"] = EdgePairs @ quiver;

  data["NullVertex"] = dummy = count + 1;
  tagOutTable = Append[dummy] /@ ReplaceAll[TagVertexOutTable @ quiver, None -> dummy];
  data["TagOutTable"] = tagOutTable;
  data["AdjacencyTable"] = VertexAdjacencyTable @ quiver;

  System`Private`ConstructNoEntry[PathAlgebra, data]
];

$complexColorFunction = ComplexHue;

ComplexHue[c_] := Hue[Arg[c]/Tau+.05, Min[Sqrt[Abs[c]]/1.5,1]]

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

constructPathVector[assoc_Association] :=
  PathVector @ Association @ KeySort @ KeyDrop[NullElement] @ DeleteCases[assoc, 0|0.];

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

notBothwaysQ[PathVector[assoc_]] := Scope[
  edges = Part[Keys @ assoc, All, 1];
  DuplicateFreeQ @ Join[edges, Reverse[edges, 2]]
]

notBothwaysQ[_] := False;

formatPathVector[pv_] := If[VertexFieldQ[pv] || (EdgeFieldQ[pv] && notBothwaysQ[pv]),
  iFormatPathVector[pv, False],
  Mouseover[
    iFormatPathVector[pv, False],
    iFormatPathVector[pv, True]
  ]
]

iFormatPathVector[PathVector[paths_Association], transparency_] := Scope[
  UnpackPathAlgebra[vertexCoordinates, plotRange, fieldColorFunction, vertexList, edgeSetback, imageSize];
  $sb = edgeSetback; $transparency = transparency;
  pathPrimitives = drawWeightedElement @@@ SortBy[
    Normal @ paths,
    -PathLength[First @ #]&
  ];
  pathPrimitives = {
    AbsolutePointSize[4], AbsoluteThickness[1.5],
    pathPrimitives
  };
  initialPathVertices = Keys[paths][[All, 1, 1]];
  remainingVertices = Complement[vertexList, initialPathVertices];
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

drawWeightedElement[PathElement[element_], weight_] :=
  drawStyledPath[
    Part[vertexCoordinates, element],
    fieldColorFunction @ weight
  ];

drawStyledPath[vertices_, style_] /; $transparency :=
  drawSingleMouseverStyledPath[vertices, style];

drawSingleMouseverStyledPath[vertices_, style_] :=
  Mouseover[
    drawSingleStyledPath[vertices, Opacity[.25, style]],
    drawSingleStyledPath[vertices, style]
  ];

drawSingleStyledPath[vertices_, style_] :=
  Style[
    drawSinglePathPrimitives @ vertices, style,
    Arrowheads[{{.13, 1, ArrowheadData["Line", style]}}]
  ]

drawSinglePathPrimitives[vertices_] := {
  Point @ Part[vertices, 1],
  myArrow[processPathSegments @ vertices]
};

drawStyledPath[vertices_, style_] :=
  Style[
    drawSinglePathPrimitives @ vertices, style,
    Arrowheads[{{.13, 1, ArrowheadData["Line", style]}}]
  ]

myArrow[{}] = Nothing;
myArrow[Nothing] = Nothing;
myArrow[e_] := Arrow[e];

processPathSegments[{_}] := Nothing;
processPathSegments[list:{_, _}] := SetbackCoordinates[list, {0, $sb}];
processPathSegments[list_] := Scope[$sa = 0; processPathSegment @@@ Partition[list, 3, 1]];

processPathSegment[a_, b_, c_] := Scope[
  ab = SetbackCoordinates[{a, b}, {$sa, $sb}]; $sa ^= $sb;
  bc = SetbackCoordinates[{b, c}, {$sa, $sb}];
  Splice @ Join[ab, bc]
];

fieldColors = MatchValues[
  2 := {$Gray, $DarkGray};
  3 := {$Gray, $Red, $Blue};
  4 := {$Gray, $Red, $Purple, $Blue};
  4 := {$Gray, $Red, $DarkRed, $DarkBlue, $Blue};
  n_ := Take[Prepend[$ColorPalette, White], n];
];

(**************************************************************************************************)

PackageExport["PathHead"]
PackageExport["PathTail"]

PathTail[PathElement[list_List]] := First @ list;
PathHead[PathElement[list_List]] := Last @ list;

(**************************************************************************************************)

PackageExport["NullPath"]
PackageExport["NullElement"]

declareFormatting[
  NullPath :> "\[UpTee]"
  NullElement :> "\[UpTee]"
];


(**************************************************************************************************)

PathVector /: CenterDot[v__PathVector] := PathCompose[v];

PathBivector /: CenterDot[b__PathBivector] := PathCompose[b];

(**************************************************************************************************)

PackageExport["PathCompose"]

$cancelRules = Dispatch @ {
  {l___, i_, j_, i_, r___} :> {l, i, r}
};

compose[a_, b_] := PathElement[Join[a, Rest @ b] //. $cancelRules];

compose[a_, b_, c_] := PathElement[Join[a, Rest @ b, Rest @ c] //. $cancelRules];

PathCompose[e_] := e;

PathCompose[PathElement[a_], PathElement[b_], PathElement[c_]] :=
  If[Last[a] === First[b] && Last[b] === First[c], compose[a, b, c], NullElement];

PathCompose[PathElement[a_], PathElement[b_]] :=
  If[Last[a] === First[b], compose[a, b], NullPath];

PathCompose[a_PathVector, b_PathVector, c_PathVector, rest___PathVector] /; $PathAlgebraQ :=
  PathCompose[PathCompose[PathCompose[a, b], c], rest];

PathCompose[PathVector[a1_Association], PathVector[a2_Association]] /; $PathAlgebraQ := Scope[
  $times = $FieldTimes;
  constructPathVector @ Merge[
    Flatten @ Outer[composeWeightedPaths, Normal @ a1, Normal @ a2, 1],
    Apply @ $FieldPlus
  ]
]

PathCompose[PathBivector[l1_, r1_], PathBivector[l2_, r2_]] /; $PathAlgebraQ :=
  PathBivector[PathCompose[l1, l2], PathCompose[r1, r2]]

composeWeightedPaths[PathElement[a_] -> aw_, PathElement[b_] -> bw_] :=
  If[Last[a] === First[b], compose[a, b] -> $times[aw, bw], {}];

(**************************************************************************************************)

PackageExport["WordVector"]

SetUsage @ "
WordVector['word$'] constructs a %PathVector consisting of all paths that have path word word$.
* The weights are all 1.
* 'word$' should consist of cardinals, or their negations (indicated by uppercase letters).
"

foobar

WordVector[word_String] /; $PathAlgebraQ := Scope[
  UnpackPathAlgebra[vertexList, tagOutTable, nullVertex];
  word = ParseCardinalWord @ word;

  pathVertices = Internal`Bag[{vertexList}];
  Scan[card |-> (
    vertexList = Part[tagOutTable @ card, vertexList];
    Internal`StuffBag[pathVertices, vertexList]),
    word
  ];

  pathVertices = Select[FreeQ[#, nullVertex]&] @ Transpose @ Internal`BagPart[pathVertices, All];
  pathElements = PathElement /@ pathVertices;

  PathVector[ConstantAssociation[pathElements, 1]]
]

(**************************************************************************************************)

PackageExport["WordDelta"]

declareFunctionAutocomplete[WordDelta, {0, $directionStrings}];

WordDelta[word_String, type_:"Forward"] /; $PathAlgebraQ := Scope[
  forward = WordVector @ word;
  reverse := PathReverse @ forward;
  unit := VertexField[];
  Switch[type,
    "Forward", forward - unit,
    "Reverse", reverse - unit,
    "Symmetric", forward - reverse
  ]
]

(**************************************************************************************************)

PackageExport["PathLength"]

PathLength[PathElement[list_List]] := Length @ list;

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
  PathVector @ KeyMap[PathReverse, assoc]

PathReverse[PathBivector[l_PathVector, r_PathVector]] :=
  PathBivector[PathReverse @ l, PathReverse @ r];

PathReverse[PathElement[list_]] := PathElement @ Reverse @ list;

(**************************************************************************************************)

PackageExport["PathElement"]

SetUsage @ "
PathElement[{v$1, v$2, $$, v$n}] represents a path starting at v$1 and ending at v$n.
"

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
    PathElement[{#}]& /@ $PathAlgebra["VertexList"],
    1
  ];

VertexField[i_Integer] := VertexField[{i}];

VertexField[ints:{__Integer}] /; $PathAlgebraQ := Scope[
  UnpackPathAlgebra[pos, neg];
  PathVector @ Association @ Map[i |-> PathElement[{Abs @ i}] -> If[Positive[i], pos, neg], ints]
]


(**************************************************************************************************)

PackageExport["RandomVertexField"]

SetUsage @ "
RandomVertexField[] constructs a random vertex field, containing every empty path with a random weight.
* The random weights are chosen from the appropriate base field.
* For finite fields, any value is equally likely to be chosen.
* For infinite fields, the range [-2, 2] is used.
"

RandomVertexField[] /; $PathAlgebraQ := Scope[
  UnpackPathAlgebra[vertexList, randomFieldElement];
  constructPathVector @ Map[v |-> PathElement[{v}] -> randomFieldElement[], vertexList]
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

RandomEdgeField[type_:"Symmetric"] /; $PathAlgebraQ := Scope[
  UnpackPathAlgebra[edgePairs, randomFieldElement];
  forward = edgePairs;
  reverse := Reverse[edgePairs, 2];
  pairs = PathElement /@ Switch[type,
    "Forward", forward,
    "Reverse", reverse,
    "Symmetric" | "Antisymmetric", Join[forward, reverse]
  ];
  constructPathVector @ Map[elem |-> elem -> randomFieldElement[], pairs]
];

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

RandomPathField[] :=
  RandomPathVector[1];

RandomPathField[range:{_Integer ? NonNegative, _Integer ? NonNegative} | (_Integer ? NonNegative)] :=
  RandomPathVector[1, range];

(**************************************************************************************************)

PackageExport["SparsifyPathVector"]

SetUsage @ "
SparsifyPathVector[vector$, frac$] deletes frac$ of the paths in a %PathVector.
"

SparsifyPathVector[PathVector[assoc_], frac_Real] :=
  PathVector @ Select[assoc, RandomReal[] > frac&]

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

RandomPathVector[m_Integer] :=
  RandomPathVector[m, {0, 2}];

RandomPathVector[m_Integer, range_Integer] := RandomPathVector[m, {range, range}];

RandomPathVector[m_Integer, range:{_Integer ? NonNegative, _Integer ? NonNegative}] := Scope[
  UnpackPathAlgebra[vertexList, adjacencyTable, randomFieldElement];
  constructPathVector @ Map[
    v |-> Table[
        len = RandomInteger[range];
        PathElement[NestList[chooseNext, v, len]] -> randomFieldElement[]
      ,
        {m}
      ],
    vertexList
  ]
];

chooseNext[v_] := RandomChoice @ Part[adjacencyTable, v]

(**************************************************************************************************)

PackageExport["VertexFieldQ"]

VertexFieldQ[PathVector[assoc_Association]] := MatchQ[Keys @ assoc, {PathElement[{_}]...}];
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

EdgeField[All] := Scope[
  UnpackPathAlgebra[edgePairs, pos, neg];
  PathVector @ ConstantAssociation[
    PathElement[#]& /@ edgePairs,
    pos
  ]
];

EdgeField[i_Integer] := EdgeField[{i}];

EdgeField[ints:{__Integer}] := Scope[
  UnpackPathAlgebra[edgePairs, pos, neg];
  edges = Part[edgePairs, Abs @ ints];
  PathVector @ Association @ MapThread[
    {edge, int} |-> edge -> If[Positive[int], pos, neg],
    {PathElement /@ edges, ints}
  ]
];

EdgeField[spec_, "Forward"] := EdgeField[spec];
EdgeField[spec_, "Reverse"] := PathReverse @ EdgeField[spec];

EdgeField[spec_, "Symmetric"] := symmetricEdgeField[spec, True];
EdgeField[spec_, "Antisymmetric"] := symmetricEdgeField[spec, False];

symmetricEdgeField[All, sym_] := Scope[
  UnpackPathAlgebra[edgePairs, pos, neg];
  If[sym, neg = pos];
  PathVector @ Association[
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

(**************************************************************************************************)

PackageExport["EdgeFieldQ"]

EdgeFieldQ[PathVector[assoc_Association]] := MatchQ[Keys @ assoc, {PathElement[{_, _}]...}];
EdgeFieldQ[_] := False;


(**************************************************************************************************)

PackageExport["PathBivector"]

SetUsage @ "
PathBivector[$$] represents a path bivector on a cardinal quiver.
"

PathBivector[a_PathVector] :=
  PathBivector[a, a];

declareFormatting[
  bv:PathBivector[_PathVector, _PathVector] /; $PathAlgebra =!= None :>
    formatPathBivector[bv]
];

formatPathBivector[PathBivector[a_PathVector, b_PathVector]] :=
  AngleBracket[a, b];

(**************************************************************************************************)

PackageExport["PathBivectorQ"]

PathBivectorQ[PathBivector[a_ ? PathVectorQ, b_ ? PathVectorQ]] := True;
PathBivectorQ[_] := False;

declarePlusTimesDispatch[PathBivector, PathBivectorQ, $pathBivectorDispatch]

$pathBivectorDispatch = <|
  Plus -> PathBivectorPlus,
  Times -> PathBivectorTimes
|>;

PackageExport["PathBivectorPlus"]
PackageExport["PathBivectorTimes"]

PathBivectorPlus[a_, b_, c_] := PathBivectorPlus[PathBivectorPlus[a, b], c];

PathBivectorPlus[PathBivector[a_, b_], PathBivector[c_, d_]] :=
  PathBivector[PathVectorPlus[a, c], PathVectorPlus[b, d]];

PathBivectorTimes[PathBivector[a_, b_], PathBivector[c_, d_]] :=
  PathBivector[PathVectorTimes[a, c], PathVectorTimes[b, d]];

PathBivectorTimes[n_ ? NumericQ, PathBivector[a_, b_]] := PathBivector[
  PathVectorTimes[n, a],
  PathVectorTimes[n, b]
];

(**************************************************************************************************)

PackageExport["ConvolutionBivector"]

SetUsage @ "
ConvolutionBivector[vector$] gives the Bivector[$$] that achieves a convolution with vector$.
"

ConvolutionBivector[pv_PathVector] :=
  PathBivector[pv, pv];

(**************************************************************************************************)

PackageExport["TranslationBivector"]

SetUsage @ "
TranslationBivector[word$] gives the Bivector[$$] that achieves the translation by the cardinal word.
"

TranslationBivector[w_String] :=
  ConvolutionBivector @ WordVector @ w;

(**************************************************************************************************)

PackageExport["PathConvolution"]

SetUsage @ "
PathConvolution[k$, v$] gives the resulting of convolving path kernel k$ with v$.
"

PathConvolution[k_PathVector, v_PathVector] := PathCompose[k, PathCompose[v, PathReverse @ k]]

(**************************************************************************************************)

PackageExport["Sandwich"]

SetUsage @ "
Sandwich[bivector$, vector$] computes the sandwich product of a bivector with a vector.
"

Sandwich[PathBivector[l_PathVector, r_PathVector], PathVector[m_Association]] /; $PathAlgebraQ := Scope[
  s = Normal @ First @ (l + r);
  a = Normal @ First @ (l - r);
  m = Normal @ KeySelect[m, PathHead[#] === PathTail[#]&];
  UnpackPathAlgebra[fieldPlus];
  temp = None;
  z1 = constructPathVector @ mergeWeights[Outer[symmetricSandwichProduct, s, m], 0, Apply @ fieldPlus];
  z2 = constructPathVector @ mergeWeights[Outer[symmetricSandwichProduct, a, m], 0, Apply @ fieldPlus];
  z1 + z2
];

symmetricSandwichProduct[s:PathElement[sv_] -> sw_, m:PathElement[mv_] -> mw_] := (
  temp = PathCompose[s, m, PathReverse @ s];
  If[temp === NullElement, Nothing, temp -> (sw * mw)/2]
)

PathBivector /: CircleDot[a_PathBivector, v_PathVector] := Sandwich[a, v];
