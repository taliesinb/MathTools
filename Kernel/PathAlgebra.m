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
  EdgeSetback -> 0.15,
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
  data["EdgePairs"] = pairs = EdgePairs @ quiver;
  data["EdgePairIndex"] = Join[AssociationRange @ pairs, -AssociationRange[Reverse[pairs, 2]]];

  data["NullVertex"] = dummy = count + 1;
  tagOutTable = Append[dummy] /@ ReplaceAll[TagVertexOutTable @ quiver, None -> dummy];
  data["TagOutTable"] = tagOutTable;
  data["AdjacencyTable"] = VertexAdjacencyTable @ quiver;
  data["VertexTags"] = VertexTagTable @ quiver;
  data["EdgeTags"] = edgeTags = EdgeTags @ quiver;
  data["InOutEdges"] = VertexInOutEdgeTable @ quiver;
  data["VertexRewrites"] = calculateVertexRewrites[data];

  tagLists = Replace[edgeTags, {CardinalSet[c_] :> c, c_ :> {c}}, {1}];
  data["EdgeTagLists"] = tagLists;
  data["PairTagLists"] = AssociationThread[
    Join[pairs, Reverse[pairs, 2]],
    Join[tagLists, Map[Negated, tagLists, {2}]]
  ];

  System`Private`ConstructNoEntry[PathAlgebra, data]
];

calculateVertexRewrites[algebra_] := Scope[
  inOutEdges = algebra["InOutEdges"];
  edgeTags = algebra["EdgeTags"];
  vertexCardinalClasses = Apply[
    {inEdges, outEdges} |-> Join[
      Cases[Part[edgeTags, inEdges], CardinalSet[s_] :> Map[Negated, s]],
      Cases[Part[edgeTags, outEdges], CardinalSet[s_] :> s]
    ],
    inOutEdges, {1}
  ];
  Map[
    classes |-> Flatten @ Map[
      class |-> Outer[
        If[#1 =!= #2, #1 -> #2, {}]&,
        class, class, 1
      ],
      classes
    ],
    vertexCardinalClasses
  ]
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

rulesToPathVector[list_List] := constructPathVector @ Merge[Flatten @ list, Apply @ $FieldTimes];

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

PackageExport["PathHeadVertex"]
PackageExport["PathTailVertex"]

PathTailVertex[PathElement[list_List]] := First @ list;
PathHeadVertex[PathElement[list_List]] := Last @ list;

(**************************************************************************************************)

PackageExport["PathHeadVector"]
PackageExport["PathTailVector"]

PathHeadVector[PathElement[list_List]] := PathElement @ Take[list, 1];

PathHeadVector[PathVector[assoc_]] :=
  rulesToPathVector @ MapAt[PathHeadVector, Normal @ assoc, {All, 1}];

PathTailVector[PathElement[list_List]] := PathElement @ Take[list, -1];

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

PathCompose[a_PathVector, b_PathVector] /; $PathAlgebraQ :=
  BilinearApply[PathCompose, a -> PathHeadVertex, b -> PathTailVertex];

(* defined on elements *)

PathCompose[PathElement[a_], PathElement[b_]] :=
  If[Last[a] === First[b], compose[a, b], NullPath];

$cancelRules = Dispatch @ {
  {l___, i_, j_, i_, r___} :> {l, i, r}
};

compose[a_, b_] := PathElement[Join[a, Rest @ b] //. $cancelRules];

compose[a_, b_, c_] := PathElement[Join[a, Rest @ b, Rest @ c] //. $cancelRules];

(* non-binary versions *)

PathCompose[e_] := e;

PathCompose[PathElement[a_], PathElement[b_], PathElement[c_]] :=
  If[Last[a] === First[b] && Last[b] === First[c], compose[a, b, c], NullElement];

PathCompose[a_PathVector, b_PathVector, c_PathVector, rest___PathVector] /; $PathAlgebraQ :=
  PathCompose[PathCompose[PathCompose[a, b], c], rest];

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

PackageExport["PathComplexRotation"]

PathComplexRotation[PathVector[assoc_Association]] /; $PathAlgebraQ := Scope[
  UnpackPathAlgebra[edgePairIndex]; temp = None;
  constructPathVector @ KeyValueMap[complexRotatePath, assoc]
];

complexRotatePath[elem_, w_] :=
  elem -> w;

complexRotatePath[PathElement[pair:{_, _}], w_] := (
  temp = edgePairIndex @ pair;
  PathElement[Reverse @ pair] -> If[Positive[temp], -w, w]
);

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

PathLength[PathElement[list_List]] := Length[list] - 1;

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

VertexField[ints:{__Integer} | _Integer] /; $PathAlgebraQ := Scope[
  UnpackPathAlgebra[pos, neg];
  PathVector @ Association @ Map[i |-> PathElement[{Abs @ i}] -> If[Positive[i], pos, neg], ToList @ ints]
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

EdgeField[All] /; $PathAlgebraQ := Scope[
  UnpackPathAlgebra[edgePairs, pos, neg];
  PathVector @ ConstantAssociation[
    PathElement[#]& /@ edgePairs,
    pos
  ]
];

EdgeField[ints:{__Integer} | _Integer] /; $PathAlgebraQ := Scope[
  UnpackPathAlgebra[edgePairs, pos, neg];
  ints = ToList @ ints;
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

PackageExport["MultiWordVector"]

MultiWordVector[vertex_, word_] /; $PathAlgebraQ := Scope[
  UnpackPathAlgebra[quiver, nullVertex, tagOutTable, vertexTags, vertexRewrites];

  initialCards = Part[vertexTags, vertex];
  frame = AssociationThread[initialCards, initialCards];

  word = ParseCardinalWord @ word;
  CollectTo[{$paths},
    enumeratePaths[{vertex}, vertex, word, frame];
  ];
  elements = Map[PathElement, Union @ $paths];
  PathVector @ ConstantAssociation[elements, 1]
]

enumeratePaths[path_, vertex_, word_, frame_] := Scope[
  {frameCard, word} = FirstRest @ word;
  rewrites = possibleRewrites[Values @ frame, vertex];
  Scan[rewrite |-> (
    newFrame = applyRewrite[frame, rewrite];
    card = newFrame[frameCard];
    If[MissingQ @ card, Return[]];
    next = Part[tagOutTable, Key @ card, vertex];
    If[next =!= nullVertex,
      enumeratePaths[Append[path, next], next, word, newFrame]];
  ), rewrites];
];

enumeratePaths[path_, _, {}, _] :=
  Internal`StuffBag[$paths, path];

applyRewrite[frame_, {}] := frame;

applyRewrite[frame_, rewrite_] :=
  Replace[frame, rewrite, {1}];

possibleRewrites[keys_, vertex_] := Scope[
  rewrites = Part[vertexRewrites, vertex];
  rewrites = Select[rewrites, MemberQ[keys, First[#]]&];
  Select[Subsets @ rewrites, Keys /* DuplicateFreeQ]
];

possibleRewrites[keys_, vertex_] := Scope[
  rewrites = Part[vertexRewrites, vertex];
  rewrites = Select[rewrites, MemberQ[keys, First[#]]&];
  Select[Subsets @ rewrites, Keys /* DuplicateFreeQ]
];

(**************************************************************************************************)

PackageExport["PathElementToWord"]

PathElementToWord[PathElement[vertices_]] := Scope[
  UnpackPathAlgebra[vertexTags, vertexRewrites, pairTagLists];

  vertex = First @ vertices;
  initialCards = Part[vertexTags, vertex];
  frame = AssociationThread[initialCards, initialCards];

  CollectTo[{$words},
    enumerateWords[{}, vertices, frame];
  ];
  DeleteDuplicates @ $words
];

enumerateWords[word_, {v_}, _] := (
  Internal`StuffBag[$words, word];
);

enumerateWords[word_, vertices_, frame_] := Scope[
  pair = Take[vertices, 2];
  vertices = Drop[vertices, 1];
  vertex = First @ pair;
  pairTags = pairTagLists[pair];
  cards = DeleteMissing @ Map[tag |-> keyOf[frame, tag], pairTags];
  If[cards === {}, Return[]];
  rewrites = possibleRewrites[Values @ frame, vertex];
  cards //= If[Length[cards] === 1, First, CardinalSet];
  Scan[rewrite |-> (
    newFrame = applyRewrite[frame, rewrite];
    enumerateWords[Append[word, cards], vertices, newFrame];
  ), rewrites];
];

keysOf[assoc_, value_] := Part[IndicesOf[assoc, value], All, 1];
keyOf[assoc_, value_] := Replace[IndexOf[assoc, value], Key[k_] :> k];

(**************************************************************************************************)

PackageExport["PathTranslate"]

PathTranslate[t_PathVector, p_PathVector] /; $PathAlgebraQ := Scope[

  UnpackPathAlgebra[nullVertex, tagOutTable, vertexTags, vertexRewrites, pairTagLists];

  BilinearApply[elementTranslate, t -> PathTailVertex, p -> PathTailVertex]
]

(* fast path *)
(* elementTranslate[PathElement[t_], PathElement[p_]] :=
  PathElement @ Take[t, -1];
 *)
elementTranslate[PathElement[tVertices_], p_PathElement] := Scope[
  pWords = PathElementToWord[p] /. CardinalSet[s_] :> First[s];

  {tTail, tHead} = FirstLast @ tVertices;
  initialCards = Part[vertexTags, tTail];
  initialFrame = AssociationThread[initialCards, initialCards];

  CollectTo[{$frames},
    enumerateTranslatedFrames[tVertices, initialFrame];
  ];

  $frames //= DeleteDuplicates;
  relevantCardinals = DeleteDuplicates @ Flatten @ pWords;
  $frames = DeleteDuplicates @ KeyTake[$frames, relevantCardinals];

  CollectTo[{$paths},
    Outer[
      {word, frame} |-> enumeratePaths[{tHead}, tHead, word, frame],
      pWords, $frames, 1
    ];
  ];

  Map[PathElement, DeleteDuplicates @ $paths]
]

enumerateTranslatedFrames[{vertex_}, frame_] :=
  Internal`StuffBag[$frames, frame];

enumerateTranslatedFrames[vertices_, frame_] := Scope[
  pair = Take[vertices, 2];
  vertices = Drop[vertices, 1];
  vertex = First @ pair;
  pairTags = pairTagLists[pair];
  cards = DeleteMissing @ Map[tag |-> keyOf[frame, tag], pairTags];
  If[cards === {}, Return[]];
  rewrites = possibleRewrites[Values @ frame, vertex];
  cards //= If[Length[cards] === 1, First, CardinalSet];
  Scan[rewrite |-> (
    newFrame = applyRewrite[frame, rewrite];
    enumerateTranslatedFrames[vertices, newFrame];
  ), rewrites];
];

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

(* (**************************************************************************************************)

PackageExport["FramedPathElement"]

SetUsage @ "
FramedPathElement[vertices$, frames$, indices$] represents a framed path.
* vertices$ is a list of n$ vertices.
* frames$ is a list of n$ cardinal tuples.
* indices$ is a list of n$ - 1 frame indices.
"

PathCompose[FramedPathElement[va_, fa_, ia_], FramedPathElement[vb_, fb_, ib_]] := Scope[
  If[Last[va] =!= First[vb] || Last[fa] =!= First[fb], Return @ NullElement];
  FramedPathElement[
    Join[va, Rest @ vb],
    Join[fa, Rest @ fb],
    Join[ia, ib]
  ]
];

declareFormatting[
  FramedPathElement[v_List, f_List, i_List] /; Length[v] === Length[f] === (Length[i] + 1) :>
    formatFramedPathElement[v, f, i]
];

formatFramedPathElement[v_List, f_List, i_List] :=
  LabelForm @ Row @ Riffle[
    MapThread[Underscript[#1, Row[#2]]&, {v, f}],
    Underscript[" \[LongRightArrow] ", Style[#, Gray]]& /@ i
  ];

 *)(**************************************************************************************************)

(* PackageExport["PathTranslate"]

PathTranslate[FramedPathElement[va_, fa_, ia_], FramedPathElement[vb_, fb_, ib_]] := Scope[
  If[First[va] =!= First[vb] || First[fa] =!= First[fb], Return @ NullElement];
  FramedPathElement[
];

apl
 *)
(* algorithm: 
1. path B starts where path A ends.
2. convert indices of path b back into cardinals
3. construct a fresh path, doing a branch-out whenever cardinal transport is available.
4. cardinal transport is defined per-vertex: we need to setup an association of rules, that says for each vertex
   what replacements can be made, e.g. v1 -> {a -> b,
*)