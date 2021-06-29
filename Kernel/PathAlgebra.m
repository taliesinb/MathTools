declarePlusTimesDispatch[symbol_Symbol, test_, dispatch_] := (
  symbol /: Plus[objects:Repeated[_symbol ? test, {2, Infinity}]] := dispatch[Plus][objects];
  symbol /: Times[scalar_ /; NumericQ[Unevaluated @ scalar], object_symbol ? test] := dispatch[Times][scalar, object];
  symbol /: Times[object1_symbol ? test, object2_symbol ? test] := dispatch[Times][object1, object2];
)

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
    $redBlueColorFunction
  ];

  data["VertexCount"] = count = VertexCount @ quiver;
  data["VertexList"] = VertexRange @ quiver;
  data["Cardinals"] = CardinalList @ quiver;
  data["EdgePairs"] = EdgePairs @ quiver;

  data["NullVertex"] = dummy = count + 1;
  tagOutTable = Append[dummy] /@ ReplaceAll[TagVertexOutTable @ quiver, None -> dummy];
  data["TagOutTable"] = tagOutTable;

  System`Private`ConstructNoEntry[PathAlgebra, data]
];

$redBlueColorFunction = ChooseContinuousColorFunction[{-2, 2}];

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

PackageExport["ConvolutionBivector"]

SetUsage @ "
ConvolutionBivector[vector$] gives the Bivector[$$] that achieves a convolution with vector$.
"

ConvolutionBivector[p_] := PathBivector[p, p];

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

Sandwich[PathBivector[a_PathVector, b_PathVector], v_PathVector] :=
  PathCompose[a, PathCompose[v, PathReverse @ b]];

PathBivector /: CircleDot[bv_PathBivector, v_PathVector] := Sandwich[bv, v];

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
  PathVector @ dropZero @ Merge[KeyUnion[assocs, id&], f]
];

dropZero[assoc_] := DeleteCases[assoc, 0|0.];

(**************************************************************************************************)

PackageExport["PathHead"]
PackageExport["PathTail"]

PathTail[PathElement[list_List]] := First @ list;
PathHead[PathElement[list_List]] := Last @ list;

(**************************************************************************************************)

PackageExport["NullPath"]

declareFormatting[
  NullPath :> "\[UpTee]"
];

(**************************************************************************************************)

Unprotect[NonCommutativeMultiply];

NonCommutativeMultiply[pv_PathVector] := pv;

NonCommutativeMultiply[a:Repeated[_PathVector, {2, Infinity}]] :=
  Fold[PathCompose, {a}];

Protect[NonCommutativeMultiply];

(**************************************************************************************************)

PackageExport["PathCompose"]

$cancelRules = Dispatch @ {
  {l___, i_, j_, i_, r___} :> {l, i, r}
};

compose[a_, b_] := PathElement[Join[a, Rest @ b] //. $cancelRules];

PathCompose[PathElement[a_], PathElement[b_]] :=
  If[Last[a] === First[b], compose[a, b], NullPath];

PathCompose[PathVector[a1_Association], PathVector[a2_Association]] /; $PathAlgebraQ := Scope[
  $times = $FieldTimes;
  assoc = dropZero @ Merge[
    Flatten @ Outer[composeWeightedPaths, Normal @ a1, Normal @ a2, 1],
    Apply @ $FieldPlus
  ];
  PathVector @ assoc
]

composeWeightedPaths[PathElement[a_] -> aw_, PathElement[b_] -> bw_] :=
  If[Last[a] === First[b], compose[a, b] -> $times[aw, bw], {}];

(**************************************************************************************************)

declareFormatting[
  pv:PathVector[_Association] /; $PathAlgebraQ :>
    formatPathVector[pv]
];

DefineMacro[UnpackPathAlgebra,
UnpackPathAlgebra[args___] := Quoted @ UnpackAssociation[getObjectData @ $PathAlgebra, args]
];

formatPathVector[pv_] := If[VertexFieldQ[pv] || EdgeFieldQ[pv],
  iFormatPathVector[pv, False],
  Mouseover[
    iFormatPathVector[pv, False],
    iFormatPathVector[pv, True]
  ]
]

iFormatPathVector[PathVector[paths_Association], transparency_] := Scope[
  UnpackPathAlgebra[vertexCoordinates, plotRange, fieldColorFunction, vertexList, edgeSetback, imageSize];
  $sb = edgeSetback; $transparency = transparency;
  pathPrimitiveGroups = Merge[Reverse[Normal @ paths, {2}], Identity];
  pathPrimitives = KeyValueMap[
    {weight, elements} |->
      drawStyledPaths[
        Map[Part[vertexCoordinates, #]&, SortBy[First /@ elements, Length /* Minus]],
        fieldColorFunction @ weight
      ],
    pathPrimitiveGroups
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

drawStyledPaths[vertices_, style_] /; $transparency :=
  drawSingleMouseverStyledPath[#, style]& /@ vertices;

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


drawStyledPaths[vertices_, style_] :=
  Style[
    drawPathPrimitives @ vertices, style,
    Arrowheads[{{.13, 1, ArrowheadData["Line", style]}}]
  ]

drawPathPrimitives[vertices_] := {
  Point @ Part[vertices, All, 1],
  myArrow[processPathSegments /@ vertices]
};

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

PackageExport["WordVector"]

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

PackageExport["WordDirective"]

WordDirective[word_String, type_:"Forward"] /; $PathAlgebraQ := Scope[
  forward = WordVector @ word;
  reverse := PathReverse @ forward;
  unit := VertexField[];
  Switch[type,
    "Forward", forward - unit,
    "Reverse", unit - reverse,
    "Symmetric", forward - reverse
  ]
]

(**************************************************************************************************)

PackageExport["PathReverse"]

SetUsage @ "
PathReverse[PathVector[$$]] yields the PathVector[$$] in which all \
paths have been reversed.
PathReverse[PathElement[$$]] yields the reverse of PathElement[$$].
"

PathReverse[PathVector[assoc_]] :=
  PathVector @ KeyMap[PathReverse, assoc]

PathReverse[PathElement[list_]] := PathElement @ Reverse @ list;

(**************************************************************************************************)

PackageExport["PathElement"]

SetUsage @ "
PathElement[{v$1, v$2, $$, v$n}] represents a path starting at v$1 and ending at v$n.
"

(**************************************************************************************************)

PackageExport["VertexField"]

SetUsage @ "
VertexField[] constructs the unit vertex field.
VertexField[i$] constructs the basis vertex field with weight 1 for vertex $i, and 0 elsewhere.
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

RandomVertexField[] /; $PathAlgebraQ := Scope[
  UnpackPathAlgebra[vertexList, randomFieldElement];
  PathVector @ dropZero @ Association @ Map[v |-> PathElement[{v}] -> randomFieldElement[], vertexList]
];

(**************************************************************************************************)

PackageExport["RandomEdgeField"]

RandomEdgeField[type_:"Forward"] /; $PathAlgebraQ := Scope[
  UnpackPathAlgebra[edgePairs, randomFieldElement];
  forward = edgePairs;
  reverse := Reverse[edgePairs, 2];
  pairs = PathElement /@ Switch[type,
    "Forward", forward,
    "Reverse", reverse,
    "Symmetric", Join[forward, reverse]
  ];
  PathVector @ dropZero @ Association @ Map[elem |-> elem -> randomFieldElement[], pairs]
];

(**************************************************************************************************)

PackageExport["VertexFieldQ"]

VertexFieldQ[PathVector[assoc_Association]] := MatchQ[Keys @ assoc, {PathElement[{_}]...}];
VertexFieldQ[_] := False;

(**************************************************************************************************)

PackageExport["EdgeField"]

SetUsage @ "
EdgeField[] constructs the unit edge field.
EdgeField[All] is the same as EdgeField[].
EdgeField[i$] constructs the basis vertex field with weight 1 for the forward 1-path on edge $i.
EdgeField[spec$, 'Forward'] is the same as the above.
EdgeField[spec$, 'Reverse'] gives reverse paths, as above.
EdgeField[spec$, 'Symmetric'] uses weight 1 for forward paths and -1 for reverse paths.
"

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
EdgeField[spec_, "Symmetric"] := symmetricEdgeField[spec];

symmetricEdgeField[All] := Scope[
  UnpackPathAlgebra[edgePairs, pos, neg];
  PathVector @ Association[
    {PathElement[#] -> pos, PathElement[Reverse @ #] -> neg}& /@ edgePairs
  ]
];

symmetricEdgeField[i_Integer] := symmetricEdgeField[{i}];

symmetricEdgeField[ints:{__Integer}] := Scope[
  UnpackPathAlgebra[edgePairs, pos, neg];
  edges = Part[edgePairs, Abs @ ints];
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

(*
(**************************************************************************************************)

declareFormatting[
  o:VertexField[_Graph, _List, _] :> formatField[o],
  o:EdgeField[_Graph, _List, _] :> formatField[o]
];

frameField[f_] := Framed[f, FrameStyle -> LightGray];

formatField[VertexField[g_Graph, values_List, mod_]] :=
  frameField @ ExtendedGraphPlot @ ExtendedGraph[g,
    VertexColorFunction -> Part[fieldColors @ mod, 1 + (values /. Indeterminate -> 0)],
    GraphLegend -> None, ArrowheadShape -> None,
    ImageSize -> "ShortestEdge" -> 15, VertexSize -> AbsolutePointSize[10]
  ];

formatField[EdgeField[g_Graph, values_List, _]] :=
  ExtendedGraphPlot @ ExtendedGraph[g,
    EdgeLabels -> values, ArrowheadShape -> "Line",
    GraphLegend -> None,
    VertexSize -> AbsolutePointSize[5], ImageSize -> "ShortestEdge" -> 20];

(**************************************************************************************************)

PackageExport["OperatorPlus"]
PackageExport["OperatorTimes"]

SetAttributes[OperatorPlus, {Flat, Orderless}];

ops_OperatorPlus[field_] := Plus @@ Map[#[field]&, ops];

OperatorTimes[scalar_, op_][field_] := scalar * op[field];

declareFormatting[
  HoldPattern @ OperatorPlus[ops__] :> Inactive[Plus][ops],
  HoldPattern @ OperatorTimes[scalar_, op_] :> Inactive[Times][scalar, op]
];

SetHoldFirst[operatorQ];

declareOperatorDispatch[symbol_] := (
  operatorQ[_symbol] := True;
  symbol /: Plus[object_symbol, others:Repeated[_ ? operatorQ]] := OperatorPlus[object, others];
  symbol /: Times[scalar_ /; NumericQ[Unevaluated @ scalar], object_symbol] := OperatorTimes[scalar, object];
);

(* OperatorPlus /: Plus[HoldPattern[OperatorPlus[a___]], HoldPattern[OperatorPlus[b___]]] :=
  OperatorPlus[a, b];

OperatorPlus /: Times[scalar_ /; NumericQ[Unevaluated @ scalar], op_OperatorPlus] :=
  OperatorTimes[scalar, op];
 *)

declareOperatorDispatch[OperatorTimes];
declareOperatorDispatch[OperatorPlus];

(* OperatorPlus[a_] := a;
 *)
(* OperatorPlus[a___, OperatorPlus[b___], c___] := OperatorPlus[a, b, c];
 *)

 OperatorTimes[a_, OperatorTimes[b_, c_]] := OperatorTimes[a * b, c];

************************************************************************************************

PackageExport["IdentityOperator"]

IdentityOperator[][vf_] := vf;

declareOperatorDispatch[IdentityOperator];

declareFormatting[
  IdentityOperator[] :> "I"
];


(**************************************************************************************************)

PackageExport["CardinalDerivative"]

CardinalDerivative[vf_, None] := vf;

CardinalDerivative[vf_VertexField, path_] :=
  Displacement[vf, path] - vf;

CardinalDerivative[path_][field_] :=
  CardinalDerivative[field, path];

declareOperatorDispatch[CardinalDerivative];

declareFormatting[
  CardinalDerivative[path_] :> Subscript["\[CapitalDelta]", Sequence @@ ParseCardinalWord[path]]
];

(**************************************************************************************************)

PackageExport["Displacement"]

Displacement[path_][field_] :=
  Displacement[field, path];

Displacement[VertexField[graph_, values_, mod_], path_] := Scope[
  tagOutTable = TagVertexOutTable @ graph;
  cardinals = ParseCardinalWord @ path;
  Scan[cardinal |-> Set[values, indeterminatePart[values, tagOutTable @ cardinal]], cardinals];
  VertexField[graph, values, mod]
];

indeterminatePart[values_, part_] :=
  Map[Internal`UnsafeQuietCheck[Part[values, #], Indeterminate]&, part];

declareOperatorDispatch[Displacement];

declareFormatting[
  Displacement[path_] :> Subscript["D", Sequence @@ ParseCardinalWord[path]]
];

(**************************************************************************************************)

PackageExport["RandomVertexField"]

RandomVertexField[graph_, n_Integer] :=
  VertexField[graph, RandomInteger[{0, n-1}, VertexCount @ graph], n];

PackageExport["RandomEdgeField"]

RandomEdgeField[graph_, n_Integer] :=
  EdgeField[graph, RandomInteger[{0, n-1}, EdgeCount @ graph], n];

(**************************************************************************************************)

declarePlusTimesDispatch[VertexField, <|Plus -> fieldPlus, Times -> fieldTimes|>]

fieldPlus[fields__] := Scope[
  fields = {fields};
  head = Part[fields, 1, 0];
  graph = Part[fields, 1, 1];
  mod = Part[fields, 1, 3];
  values = Part[fields, All, 2];
  sum = MapThread[Mod[Plus[##], mod]&, values];
  head[graph, sum, mod]
];

fieldTimes[scalar_, head_[graph_, values_, mod_]] :=
  head[graph, Mod[values * scalar, mod], mod]

*)