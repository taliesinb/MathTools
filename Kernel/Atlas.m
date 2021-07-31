PackageExport["CardinalAtlas"]

CardinalAtlas[quiver_Graph, charts_] := Scope[
  Null;
];


(**************************************************************************************************)

PackageExport["ChartSymbol"]

SetUsage @ "
ChartSymbol[sub$] represents a chart and formats as C$sub.
"

declareFormatting[
  ChartSymbol[sym_String] :> formatChartSymbol[sym, Automatic],
  ChartSymbol[other__] :> Subscript["C", other]
];

(**************************************************************************************************)

PackageExport["ChartSymbolCardinals"]

SetUsage @ "
ChartSymbolCardinals[chart$] returns the list of cardinals in a ChartSymbol[$$].
"

ChartSymbolCardinals = Case[
  ChartSymbol[s_, ___] := ToPathWord[s];
  (GraphRegionIntersection|GraphRegionUnion)[s__] := Union @@ Map[%, {s}];
];

(**************************************************************************************************)

PackageExport["CardinalTransitionMatrices"]

CardinalTransitionMatrices[atlas_Graph] := Scope[
  edgeAnnos = LookupExtendedGraphAnnotations[atlas, EdgeAnnotations];
  cardinals = CardinalList @ atlas;
  transitions = Lookup[Replace[edgeAnnos, None -> <||>], "CardinalTransitions", None];
  If[!AssociationQ[transitions], ReturnFailed[]];
  tagIndices = TagIndices @ atlas;
  numCardinals = Length @ cardinals;
  cardinalIndex = AssociationRange @ cardinals;
  matrixShape = {numCardinals, numCardinals};
  KeySortBy[IndexOf[cardinals, #]&] @ Association @ KeyValueMap[buildTagMatrices, tagIndices]
];

(* TODO: check for compatibility *)
buildTagMatrices[tag_, edgeIndices_] := Scope[
  trans = Flatten @ Lookup[transitions, edgeIndices, {}];
  matrix = Normal @ SparseArray[procTransRule /@ trans, matrixShape];
  tag -> matrix
];

procTransRule = Case[
  a_ -> b_          := Lookup[cardinalIndex, {b, a}] -> 1;
  a_ -> Negated[b_] := Lookup[cardinalIndex, {b, a}] -> -1;
];

(**************************************************************************************************)

PackageExport["CardinalTransitionRepresentation"]

CardinalTransitionRepresentation[atlas_Graph] := Scope[
  matrices = CardinalTransitionMatrices[atlas];
  If[FailureQ @ matrices, ReturnFailed[]];
  QuiverRepresentation[
    atlas,
    matrices
  ]
];