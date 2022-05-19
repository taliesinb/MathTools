PackageExport["Cardinals"]

(**************************************************************************************************)

PackageExport["CardinalSet"]

SetUsage @ "
CardinalSet[cardinals$] represents a set of cardinals that is simultaneously present on an edge.
"

MakeBoxes[CardinalSet[set_List], TraditionalForm] :=
  RowBox @ Riffle[MakeBoxes[#, TraditionalForm]& /@ set, " "];

(**************************************************************************************************)

PackageExport["SimplifyCardinalSet"]

SimplifyCardinalSet = Case[
  CardinalSet[{a_}]                               := % @ a;
  CardinalSet[{l___, CardinalSet[{m___}], r___}]  := % @ CardinalSet[{l, m, r}];
  other_                                          := other;
];

(**************************************************************************************************)

PackageScope["SpliceCardinalSets"]

SpliceCardinalSets[e_] := Map[StripInverted, ReplaceAll[e, CardinalSet -> Splice]];

(**************************************************************************************************)

PackageScope["SpliceCardinalSetEdges"]

SpliceCardinalSetEdges[e_] := ReplaceAll[e, DirectedEdge[a_, b_, CardinalSet[s_]] :> Splice[DirectedEdge[a, b, #]& /@ s]];

