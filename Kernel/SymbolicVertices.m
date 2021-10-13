PackageExport["LatticeVertex"]

SetUsage @ "
LatticeVertex[coords$] represents a vertex in a lattice graph with lattice coordinates coords$.
LatticeVertex[coords$, type$] represents a vertex in a lattice graph with type type$.
"

$thinGap = Style["\[CenterDot]", Gray];
$thickGap = Style[" \[CenterDot] ", Gray];
formatInteger[a_] := Style[If[Negative[a], UnderBar[Abs[a]], a], FontFamily -> "Avenir"];
formatLVertex[args:{__Integer}] := Row[formatInteger /@ args, $thinGap];
formatLVertex[args_List] := Row[args, $thickGap];
formatLVertex[args_] := args;
formatLVertex[args_, type_] := Overscript[formatLVertex[args], type];

declareFormatting[
  LatticeVertex[args_List] :> formatLVertex[args],
  LatticeVertex[args_List, type_] :> formatLVertex[args, type]
];

(**************************************************************************************************)

PackageExport["CardinalProduct"]

SetUsage @ "
CardinalProduct[a$, b$] represents a product of two vertices a$ and b$.
"

declareBoxFormatting[
  CardinalProduct[a_, b_] :> TemplateBox[{MakeBoxes @ a, MakeBoxes @ b}, "CardinalProductForm"]
];

$TemplateKatexFunction["CardinalProductForm"] = riffled[" \\cardinalProductSymbol "];


(**************************************************************************************************)

PackageExport["CardinalSequence"]

SetUsage @ "
CardinalSequence[a$, b$] represents a sequence of cardinals.
"

declareBoxFormatting[
  CardinalSequence[a_, b_] :> TemplateBox[{MakeBoxes @ a, MakeBoxes @ b}, "CardinalSequenceForm"]
];

$TemplateKatexFunction["CardinalSequenceForm"] = riffled[" \\cardinalSequenceSymbol "];

(**************************************************************************************************)

PackageExport["VertexProduct"]

SetUsage @ "
VertexProduct[a$, b$] represents a product of two vertices a$ and b$.
"

declareBoxFormatting[
  VertexProduct[args__] :> TemplateBox[MapUnevaluated[MakeBoxes, {args}], "VertexProductForm"]
];

$TemplateKatexFunction["VertexProductForm"] = riffled[" \\vertexProductSymbol "];

(**************************************************************************************************)

PackageExport["FlattenProductSymbols"]

$flattenProductsRule = {
  VertexProduct[l___, VertexProduct[m__], r___] :> VertexProduct[l, m, r],
  CardinalProduct[l___, CardinalProduct[m__], r___] :> CardinalProduct[l, m, r]
};

FlattenProductSymbols[e_] := e //. $flattenProductsRule;

(**************************************************************************************************)

PackageExport["ProductVertices"]

ProductVertices[a_, b_] := VertexProduct @@@ Tuples[{toRange[a], toRange[b]}];

toRange[list_List] := list;
toRange[n_Integer] := Range[-n, n];
toRange[m_Integer ;; n_Integer] := Range[m, n];
toRange[m_Integer ;; n_Integer ;; s_Integer] := Range[m, n, s];

(**************************************************************************************************)

PackageExport["ContractedVertex"]

SetUsage @ "
ContractedVertex[{v$1, v$2, $$}] represents a vertex formed by contracting several vertices v$i.
ContractedVertex[vlist$, name$] represents a vertex formed by contraction with new name name$.
"

declareFormatting[
  ContractedVertex[v__] :> ContractionProductForm[v]
];

(**************************************************************************************************)

PackageExport["SumVertex"]

SetUsage @ "
SumVertex[v$, g$] represents a vertex v$ from graph g$ in a sum of graphs.
"

SumVertex[i_][e_] := SumVertex[e, i];

declareFormatting[
  SumVertex[v_, g_] :> Subscript[v, g]
];
