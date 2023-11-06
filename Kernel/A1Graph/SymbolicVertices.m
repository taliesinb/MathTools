PublicHead[LatticeVertex]

SetUsage @ "
LatticeVertex[coords$] represents a vertex in a lattice graph with lattice coordinates coords$.
LatticeVertex[coords$, type$] represents a vertex in a lattice graph with type type$.
"

$thinGap = Style["\[CenterDot]", Gray];
$thickGap = Style[" \[CenterDot] ", Gray];
formatInteger[a_] := Style[If[Negative[a], UnderBar[Abs[a]], a], FontFamily -> "Avenir"];
formatLVertex[args:{__Int}] := Row[formatInteger /@ args, $thinGap];
formatLVertex[args_List] := Row[args, $thickGap];
formatLVertex[args_] := args;
formatLVertex[args_, type_] := Overscript[formatLVertex[args], type];

declareFormatting[
  LatticeVertex[args_List] :> formatLVertex[args],
  LatticeVertex[args_List, type_] :> formatLVertex[args, type]
];

(**************************************************************************************************)

PublicHead[CardinalProduct]

SetUsage @ "
CardinalProduct[a$, b$] represents a product of two vertices a$ and b$.
"

declareBoxFormatting[
  CardinalProduct[args___] :> MakeBoxes @ CardinalProductForm[args]
];

(**************************************************************************************************)

PublicHead[CardinalSequence]

SetUsage @ "
CardinalSequence[a$, b$] represents a sequence of cardinals.
"

declareBoxFormatting[
  CardinalSequence[a_, b_] :> TemplateBox[{MakeBoxes @ a, MakeBoxes @ b}, "CardinalSequenceForm"]
];

$TemplateKatexFunction["CardinalSequenceForm"] = riffled[" \\cardinalSequenceSymbol "];

(**************************************************************************************************)

PublicHead[VertexProduct]

SetUsage @ "
VertexProduct[a$, b$] represents a product of two vertices a$ and b$.
"

declareBoxFormatting[
  VertexProduct[args__] :> TemplateBox[MapUnevaluated[MakeBoxes, {args}], "VertexProductForm"]
];

(**************************************************************************************************)

PublicFunction[FlattenProductSymbols]

$flattenProductsRule = {
  VertexProduct[l___, VertexProduct[m__], r___] :> VertexProduct[l, m, r],
  CardinalProduct[l___, CardinalProduct[m__], r___] :> CardinalProduct[l, m, r]
};

FlattenProductSymbols[e_] := e //. $flattenProductsRule;

(**************************************************************************************************)

PublicFunction[ProductVertices]

ProductVertices[a_, b_] := VertexProduct @@@ Tuples[{toRange[a], toRange[b]}];

toRange[list_List] := list;
toRange[n_Int] := Range[-n, n];
toRange[m_Int ;; n_Int] := Range[m, n];
toRange[m_Int ;; n_Int ;; s_Int] := Range[m, n, s];

(**************************************************************************************************)

PublicHead[ContractedVertex]

SetUsage @ "
ContractedVertex[{v$1, v$2, $$}] represents a vertex formed by contracting several vertices v$i.
ContractedVertex[vlist$, name$] represents a vertex formed by contraction with new name name$.
"

declareFormatting[
  ContractedVertex[v__] :> ContractionProductForm[v]
];

(**************************************************************************************************)

PublicHead[SumVertex]

SetUsage @ "
SumVertex[v$, g$] represents a vertex v$ from graph g$ in a sum of graphs.
"

SumVertex[i_][e_] := SumVertex[e, i];

declareFormatting[
  SumVertex[v_, g_] :> Subscript[v, g]
];
