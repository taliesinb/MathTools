PublicForm[GraphSymbol, EdgeSymbol, VertexSymbol]

GraphSymbol[] := GraphSymbol["G"];

declareSymbolForm[GraphSymbol];
declareSymbolForm[EdgeSymbol];

(**************************************************************************************************)

PublicForm[EdgesSymbol, VerticesSymbol]

declareBoxFormatting[
  EdgesSymbol[q_] :> makeHintedTemplateBox[q -> QuiverSymbol, "EdgesSymbolForm"],
  EdgesSymbol[] :> SBox["EdgesSymbol"],
  VerticesSymbol[q_] :> makeHintedTemplateBox[q -> QuiverSymbol, "VerticesSymbolForm"],
  VerticesSymbol[] :> SBox["VerticesSymbol"]
]

$TemplateKatexFunction["EdgesSymbolForm"] = "edges";
$TemplateKatexFunction["VerticesSymbolForm"] = "vertices";

$TemplateKatexFunction["EdgesSymbol"] = "edges";
$TemplateKatexFunction["VerticesSymbol"] = "vertices";

(**************************************************************************************************)

PublicForm[VertexSymbol, HeadVertexForm, TailVertexForm]

declareSymbolForm[VertexSymbol];
declareUnaryForm[TailVertexForm];
declareUnaryForm[HeadVertexForm];

declareBoxFormatting[
  VertexSymbol[e:(_[___])] :> TemplateBox[List @ MakeBoxes @ e, "VertexSymbolForm"]
];

$TemplateKatexFunction["HeadVertexForm"] = "hvert";
$TemplateKatexFunction["TailVertexForm"] = "tvert";
$TemplateKatexFunction["VertexSymbolForm"] = "vert";

(* for legacy notebooks: *)
$TemplateKatexFunction["HeadVertexSymbolForm"] = "hvert";
$TemplateKatexFunction["TailVertexSymbolForm"] = "tvert";

(**************************************************************************************************)

PublicSymbol[PlaceholderVertexSymbol, HeadVertexSymbol, TailVertexSymbol]

declareConstantSymbol[PlaceholderVertexSymbol] // usingCustomKatex["placeholderVertexSymbol"];
declareConstantSymbol[HeadVertexSymbol] // usingCustomKatex["headVertexSymbol"];
declareConstantSymbol[TailVertexSymbol] // usingCustomKatex["tailVertexSymbol"];

(**************************************************************************************************)

PublicSymbol[InverseArrowheadSymbol, ArrowheadSymbol, UpArrowheadSymbol, DownArrowheadSymbol, LeftArrowheadSymbol, RightArrowheadSymbol]

declareConstantSymbol[{ArrowheadSymbol, InverseArrowheadSymbol, UpArrowheadSymbol, DownArrowheadSymbol, LeftArrowheadSymbol, RightArrowheadSymbol}];


(**************************************************************************************************)

PublicForm[VertexCountOfForm]

declareUnaryForm[VertexCountOfForm, QuiverSymbol];

(**************************************************************************************************)

PublicForm[UndirectedEdgeForm]

declareBoxFormatting[
  UndirectedEdgeForm[a_, b_] :>
    makeHintedTemplateBox[a -> VertexSymbol, b -> VertexSymbol, "UndirectedEdgeForm"],
  UndirectedEdgeForm[a_, b_, c_] :>
    toLongEdgeForm @ makeHintedTemplateBox[a -> VertexSymbol, b -> VertexSymbol, c -> CardinalSymbol, "TaggedUndirectedEdgeForm"]
]

$TemplateKatexFunction["UndirectedEdgeForm"] = "de";
$TemplateKatexFunction["TaggedUndirectedEdgeForm"] = "tue";
$TemplateKatexFunction["LongTaggedUndirectedEdgeForm"] = "tue";

(**************************************************************************************************)

PublicForm[DirectedEdgeForm]

declareBoxFormatting[
  DirectedEdgeForm[a_, b_] :>
    makeHintedTemplateBox[a -> VertexSymbol, b -> VertexSymbol, "DirectedEdgeForm"],
  DirectedEdgeForm[a_, b_, c_] :>
    toLongEdgeForm @ makeHintedTemplateBox[a -> VertexSymbol, b -> VertexSymbol, c -> CardinalSymbol, "TaggedDirectedEdgeForm"],
  DirectedEdgeForm[a_, b_, c_, d_] :>
    toLongEdgeForm @ makeTemplateBox[a, b, c, d, "MultiTaggedDirectedEdgeForm"]

]

longTagQ[e_] := Count[e, "CardinalSymbolForm", {0, Infinity}] > 1;
longTagQ[TemplateBox[_, "CardinalProductForm"]] := True;

toLongEdgeForm[TemplateBox[{a_, b_, tag_}, form_String]] /; longTagQ[tag] :=
  TemplateBox[{a, b, tag}, "Long" <> form];

toLongEdgeForm[other_] := other;

$TemplateKatexFunction["DirectedEdgeForm"] = "de";
$TemplateKatexFunction["TaggedDirectedEdgeForm"] = "tde";
$TemplateKatexFunction["MultiTaggedDirectedEdgeForm"] = "mtde";
$TemplateKatexFunction["LongTaggedDirectedEdgeForm"] = "tde";


(**************************************************************************************************)

PublicForm[GraphHomomorphismSymbol]

GraphHomomorphismSymbol[] := GraphHomomorphismSymbol["\[Pi]"]

declareBoxFormatting[
  
  GraphHomomorphismSymbol[f_] :>
    TemplateBox[List @ rawSymbolBoxes @ f, "GraphHomomorphismSymbolForm"]

]

$TemplateKatexFunction["GraphHomomorphismSymbolForm"] = "graphHomomorphism";

(**************************************************************************************************)

PublicForm[FromToForm]

declareInfixSymbol[FromToForm, VertexSymbol, True];

(**************************************************************************************************)

PublicForm[VertexOfForm, EdgeOfForm, PathOfForm]

declareBoxFormatting[
  VertexOfForm[a_, b_] :> makeHintedTemplateBox[a -> VertexSymbol, b -> QuiverSymbol, "VertexOfForm"],
  EdgeOfForm[a_, b_] :> makeHintedTemplateBox[a -> EdgeSymbol, b -> QuiverSymbol, "EdgeOfForm"],
  PathOfForm[a_, b_] :> makeHintedTemplateBox[a -> PathSymbol, b -> QuiverSymbol, "PathOfForm"]
];

$TemplateKatexFunction["VertexOfForm"] = "vertOf";
$TemplateKatexFunction["EdgeOfForm"] = "edgeOf";
$TemplateKatexFunction["PathOfForm"] = "pathOf";

(**************************************************************************************************)

PublicForm[IndexedGraphUnionForm, IndexedGraphDisjointUnionForm]

declareSumLikeFormatting[IndexedGraphUnionForm, "indexGraphUnion"];
declareSumLikeFormatting[IndexedGraphDisjointUnionForm, "indexGraphDisjointUnion"];

(**************************************************************************************************)

PublicForm[GraphRegionIntersectionForm, GraphRegionUnionForm]

declareInfixSymbol[{GraphRegionIntersectionForm, GraphRegionUnionForm}]

