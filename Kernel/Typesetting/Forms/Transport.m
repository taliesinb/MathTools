PublicForm[CardinalRewriteForm]

declareBoxFormatting[
  CardinalRewriteForm[a_, b_] :>
    TemplateBox[MapUnevaluated[cardinalBox, {a, b}], "CardinalRewriteForm"]
]

$TemplateKatexFunction["CardinalRewriteForm"] = "cardinalRewrite"

(**************************************************************************************************)

PublicForm[ChartSymbolForm]

declareBoxFormatting[
  ChartSymbolForm[elem_] :>
    makeTemplateBox[elem, "ChartSymbolForm"],
  ChartSymbolForm[elem_List] :>
    makeTypedBoxTemplate[elem -> ConcatenationForm, "ChartSymbolForm"],
  ChartSymbolForm[] :>
    SBox["ChartSymbol"]
];

$TemplateKatexFunction["ChartSymbolForm"] = "chart"
$TemplateKatexFunction["ChartSymbol"] = katexAlias["chartSymbol"];


(**************************************************************************************************)

PublicFunction[TransportMapSymbol]

declareBoxFormatting[
  TransportMapSymbol[p_] :> makeTypedTemplateBox[p -> PathSymbol, "TransportMapSymbolForm"],
  TransportMapSymbol[] :> SBox["TransportMapSymbol"]
];

$TemplateKatexFunction["TransportMapSymbolForm"] = "transportMap"
$TemplateKatexFunction["TransportMapSymbol"] = "transportMapSymbol"

(**************************************************************************************************)

(* how is this different from wordgroup? *)
PublicForm[CardinalGroupSymbolForm]

declareSymbolForm[CardinalGroupSymbolForm, SymbolForm];

(**************************************************************************************************)

(* this doesn't have any katex, and what is it for? shouldn't it be a word group? *)
(* PublicForm[CardinalGroupoidSymbolForm]

declareSymbolForm[CardinalGroupoidSymbolForm, SymbolForm];
 *)
(**************************************************************************************************)

PublicForm[TransportAtlasSymbolForm]

declareBoxFormatting[
  TransportAtlasSymbolForm[q_] :> makeTypedTemplateBox[q -> QuiverSymbol, "TransportAtlasSymbolForm"],
  TransportAtlasSymbolForm[] :> SBox["TransportAtlasSymbol"]
];

$TemplateKatexFunction["TransportAtlasSymbolForm"] = "transportAtlas"
$TemplateKatexFunction["TransportAtlasSymbol"] = "\\transportAtlas{}"&
