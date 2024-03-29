PublicTypesettingForm[GraphSymbol, EdgeSymbol, VertexSymbol]

DefineTaggedForm[{GraphSymbol, EdgeSymbol, VertexSymbol}];

(**************************************************************************************************)

PublicTypesettingForm[GraphEdgesSymbol, GraphVerticesSymbol]

DefineNamedFunctionSymbolForm[{
  GraphEdgesSymbol -> "E",
  GraphVerticesSymbol -> "V"
}];

PublicTypesettingForm[HeadVertexForm, TailVertexForm]

DefineUnaryForm[HeadVertexForm, OverdotBox[$1]]
DefineUnaryForm[TailVertexForm, UnderdotBox[$1]];

(**************************************************************************************************)

PublicSymbol[PlaceholderVertexSymbol, HeadVertexSymbol, TailVertexSymbol]

DefineSymbolForm[{
  PlaceholderVertexSymbol -> KBox["\[FilledSquare]", """\mathrlap{◨}{◧}"""],
  HeadVertexSymbol -> "\:25e8",
  TailVertexSymbol -> "\:25e7"
}];

(**************************************************************************************************)

PublicSymbol[InverseArrowheadSymbol, ArrowheadSymbol, UpArrowheadSymbol, DownArrowheadSymbol, LeftArrowheadSymbol, RightArrowheadSymbol]

DefineSymbolForm[{
  InverseArrowheadSymbol -> "⏵",
  ArrowheadSymbol        -> "⏴",
  UpArrowheadSymbol      -> "⏶",
  DownArrowheadSymbol    -> "⏷",
  LeftArrowheadSymbol    -> "⏴",
  RightArrowheadSymbol   -> "⏵"
}]

(**************************************************************************************************)

PublicTypesettingForm[VertexCountOfForm]

DefineUnaryForm[VertexCountOfForm, RBox[LeftBox @ "\[LeftBracketingBar]", $wlThinSpace, $1, $wlThinSpace, RightBox @ "\[RightBracketingBar]"]]

(**************************************************************************************************)

PublicTypesettingForm[DirectedEdgeSymbol, UndirectedEdgeSymbol]

DefineSymbolForm[{
  DirectedEdgeSymbol   -> KBox["\[DirectedEdge]", "\\desym"],
  UndirectedEdgeSymbol -> KBox["\[UndirectedEdge]", "\\uesym"]
}]

(**************************************************************************************************)

PublicTypesettingForm[TaggedDirectedEdgeSymbol, TaggedUndirectedEdgeSymbol]

makeTaggedEdgeBoxes[sym_, katex_] := KBox[
  OverscriptBox[sym, StyleBox[$1, ScriptSizeMultipliers -> 0.1, ImageSizeMultipliers -> {0.02, 0.02}]],
  {katex, "{", $1, "}"}
];

DefineUnaryForm[TaggedDirectedEdgeSymbol, makeTaggedEdgeBoxes["\[DirectedEdge]", "\\xundirectededge"], BoxFunction -> taggedDEBox]
DefineUnaryForm[TaggedUndirectedEdgeSymbol, makeTaggedEdgeBoxes["\[UndirectedEdge]", "\\xrightedge"], BoxFunction -> taggedUEBox]

(**************************************************************************************************)

PublicTypesettingForm[MultiTaggedDirectedEdgeSymbol, MultiTaggedUndirectedEdgeSymbol]

makeMultiTaggedEdgeBoxes[sym_, katex_] := KBox[
  UnderoverscriptBox[sym,
    StyleBox[$2, ScriptSizeMultipliers -> 0.1, ImageSizeMultipliers -> {0.02, 0.02}],
    StyleBox[$1, ScriptSizeMultipliers -> 0.1, ImageSizeMultipliers -> {0.02, 0.02}]
  ],
  {"\\operatornamewithlimits{", katex, "{", $1, "}}\\limits_{", $2, "}"}
];

(* {"""\underset{\raisebox{0.15em}{\scriptsize $""", $2, "$}}{", katex <> "{", $1, "}}\\;"} *)

DefineBinaryForm[MultiTaggedDirectedEdgeSymbol, makeMultiTaggedEdgeBoxes["\[DirectedEdge]", "\\xundirectededge"], BoxFunction -> multitaggedDEBox]
DefineBinaryForm[MultiTaggedUndirectedEdgeSymbol, makeMultiTaggedEdgeBoxes["\[UndirectedEdge]", "\\xrightedge"], BoxFunction -> multitaggedUEBox]

(**************************************************************************************************)

PublicTypesettingForm[DirectedEdgeForm, UndirectedEdgeForm]

DefineInfixBinaryForm[DirectedEdgeForm, OpBox @ ToBoxes @ DirectedEdgeSymbol];
DefineInfixBinaryForm[UndirectedEdgeForm, OpBox @ ToBoxes @ UndirectedEdgeSymbol];

DefineStandardTraditionalForm[{
  DirectedEdgeForm[a_, b_, c_]       :> TBox[MakeMathBoxes @ a, MakeMathBoxes @ b, MakeMathBoxes @ c, "TaggedDirectedEdgeForm"],
  UndirectedEdgeForm[a_, b_, c_]     :> TBox[MakeMathBoxes @ a, MakeMathBoxes @ b, MakeMathBoxes @ c, "TaggedUndirectedEdgeForm"],
  DirectedEdgeForm[a_, b_, c_, d_]   :> TBox[MakeMathBoxes @ a, MakeMathBoxes @ b, MakeMathBoxes @ c, MakeMathBoxes @ d, "MultiTaggedDirectedEdgeForm"],
  UndirectedEdgeForm[a_, b_, c_, d_] :> TBox[MakeMathBoxes @ a, MakeMathBoxes @ b, MakeMathBoxes @ c, MakeMathBoxes @ d, "MultiTaggedUndirectedEdgeForm"],
  DirectedEdgeForm[a_, b_, CardinalProductForm[c_, d_]] :> MakeBoxes @ DirectedEdgeForm[a, b, c, d]
}];

DefineTemplateBox[DirectedEdgeForm,   "TaggedDirectedEdgeForm", RBox[$1, " ", taggedDEBox[$3], " ", $2], None];
DefineTemplateBox[UndirectedEdgeForm, "TaggedUndirectedEdgeForm", RBox[$1, " ", taggedUEBox[$3], " ", $2], None];
DefineTemplateBox[DirectedEdgeForm,   "MultiTaggedDirectedEdgeForm", RBox[$1, " ", multitaggedDEBox[$3, $4], " ", $2], None];
DefineTemplateBox[UndirectedEdgeForm, "MultiTaggedUndirectedEdgeForm", RBox[$1, " ", multitaggedUEBox[$3, $4], " ", $2], None];

(**************************************************************************************************)

PublicTypesettingForm[GraphHomomorphismSymbol]

DefineTaggedForm[GraphHomomorphismSymbol]

(**************************************************************************************************)

PublicTypesettingForm[VertexOfForm, EdgeOfForm, PathOfForm]

DefineInfixBinaryForm[VertexOfForm, WideOpBox @ SubscriptBox["\[Element]", "v"]];
DefineInfixBinaryForm[EdgeOfForm,   WideOpBox @ SubscriptBox["\[Element]", "e"]];
DefineInfixBinaryForm[PathOfForm,   WideOpBox @ SubscriptBox["\[Element]", "p"]];

(**************************************************************************************************)

PublicTypesettingForm[IndexedGraphUnionForm, IndexedGraphDisjointUnionForm]

DefineLegacyIndexedForm[IndexedGraphUnionForm, KBox["\[Union]", "\\bigcup"]];
DefineLegacyIndexedForm[IndexedGraphDisjointUnionForm, KBox[LowerBox[StyleBox["\[SquareUnion]", FontSize -> Large], .15], "\\bigsqcup"]];

(**************************************************************************************************)

PublicTypesettingForm[GraphRegionIntersectionForm, GraphRegionUnionForm]

DefineInfixForm[GraphRegionUnionForm,        OpBox @ StyleBox["\[Union]", ScriptSizeMultipliers -> 1]];
DefineInfixForm[GraphRegionIntersectionForm, OpBox @ StyleBox["\[Intersection]", ScriptSizeMultipliers -> 1]];

(**************************************************************************************************)

PublicTypesettingForm[GraphIsomorphismSymbol]

DefineTaggedForm[GraphIsomorphismSymbol]

