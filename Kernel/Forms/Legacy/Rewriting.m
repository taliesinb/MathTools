declareNamedRewritingSystem[symbol_] := With[
  {symbolName = SymbolName[symbol]},
  declareBoxFormatting[
    symbol[] :> SBox[symbolName],
    symbol[args__] :> TemplateBox[
      Pre[SBox[symbolName]] @
      MapUnevaluated[rewritingRuleBoxes, {args}],
      "RewritingSystemRuleBindingForm"
    ]
  ];
  $TemplateKatexFunction[symbolName] = LowerCaseFirst @ STrim[symbolName, "Symbol"];
]

$TemplateKatexFunction["RewritingSystemRuleBindingForm"] = Fn["rewritingRuleBinding"[#1, riffled[","][##2]]];

(**************************************************************************************************)

SetHoldAllComplete[rewritingRuleBoxes];

rewritingRuleBoxes = Case[
  a_ -> b_ := MakeBoxes @ RewritingRuleForm[a, b];
  other_   := MakeMathBoxes @ other;
];

PublicTypesettingForm[GenericRewritingSystemSymbol, StringRewritingSystemSymbol, CircularStringRewritingSystemSymbol, TuringMachineRewritingSystemSymbol, GraphRewritingSystemSymbol, HypergraphRewritingSystemSymbol, CellularAutomatonRewritingSystemSymbol, PetriNetRewritingSystemSymbol]

declareNamedRewritingSystem[GenericRewritingSystemSymbol];
declareNamedRewritingSystem[StringRewritingSystemSymbol];
declareNamedRewritingSystem[CircularStringRewritingSystemSymbol];
declareNamedRewritingSystem[TuringMachineRewritingSystemSymbol];
declareNamedRewritingSystem[GraphRewritingSystemSymbol];
declareNamedRewritingSystem[HypergraphRewritingSystemSymbol];
declareNamedRewritingSystem[CellularAutomatonRewritingSystemSymbol];
declareNamedRewritingSystem[PetriNetRewritingSystemSymbol];

(**************************************************************************************************)

PublicTypesettingForm[RewritingSystemStateBindingForm]

declareBoxFormatting[
  RewritingSystemStateBindingForm[sys_, state_] :>
    makeHintedTemplateBox[sys -> RewritingSystemSymbol, state, "RewritingSystemStateBindingForm"]
]

$TemplateKatexFunction["RewritingSystemStateBindingForm"] = "rewritingStateBinding";

(**************************************************************************************************)

PublicTypesettingForm[RewritingSystemSymbol]

RewritingSystemSymbol[] := RewritingSystemSymbol["R"];

DefineTaggedForm[RewritingSystemSymbol];

(**************************************************************************************************)

PublicTypesettingForm[MultiwayGraphForm]

declareBoxFormatting[
  MultiwayGraphForm[rs_, init_, d_] :>
    makeHintedTemplateBox[rs -> RewritingSystemSymbol, init, d -> SymbolForm, "MultiwayGraphForm"],
  MultiwayGraphForm[rs_, d_] :>
    makeHintedTemplateBox[rs -> RewritingSystemSymbol, d -> SymbolForm, "MultiwayGraphForm"]
];

$TemplateKatexFunction["MultiwayGraphForm"] = applyRiffled["multiwayBFS", ","];

(**************************************************************************************************)

PublicTypesettingForm[ExpressionRewriteMatchForm]

declareBoxFormatting[
  ExpressionRewriteMatchForm[{a___}] :> MakeBoxes[TupleForm[a]],
  ExpressionRewriteMatchForm[a_, i_] :> MakeBoxes[ExpressionRewriteMatchForm[a]]
];

(**************************************************************************************************)

PublicTypesettingForm[RewriteForm]

DefineBinaryForm[RewriteForm, "?"]

declareBoxFormatting[
  RewriteForm[a_, b_, i_] :> MakeBoxes[RewriteForm[a, b]]
];
