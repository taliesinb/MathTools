PublicTypesettingForm[LatticeQuiverForm]

declareBoxFormatting[
  LatticeQuiverForm[fq_, v_, cv_, d_] :>
    makeHintedTemplateBox[fq -> QuiverSymbol, v -> VertexSymbol, cv -> FunctionSymbol, d -> SymbolForm, "LatticeQuiverForm"]
];

$TemplateKatexFunction["LatticeQuiverForm"] = applyRiffled["latticeBFS", ","];

(**************************************************************************************************)

PublicTypesettingForm[QuiverSymbol]

QuiverSymbol[] := QuiverSymbol["Q"];

DefineTaggedForm[QuiverSymbol];

declareBoxFormatting[
  QuiverSymbol[a_TransportAtlasSymbolForm] :> MakeBoxes @ a
]

(**************************************************************************************************)

declareBindingForm[form_, katexName_, argBoxFn_] := With[
  {formName = SymbolName @ form},
  declareBoxFormatting[
    form[first_, args__] :>
      TemplateBox[
        Prepend[
          MapUnevaluated[argBoxFn, {args}],
          ToBoxes @ first
        ],
        MakeMathBoxes @ formName
      ]
  ];
  $TemplateKatexFunction[formName] = katexName[#1, Riffle[{##2}, ","]]&;
];

(**************************************************************************************************)

PublicTypesettingForm[BindingRuleForm]

DefineInfixForm[BindingRuleForm, "?"];

makeSizeBindingRuleBoxes = Case[
  s:(_SymbolForm | EllipsisSymbol | _Modulo) := MakeBoxes @ s;
  sz_QuiverSizeSymbol := MakeBoxes @ sz;
  sz_                 := MakeBoxes @ QuiverSizeSymbol @ sz;
];

makeCardinalSizeBindingRuleBoxes = Case[
  s:(_SymbolForm | EllipsisSymbol | _Modulo | _Int) := MakeBoxes @ s;
  c_ -> sz_           := makeHintedTemplateBox[c -> CardinalSymbol, sz -> QuiverSizeSymbol @ sz, "CompactBindingRuleForm"];
  g_GroupGeneratorSymbol := MakeBoxes @ g;
  c_                  := cardinalBox @ c;
  Form[f_]            := MakeMathBoxes @ f;
];


(**************************************************************************************************)

PublicTypesettingForm[CompactBindingRuleForm]

DefineInfixForm[CompactBindingRuleForm, "?"];

makeCardinalBindingRuleBoxes = Case[
  s:(_SymbolForm | EllipsisSymbol | _Modulo) := MakeBoxes @ s;
  c_                  := cardinalBox @ c;
];

(**************************************************************************************************)

PublicTypesettingForm[CayleyQuiverSymbolForm]

DefineTaggedForm[CayleyQuiverSymbolForm];
DefineTaggedForm[GroupPresentationSymbol];

declareBoxFormatting[
  c_CayleyQuiverSymbolForm[args__] :> MakeBoxes @ CardinalSizeBindingForm[c, args]
];

(**************************************************************************************************)

PublicTypesettingForm[CayleyQuiverBindingForm, ActionQuiverBindingForm]

declareBindingForm[CayleyQuiverBindingForm, "bindCayleyQuiver", makeGeneratorBindingRuleBoxes];
declareBindingForm[ActionQuiverBindingForm, "bindActionQuiver", makeGeneratorBindingRuleBoxes];

makeGeneratorBindingRuleBoxes = Case[
  s:(_SymbolForm | EllipsisSymbol | _SetSymbolForm) := MakeBoxes @ s;
  t_TupleForm             := MakeBoxes @ t;
  c_ -> g_                := makeHintedTemplateBox[c -> CardinalSymbol, g -> GroupGeneratorSymbol, "BindingRuleForm"];
  g_GroupGeneratorSymbol  := MakeBoxes @ g;
  g_GroupElementSymbol  := MakeBoxes @ g;
  g_                      := MakeBoxes @ GroupGeneratorSymbol @ g;
]

(**************************************************************************************************)

PublicTypesettingForm[SubSizeBindingForm]

declareBindingForm[SubSizeBindingForm, "subSize", makeSizeBindingRuleBoxes];

(**************************************************************************************************)

PublicTypesettingForm[SizeBindingForm]

declareBindingForm[SizeBindingForm, "bindSize", makeSizeBindingRuleBoxes];

(**************************************************************************************************)

PublicTypesettingForm[CardinalBindingForm]

declareBindingForm[CardinalBindingForm, "bindCards", makeCardinalBindingRuleBoxes];

(**************************************************************************************************)

PublicTypesettingForm[CardinalSizeBindingForm]

declareBindingForm[CardinalSizeBindingForm, "bindCardSize", makeCardinalSizeBindingRuleBoxes];

(**************************************************************************************************)

PublicTypesettingForm[SerialCardinal, ParallelCardinal]

DefineNAryForm[SerialCardinal, "?"]
DefineNAryForm[ParallelCardinal, "?"]

(**************************************************************************************************)

PublicTypesettingForm[CardinalSymbol]

DefineTaggedForm[CardinalSymbol];

(**************************************************************************************************)

PublicTypesettingForm[MirrorForm]

DefineUnaryForm[MirrorForm, "?"]

(**************************************************************************************************)

declareNamedQuiverSymbol[symbol_] := With[
  {symbolName = SymbolName[symbol]},
  {katexName = LowerCaseFirst @ StringTrim[symbolName, "Symbol"]},
  AppendTo[$literalSymbolsP, symbol];
  declareBoxFormatting[
    symbol :> SBox[symbolName],
    symbol[] :> MakeBoxes @ symbol @ Infinity,
    symbol[size_] :> MakeBoxes @ SubSizeBindingForm[symbol, size],
    symbol[size_Rule] :> MakeBoxes @ CardinalSizeBindingForm[symbol, size],
    symbol[size__] | symbol[TupleForm[size__]] :> MakeBoxes @ CardinalSizeBindingForm[symbol, size],
    q_symbol[cards__] :> MakeBoxes @ CardinalSizeBindingForm[q, cards]
  ];
  $TemplateKatexFunction[symbolName] = katexAlias @ katexName;
]

declareTwoParameterNamedQuiverSymbol[symbol_] := With[
  {symbolName = SymbolName[symbol]},
  {formName = symbolName <> "Form"},
  {compactFormName = "Compact" <> symbolName <> "Form"},
  {katexName = LowerCaseFirst @ StringTrim[symbolName, "Symbol"]},
  declareBoxFormatting[
    symbol[k_] :> makeTemplateBox[k, formName],
    symbol[k_, size_] :> makeHintedTemplateBox[k, size -> QuiverSizeSymbol, compactFormName],
    symbol[k_, size__] | symbol[k_, TupleForm[size__]] :> MakeBoxes @ CardinalSizeBindingForm[symbol[k], size],
    q_symbol[cards__] :> MakeBoxes @ CardinalSizeBindingForm[q, cards]
  ];
  $TemplateKatexFunction[compactFormName] = "subSize"[katexName[#1], #2]&;
  $TemplateKatexFunction[formName] = katexName;
]

(**************************************************************************************************)

PublicTypesettingForm[BouquetQuiverSymbol, GridQuiverSymbol, TreeQuiverSymbol]

declareTwoParameterNamedQuiverSymbol[BouquetQuiverSymbol];
declareTwoParameterNamedQuiverSymbol[GridQuiverSymbol];
declareTwoParameterNamedQuiverSymbol[TreeQuiverSymbol];

PublicTypesettingForm[LineQuiverSymbol, CycleQuiverSymbol, SquareQuiverSymbol, CubicQuiverSymbol, TriangularQuiverSymbol, HexagonalQuiverSymbol, RhombilleQuiverSymbol]

declareNamedQuiverSymbol[LineQuiverSymbol];
declareNamedQuiverSymbol[CycleQuiverSymbol];
declareNamedQuiverSymbol[SquareQuiverSymbol];
declareNamedQuiverSymbol[TriangularQuiverSymbol];
declareNamedQuiverSymbol[HexagonalQuiverSymbol];
declareNamedQuiverSymbol[RhombilleQuiverSymbol];
declareNamedQuiverSymbol[CubicQuiverSymbol];

(**************************************************************************************************)

PublicTypesettingForm[ToroidalModifierForm]

DefineUnaryForm[ToroidalModifierForm];

declareBoxFormatting[
  t_ToroidalModifierForm[args___] :> MakeBoxes @ CardinalSizeBindingForm[t, args]
];

(**************************************************************************************************)

PublicTypesettingForm[AffineModifierForm]

DefineUnaryForm[AffineModifierForm];

(**************************************************************************************************)

PublicTypesettingForm[ModuloForm]

DefineUnaryForm[ModuloForm]

(**************************************************************************************************)

PublicTypesettingForm[QuiverSizeSymbol]

declareBoxFormatting[
  QuiverSizeSymbol[Null] :> "",
  QuiverSizeSymbol[n_Int] :> MakeBoxes @ n,
  QuiverSizeSymbol[Infinity] :> "\[Infinity]",
  QuiverSizeSymbol[Modulo[n_]] :> MakeBoxes @ ModuloForm @ n,
  QuiverSizeSymbol[other_] :> MakeMathBoxes @ other
]

(**************************************************************************************************)

PublicTypesettingForm[StarModifierForm]

DefineUnaryForm[StarModifierForm];

(**************************************************************************************************)

declareNamedBindingSymbol[symbol_] := With[
  {symbolName = SymbolName[symbol]},
  {formName = symbolName <> "Form"},
  declareBoxFormatting[
    symbol[dim_] :> makeHintedTemplateBox[dim -> rawSymbolBoxes, formName],
    q_symbol[cards__] :> MakeBoxes @ CardinalSizeBindingForm[q, cards]
  ];
  $TemplateKatexFunction[formName] = LowerCaseFirst @ StringTrim[symbolName, "Symbol"];
]

(**************************************************************************************************)

PublicTypesettingForm[TranslationPathValuationSymbol, StarTranslationPathValuationSymbol]

declareNamedBindingSymbol[TranslationPathValuationSymbol];
declareNamedBindingSymbol[StarTranslationPathValuationSymbol];

(**************************************************************************************************)

PublicTypesettingForm[TranslationWordHomomorphismSymbol, StarTranslationWordHomomorphismSymbol]

declareNamedBindingSymbol[TranslationWordHomomorphismSymbol];
declareNamedBindingSymbol[StarTranslationWordHomomorphismSymbol];

(**************************************************************************************************)

PublicTypesettingForm[TranslationPresentationSymbol, StarTranslationPresentationSymbol]

declareNamedBindingSymbol[TranslationPresentationSymbol];
declareNamedBindingSymbol[StarTranslationPresentationSymbol];

(**************************************************************************************************)

PublicTypesettingForm[CardinalSequenceForm]

DefineInfixForm[CardinalSequenceForm, "?"];
