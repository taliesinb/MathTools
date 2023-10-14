PublicTypesettingForm[SemiringProductForm, SemiringSumForm]

declareInfixSymbol[SemiringProductForm] // usingCustomKatex["srdot"];
declareInfixSymbol[SemiringSumForm] // usingCustomKatex["srplus"];

PublicTypesettingForm[StyledSemiringProductForm, StyledSemiringSumForm]

(**************************************************************************************************)

PublicTypesettingForm[RingSymbol]

DefineTaggedForm[RingSymbol, Aliases -> <|"Z" -> Integers|>];

(**************************************************************************************************)

PublicTypesettingForm[SemiringSymbol]

DefineTaggedForm[SemiringSymbol, Aliases -> <|"N" -> Naturals|>];

(**************************************************************************************************)

PublicTypesettingForm[RingElementSymbol]

DefineTaggedForm[RingElementSymbol];

(**************************************************************************************************)

PublicTypesettingForm[RingUnitElementForm]

RingUnitElementForm[e_] := RingUnitElementForm["1", e];

declareBinaryForm[RingUnitElementForm];

(**************************************************************************************************)

PublicTypesettingForm[RingZeroElementSymbol]

declareConstantSymbol[RingZeroElementSymbol];

(**************************************************************************************************)

PublicTypesettingForm[RingBasisElementForm]

declareUnaryForm[RingBasisElementForm];

(**************************************************************************************************)

declareRingForm[list_List] :=
  Scan[declareLieGroupOrAlgebraForm, list];

(**************************************************************************************************)

restCommaRiffled[name_][first_, rest__] := name[first, riffled[","][rest]]

declareDerivedRingForm[symbol_Symbol, type_:Automatic] := With[
  {name = SymbolName @ symbol},
  {katex = LowerCaseFirst @ StringTrim[name, "Form"]},
  declareBoxFormatting[
    symbol[ring_, args___] :> TemplateBox[
      Prepend[toHintedSymbol[ring -> RingSymbol]] @
        MapUnevaluated[toHintedSymbol[# -> type]&, {args}],
      name
    ]
  ];
  $TemplateKatexFunction[name] = restCommaRiffled[katex];
];

(**************************************************************************************************)

PublicTypesettingForm[MatrixRingForm]

declareDerivedRingForm[MatrixRingForm];

(**************************************************************************************************)

PublicTypesettingForm[MultisetSemiringForm, SignedMultisetRingForm]

(* why not declareDerivedRingForm ? *)
declareBinaryForm[MultisetSemiringForm];
declareBinaryForm[SignedMultisetRingForm];

PublicTypesettingForm[MultisetSemiringSymbolForm, SignedMultisetRingSymbolForm]

declareSymbolFormExplicit[MultisetSemiringSymbolForm];
declareSymbolFormExplicit[SignedMultisetRingSymbolForm];

PublicTypesettingForm[MultisetSemiringProductForm, MultisetSemiringSumForm]

declareInfixSymbol[MultisetSemiringProductForm] // usingCustomKatex["msrdot"];
declareInfixSymbol[MultisetSemiringSumForm] // usingCustomKatex["msrplus"];

PublicTypesettingForm[SignedMultisetRingProductForm, SignedMultisetRingSumForm]

declareInfixSymbol[SignedMultisetRingProductForm] // usingCustomKatex["smrdot"];
declareInfixSymbol[SignedMultisetRingSumForm] // usingCustomKatex["smrplus"];

(**************************************************************************************************)

PublicTypesettingForm[PolynomialRingForm]

declareDerivedRingForm[PolynomialRingForm, VariableForm];

