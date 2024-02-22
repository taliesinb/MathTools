PublicTypesettingForm[SemiringProductForm, SemiringSumForm]

DefineInfixForm[SemiringProductForm, "?"];
DefineInfixForm[SemiringSumForm, "?"];

PublicTypesettingForm[StyledSemiringProductForm, StyledSemiringSumForm]

(**************************************************************************************************)

PublicTypesettingForm[RingSymbol]

DefineTaggedForm[RingSymbol];

(**************************************************************************************************)

PublicTypesettingForm[SemiringSymbol]

DefineTaggedForm[SemiringSymbol];

(**************************************************************************************************)

PublicTypesettingForm[RingElementSymbol]

DefineTaggedForm[RingElementSymbol];

(**************************************************************************************************)

PublicTypesettingForm[RingUnitElementForm]

RingUnitElementForm[e_] := RingUnitElementForm["1", e];

DefineBinaryForm[RingUnitElementForm, "?"];

(**************************************************************************************************)

PublicTypesettingForm[RingZeroElementSymbol]

DefineSymbolForm[RingZeroElementSymbol -> "0"];

(**************************************************************************************************)

PublicTypesettingForm[RingBasisElementForm]

DefineTaggedForm[RingBasisElementForm];

(**************************************************************************************************)

declareRingForm[list_List] :=
  Scan[declareLieGroupOrAlgebraForm, list];

(**************************************************************************************************)

restCommaRiffled[name_][first_, rest__] := name[first, riffled[","][rest]]

declareDerivedRingForm[symbol_Symbol, type_:Auto] := With[
  {name = SymbolName @ symbol},
  {katex = LowerCaseFirst @ STrim[name, "Form"]},
  declareBoxFormatting[
    symbol[ring_, args___] :> TemplateBox[
      Pre[toHintedSymbol[ring -> RingSymbol]] @
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
DefineBinaryForm[MultisetSemiringForm, "?"];
DefineBinaryForm[SignedMultisetRingForm, "?"];

PublicTypesettingForm[MultisetSemiringSymbolForm, SignedMultisetRingSymbolForm]

DefineTaggedForm[MultisetSemiringSymbolForm];
DefineTaggedForm[SignedMultisetRingSymbolForm];

PublicTypesettingForm[MultisetSemiringProductForm, MultisetSemiringSumForm]

DefineInfixForm[MultisetSemiringProductForm, "?"];
DefineInfixForm[MultisetSemiringSumForm, "?"];

PublicTypesettingForm[SignedMultisetRingProductForm, SignedMultisetRingSumForm]

DefineInfixForm[SignedMultisetRingProductForm, "?"];
DefineInfixForm[SignedMultisetRingSumForm, "?"];

(**************************************************************************************************)

PublicTypesettingForm[PolynomialRingForm]

declareDerivedRingForm[PolynomialRingForm, VariableForm];

