PublicForm[SemiringProductForm, SemiringSumForm]

declareInfixSymbol[SemiringProductForm] // usingCustomKatex["srdot"];
declareInfixSymbol[SemiringSumForm] // usingCustomKatex["srplus"];

PublicForm[StyledSemiringProductForm, StyledSemiringSumForm]

(**************************************************************************************************)

PublicForm[RingSymbol]

RingSymbol[] := RingSymbol["R"];

$ringAliases = <|
  "Z" -> "Integers"
|>

declareAlgebraicSymbol[RingSymbol, $ringAliases];

(**************************************************************************************************)

PublicForm[SemiringSymbol]

$semiringAliases = <|
  "N" -> "Naturals"
|>

declareAlgebraicSymbol[SemiringSymbol, $semiringAliases];

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

PublicForm[MatrixRingForm]

declareDerivedRingForm[MatrixRingForm];

(**************************************************************************************************)

PublicForm[MultisetSemiringForm, SignedMultisetRingForm]

(* why not declareDerivedRingForm ? *)
declareBinaryForm[MultisetSemiringForm];
declareBinaryForm[SignedMultisetRingForm];

PublicForm[MultisetSemiringSymbolForm, SignedMultisetRingSymbolForm]

declareSymbolFormExplicit[MultisetSemiringSymbolForm];
declareSymbolFormExplicit[SignedMultisetRingSymbolForm];

PublicForm[MultisetSemiringProductForm, MultisetSemiringSumForm]

declareInfixSymbol[MultisetSemiringProductForm] // usingCustomKatex["msrdot"];
declareInfixSymbol[MultisetSemiringSumForm] // usingCustomKatex["msrplus"];

PublicForm[SignedMultisetRingProductForm, SignedMultisetRingSumForm]

declareInfixSymbol[SignedMultisetRingProductForm] // usingCustomKatex["smrdot"];
declareInfixSymbol[SignedMultisetRingSumForm] // usingCustomKatex["smrplus"];

(**************************************************************************************************)

PublicForm[PolynomialRingForm]

declareDerivedRingForm[PolynomialRingForm, IndeterminateSymbol];
