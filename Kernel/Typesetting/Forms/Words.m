PublicTypesettingForm[WordGroupSymbol, WordRingSymbol]

(* TODO: factor this into declare01SymbolForm *)
declareSymbolForm[WordGroupSymbol, QuiverSymbol]
declareSymbolForm[WordRingSymbol, QuiverSymbol]
declareSymbolForm[PlanRingSymbol, QuiverSymbol]

declareBoxFormatting[
  WordGroupSymbol[] :> SBox["WordGroupSymbol"],
  WordRingSymbol[] :> SBox["WordRingSymbol"],
  PlanRingSymbol[] :> SBox["PlanRingSymbol"]
];

$TemplateKatexFunction["WordGroupSymbol"] = katexAlias["wordGroupSymbol"];
$TemplateKatexFunction["WordRingSymbol"] = katexAlias["wordRingSymbol"];
$TemplateKatexFunction["PlanRingSymbol"] = katexAlias["planRingSymbol"];

(**************************************************************************************************)

PublicTypesettingForm[WordRingElementSymbol]

WordRingElementSymbol[] := WordRingElementSymbol["\[Omega]"];

declareSymbolForm[WordRingElementSymbol];

(**************************************************************************************************)

PublicTypesettingForm[WordRingBasisElementForm]

declareUnaryForm[WordRingBasisElementForm];

(**************************************************************************************************)

PublicTypesettingForm[RepeatedPowerForm]

declareBinaryForm[RepeatedPowerForm];

(**************************************************************************************************)

PublicFunction[WordSymbol]

declareBoxFormatting[
  WordSymbol[s_] :> TemplateBox[List @ rawSymbolBoxes @ s, "WordSymbolForm"]
];

$TemplateKatexFunction["WordSymbolForm"] = "wordSymbol";

(**************************************************************************************************)

PublicTypesettingForm[WordForm]

WordForm[s_Str] := WordForm @ ToPathWord @ s;

declareBoxFormatting[
  WordForm[e_] :> wordBoxes[e]
];

$TemplateKatexFunction["EmptyWordForm"] := "emptyWord"
$TemplateKatexFunction["WordForm"] = "word";

$cardP = _CardinalSymbol | InvertedForm[_CardinalSymbol] | MirrorForm[_CardinalSymbol] ;
$maybeColoredCardP = $colorFormP[$cardP] | $cardP;

PrivateTypesettingBoxFunction[cardinalBox, wordBoxes]

SetHoldAllComplete[wordBoxes, cardinalBox];
wordBoxes = Case[
  {}|""                             := SBox["EmptyWordForm"];
  1                                 := TemplateBox[{"1"}, "WordForm"];
  word_Str                          := Construct[%, ToPathWord @ word];
  Style[word_Str, assoc_Assoc]      := applyCardColoring[Construct[%, ToPathWord @ word], assoc];
  (Times|ConcatenationForm)[args__] := TemplateBox[MapUnevaluated[%, {args}], "ConcatenationForm"];
  RepeatedPowerForm[a_, b_]         := TemplateBox[{% @ a, MakeQGBoxes @ b}, "RepeatedPowerForm"];
  c:cardP                           := TemplateBox[List @ MakeBoxes @ c, "WordForm"];
  list:{cardP..}                    := TemplateBox[MapUnevaluated[MakeBoxes, list], "WordForm"];
  list_List                         := TemplateBox[tryColorCardinals @ cardinalBoxes @ list, "WordForm"];
  (Inverted|InvertedForm)[e_]       := TemplateBox[List @ wordBoxes @ e, "InvertedForm"];
  MirrorForm[e_]                    := TemplateBox[List @ wordBoxes @ e, "MirrorForm"];
  Form[f_]                          := MakeQGBoxes @ f;
  cs_CardinalSequenceForm           := MakeBoxes @ cs;
  sc_SerialCardinal                 := MakeBoxes @ sc;
  pc_ParallelCardinal               := MakeBoxes @ pc;
  w_WordForm                        := MakeBoxes @ w;
  w_WordSymbol                      := MakeBoxes @ w;
  w_SymbolForm                      := MakeBoxes @ s; (* placeholder *)
  s:symsP                           := TemplateBox[List @ rawSymbolBoxes @ s, "WordSymbolForm"],
  {symsP -> $rawSymbolP, cardP -> $maybeColoredCardP}
];

applyCardColoring[list_, colors_] := ReplaceAll[list,
  TemplateBox[{c_Str}, form_] /; KeyExistsQ[colors, c] :>
    TemplateBox[{TemplateBox[{c}, form]}, SymbolName @ colors @ c]];

cardinalBoxes[list_List] := Map[cardinalBox, list];

cardSymBox[e_] := TemplateBox[{e}, "CardinalSymbolForm"];

toNegCard[TemplateBox[{e_}, "CardinalSymbolForm"]] :=
  TemplateBox[{e}, "InvertedCardinalSymbolForm"];

toNegCard[TemplateBox[{e_}, "MirrorCardinalSymbolForm"]] :=
  TemplateBox[{e}, "InvertedMirrorCardinalSymbolForm"];

toMirrorCard[TemplateBox[{e_}, "CardinalSymbolForm"]] :=
  TemplateBox[{e}, "MirrorCardinalSymbolForm"];

toMirrorCard[TemplateBox[{e_}, "InvertedCardinalSymbolForm"]] :=
  TemplateBox[{e}, "MirrorInvertedCardinalSymbolForm"];

cardinalBox = Case[
  c_CardinalSymbol                := MakeBoxes @ c;
  c:InvertedForm[_CardinalSymbol] := MakeBoxes @ c;
  s_Str                           := cardSymBox @ s;
  (col:colsP)[e_]                 := TemplateBox[List @ MakeBoxes @ e, SymbolName @ col];
  i_Int                           := cardSymBox @ TextString @ i;
  s:symsP                         := cardSymBox @ rawSymbolBoxes @ s;
  cs_CardinalSequenceForm         := MakeBoxes @ cs;
  sc_SerialCardinal               := MakeBoxes @ sc;
  pc_ParallelCardinal             := MakeBoxes @ pc;
  MirrorForm[s_]                  := toMirrorCard @ % @ s;
  Inverted[s_]                    := toNegCard @ % @ s;
  p_CardinalProductForm           := MakeBoxes @ p;
  p_VerticalCardinalProductForm   := MakeBoxes @ p,
  {symsP -> $rawSymbolP, colsP -> $colorFormP}
]

(* only colors if it is all-or-nothing *)
tryColorCardinals[list_] /; $AutoColorCardinals :=
  If[SubsetQ[{"r", "g", "b"}, list /. TemplateBox[{a_}, _] :> a],
    list /. $colorCardinalRules,
    list
  ];

tryColorCardinals[list_] := list;

$cardFormP = "CardinalSymbolForm" | "InvertedCardinalSymbolForm";

$colorCardinalRules = {
  b:TemplateBox[{"r"}, $cardFormP] -> TemplateBox[List @ b, "RedForm"],
  b:TemplateBox[{"g"}, $cardFormP] -> TemplateBox[List @ b, "GreenForm"],
  b:TemplateBox[{"b"}, $cardFormP] -> TemplateBox[List @ b, "BlueForm"]
};

(**************************************************************************************************)

PublicTypesettingForm[PlainWordForm]

PlainWordForm[s_Str] := PlainWordForm @ ToPathWord @ s;

declareBoxFormatting[
  PlainWordForm[e_] :> Block[{$AutoColorCardinals = False}, wordBoxes[e]]
];

(**************************************************************************************************)

PublicVariable[$AutoColorCardinals]

$AutoColorCardinals = True;
