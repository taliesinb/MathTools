PublicTypesettingForm[DoubleQuotedStringForm, QuotedCharacterForm, WildcardStringForm]

PublicTypesettingForm[LiteralStringForm, StringSymbolForm]

PublicTypesettingForm[LiteralCharacterForm, CharacterSymbolForm]

declareBoxFormatting[
  DoubleQuotedStringForm[args___] :>
    TemplateBox[MapUnevaluated[stringElementBoxes, {args}], "DoubleQuotedStringForm"],

  WildcardStringForm[args___] :>
    TemplateBox[MapUnevaluated[stringElementBoxes, {args}], "WildcardStringForm"],

  QuotedCharacterForm[a_] :>
    TemplateBox[List @ quotedCharBoxes @ a, "QuotedCharacterForm"],

  LiteralStringForm[str_Str] :> TemplateBox[List @ StringJoin["\"", str, "\""], "LiteralStringForm"],
  LiteralCharacterForm[str_Str] :> TemplateBox[List @ StringJoin["\"", str, "\""], "LiteralCharacterForm"]
];

$TemplateKatexFunction["WildcardStringForm"] = applyRiffled["wstring", " "];

$TemplateKatexFunction["DoubleQuotedStringForm"] = applyRiffled["qstring", " "];
$TemplateKatexFunction["QuotedCharacterForm"] = "qchar";

$TemplateKatexFunction["LiteralStringForm"] = katexWrapText["lstr"];
$TemplateKatexFunction["LiteralCharacterForm"] = katexWrapText["lchar"];

$TemplateKatexFunction["StringSymbolForm"] = "strsym";
$TemplateKatexFunction["CharacterSymbolForm"] = "charsym";

katexWrapText[op_][s_Str] := op @ "texttt" @ StringTrim[StringReplace[s, "-" -> "\\textendash "]; s, "\""];

DefineTaggedForm[CharacterSymbolForm]
DefineTaggedForm[StringSymbolForm]

SetHoldAllComplete[stringElementBoxes, stringSymbolBoxes, quotedCharBoxes]

stringElementBoxes = Case[
  s_Str /; StringLength[s] === 1 := If[UpperCaseQ[s],
    MakeBoxes @ StringSymbolForm @ s,
    MakeBoxes @ LiteralCharacterForm @ s
  ];
  str_Str := MakeBoxes @ LiteralStringForm @ str;
  e_ ? unaryWrappedQ := recurseWrapperBoxes[e, stringSymbolBoxes];
  other := MakeBoxes @ other;
]

stringSymbolBoxes = Case[
  s_Str := If[UpperCaseQ[s],
    MakeBoxes @ StringSymbolForm @ s,
    MakeBoxes @ CharacterSymbolForm @ s
  ];
  s_StringSymbolForm | s_CharacterSymbolForm := MakeBoxes @ s;
]

(* this should only be a literal or a CharacterSymbol *)
quotedCharBoxes = Case[
  s_Str /; StringLength[s] === 1 := MakeBoxes @ LiteralCharacterForm @ s;
  s_CharacterSymbolForm := MakeBoxes @ s;
  e_ ? unaryWrappedQ := recurseWrapperBoxes[e, %];
]
