(**************************************************************************************************)

PublicForm[DoubleQuotedStringForm, QuotedCharacterForm, WildcardStringForm]

PublicForm[LiteralStringForm, StringSymbolForm]

PublicForm[LiteralCharacterForm, CharacterSymbolForm]

declareBoxFormatting[
  DoubleQuotedStringForm[args___] :>
    TemplateBox[MapUnevaluated[stringElementBoxes, {args}], "DoubleQuotedStringForm"],

  WildcardStringForm[args___] :>
    TemplateBox[MapUnevaluated[stringElementBoxes, {args}], "WildcardStringForm"],

  QuotedCharacterForm[a_] :>
    TemplateBox[List @ quotedCharBoxes @ a, "QuotedCharacterForm"],

  LiteralStringForm[str_String] :> TemplateBox[List @ StringJoin["\"", str, "\""], "LiteralStringForm"],
  LiteralCharacterForm[str_String] :> TemplateBox[List @ StringJoin["\"", str, "\""], "LiteralCharacterForm"]
];

$TemplateKatexFunction["WildcardStringForm"] = applyRiffled["wstring", " "];

$TemplateKatexFunction["DoubleQuotedStringForm"] = applyRiffled["qstring", " "];
$TemplateKatexFunction["QuotedCharacterForm"] = "qchar";

$TemplateKatexFunction["LiteralStringForm"] = katexWrapText["lstr"];
$TemplateKatexFunction["LiteralCharacterForm"] = katexWrapText["lchar"];

$TemplateKatexFunction["StringSymbolForm"] = "strsym";
$TemplateKatexFunction["CharacterSymbolForm"] = "charsym";

katexWrapText[op_][s_String] := op @ "texttt" @ StringTrim[StringReplace[s, "-" -> "\\textendash "]; s, "\""];

declareSymbolForm[CharacterSymbolForm]
declareSymbolForm[StringSymbolForm]

SetHoldAllComplete[stringElementBoxes, stringSymbolBoxes, quotedCharBoxes]

stringElementBoxes = Case[
  s_String /; StringLength[s] === 1 := If[UpperCaseQ[s],
    MakeBoxes @ StringSymbolForm @ s,
    MakeBoxes @ LiteralCharacterForm @ s
  ];
  str_String := MakeBoxes @ LiteralStringForm @ str;
  s:lsymsP := MakeBoxes @ s;
  e_ ? unaryWrappedQ := recurseWrapperBoxes[e, stringSymbolBoxes];
  other := MakeBoxes @ other;
,
  {lsymsP -> $literalSymbolsP}
]

stringSymbolBoxes = Case[
  s_String := If[UpperCaseQ[s],
    MakeBoxes @ StringSymbolForm @ s,
    MakeBoxes @ CharacterSymbolForm @ s
  ];
  s_StringSymbolForm | s_CharacterSymbolForm := MakeBoxes @ s;
]

(* this should only be a literal or a CharacterSymbol *)
quotedCharBoxes = Case[
  s_String /; StringLength[s] === 1 := MakeBoxes @ LiteralCharacterForm @ s;
  s_CharacterSymbolForm := MakeBoxes @ s;
  e_ ? unaryWrappedQ := recurseWrapperBoxes[e, %];
]
