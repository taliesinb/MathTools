PrivateFunction[toCodeMarkdown]

$subSuperRules = {
  Cell[BoxData @ SubscriptBox["", a_]]   :> {"_{", a, "}"},
  Cell[BoxData @ SuperscriptBox["", a_]] :> {"^{", a, "}"}
}

toCodeMarkdown[code_, multiline_] := Scope[
  $allowMDemph = False;
  If[!StrQ[code],
    (* this path is taken if we have been called because of a standalone "PreformattedCode" Cell with
    complex internal formatting like StyleBox etc. *)
    code //= RepAll[$subSuperRules];
    code //= textToMarkdown; (* this will call wlCharactersToUnicode *)
  ,
    (* this path is taken if we have an Output cell with a "StringBlockForm" TemplateBox, or if we have
    ` ` inside a textual context, via processInlineCodeBlock *)
    code //= wlCharactersToUnicode
  ];
  code //= subShortCodes;
  If[SContainsQ[code, $forbiddenStrings], Return @ ""];
  res = SJoin @ If[multiline, {"<pre>", code, "</pre>"}, {"<code>", code, "</code>"}];
  res
];

subShortCodes[s_Str] := SRep[s, $shortcodeRules];

$shortcodeRules = {
  (code:$colorShortcodeP ~~ ":" ~~ next_) :> applyShortcode[code, next],
  (code:$colorShortcodeP ~~ "{" ~~ body:Except["\n"|"}"].. ~~ "}") :> applyShortcode[code, balancedQ @ body],
  (* Fira Code doesn't have well-sized DS letters outside R,C,N,Z etc *)
  l:DoubleStruckCharacter :> doubleStruckToBoldRoman[l],
  "\\n" | "\n" ~~ s:" ".. :> SJoin["<br>", Repeat["&nbsp;", SLen @ s]],
  "\\n" | "\n" -> "<br>",
  "^{" ~~ sup:Shortest[___] ~~ "}" /; balancedQ[sup] :> SJoin["<sup>", subShortCodes @ sup, "</sup>"],
  "_{" ~~ sub:Shortest[___] ~~ "}" /; balancedQ[sub] :> SJoin["<sub>", subShortCodes @ sub, "</sub>"],
  "_" ~~ sub:DigitCharacter :> SJoin["<sub>", sub, "</sub>"],
  "^" ~~ sup:DigitCharacter :> SJoin["<sup>", sup, "</sup>"]
};

balancedQ[a_] := StringCount[a, "{"] == StringCount[a, "}"];

(**************************************************************************************************)

$doubleStruckCharacterList = Chars @ $DoubleStruckCharacters;
$romanCharacterList = Chars @ $RomanCharacters;

doubleStruckToBoldRoman[char:("ℂ" | "ℕ" | "ℚ" | "ℝ" | "ℤ")] := char;
doubleStruckToBoldRoman[char_] := $boldFontTemplate @ Part[$romanCharacterList, IndexOf[$doubleStruckCharacterList, char]];

$boldFontTemplate = StringFunction @ "<span style='font-weight:bold'>#1</span>";

(**************************************************************************************************)

$textColorShortcodes = <|
  "RF"  -> $Red,     "1F"  -> "Color1",
  "BF"  -> $Blue,    "2F"  -> "Color2",
  "GF"  -> $Green,   "3F"  -> "Color3",
  "OrF" -> $Orange,  "4F"  -> "Color4",
  "PuF" -> $Purple,  "5F"  -> "Color5",
  "TeF" -> $Teal,    "6F"  -> "Color6",
  "GrF" -> $Gray,    "7F"  -> "Color7",
  "PiF" -> $Pink,    "8F"  -> "Color8",
  "YeF" -> $Yellow,  "9F"  -> "Color9",
  "UF" -> "Underlined"
|>;

$backgroundShortcodes = <|
  "1B" -> "Background1",
  "2B" -> "Background2",
  "3B" -> "Background3",
  "4B" -> "Background4",
  "5B" -> "Background5",
  "6B" -> "Background6",
  "7B" -> "Background7",
  "8B" -> "Background8",
  "9B" -> "Background9"
|>;

$colorShortcodeP = Join[AssociationKeyPattern @ $textColorShortcodes, AssociationKeyPattern @ $backgroundShortcodes];

applyShortcode["UF", text_] :=
  htmlStyledString[text, {Underlined}];

applyShortcode[code_ /; SMatchQ[code, _ ~~ "B"], text_] :=
  htmlStyledString[text, {Background -> Lookup[$backgroundShortcodes, code]}];

applyShortcode[code_, text_] :=
  htmlStyledString[text, {FontColor -> Lookup[$textColorShortcodes, code]}];

(**************************************************************************************************)

PrivateVariable[$colorShortcodeMappingInverse]

$colorShortcodeMappingInverse := AssocInvert[$textColorShortcodes];


