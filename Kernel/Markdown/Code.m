


(**************************************************************************************************)

PrivateFunction[toCodeMarkdown]

$subSuperRules = {
  Cell[BoxData @ SubscriptBox["", a_]]   :> {"_{", a, "}"},
  Cell[BoxData @ SuperscriptBox["", a_]] :> {"^{", a, "}"}
}

toCodeMarkdown[code_, multiline_] := Scope[
  $allowMDemph = False;
  If[!StringQ[code],
    (* this path is taken if we have been called because of a standalone "PreformattedCode" Cell with
    complex internal formatting like StyleBox etc. *)
    code //= ReplaceAll[$subSuperRules];
    code //= textToMarkdown (* this will call wlCharactersToUnicode *)
  ,
    (* this path is taken if we have an Output cell with a "StringBlockForm" TemplateBox, or if we have
    ` ` inside a textual context, via processInlineCodeBlock *)
    code //= wlCharactersToUnicode
  ];
  code2 = StringReplace[code, $shortcodeRules];
  res = StringJoin @ If[multiline, {"<pre>", code2, "</pre>"}, {"<code>", code2, "</code>"}];
  res
];

$shortcodeRules = {
  (code:$colorShortCodes ~~ ":" ~~ next_) :> applyShortcode[code, next],
  (code:$colorShortCodes ~~ "{" ~~ body:Except["\n"|"}"].. ~~ "}") :> applyShortcode[code, body],
  "\[DoubleStruckCapitalK]" | "𝕂" -> "<span style='font-weight:bold'>K</span>",
  "\[DoubleStruckCapitalV]" | "𝕍" -> "<span style='font-weight:bold'>V</span>", (* Fira Code doesn't have well-sized DS letters outside R,C,N,Z etc *)
  "\\n" | "\n" ~~ s:" ".. :> StringJoin["<br>", ConstantArray["&nbsp;", StringLength @ s]],
  "\\n" | "\n" -> "<br>",
  "^{" ~~ sup:Shortest[___] ~~ "}" :> StringJoin["<sup>", sup, "</sup>"],
  "_{" ~~ sub:Shortest[___] ~~ "}" :> StringJoin["<sub>", sub, "</sub>"],
  "_" ~~ sub:DigitCharacter :> StringJoin["<sub>", sub, "</sub>"],
  "^" ~~ sup:DigitCharacter :> StringJoin["<sup>", sup, "</sup>"]
};

$colorShortcodeMapping = <|
  "RF" -> $Red,
  "GF" -> $Green,
  "BF" -> $Blue,
  "OF" -> $Orange,
  "TF" -> $Teal,
  "GrF" -> $Gray,
  "PiF" -> $Pink,
  "PF" -> $Purple
|>;

PrivateVariable[$colorShortcodeMappingInverse]

$colorShortcodeMappingInverse = Association @ Reverse[Normal @ $colorShortcodeMapping, 2];

$colorShortCodes = Apply[Alternatives, Keys @ $colorShortcodeMapping];

applyShortcode[code_, next_] := htmlStyledString[next, {FontColor -> Lookup[$colorShortcodeMapping, code]}];

