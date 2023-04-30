


(**************************************************************************************************)

PrivateFunction[toCodeMarkdown]

toCodeMarkdown[code_, multiline_] := Scope[
  code2 = StringReplace[code, $shortcodeRules];
  StringJoin @ If[multiline, {"<pre>", code2, "</pre>"}, {"<code>", code2, "</code>"}]
];

$shortcodeRules = {
  (code:$colorShortCodes ~~ ":" ~~ next_) :> applyShortcode[code, next],
  "\\n" ~~ s:" ".. :> StringJoin["<br>", ConstantArray["&nbsp;", StringLength @ s]],
  "\\n" -> "<br>",
  "^{" ~~ sup:Shortest[___] ~~ "}" :> StringJoin["<sup>", sup, "</sup>"],
  "_{" ~~ sub:Shortest[___] ~~ "}" :> StringJoin["<sub>", sub, "</sub>"]
};

$colorShortcodeMapping = <|
  "RF" -> $Red,
  "GF" -> $Green,
  "BF" -> $Blue,
  "OF" -> $Orange,
  "GrF" -> $Gray,
  "PiF" -> $Pink,
  "PF" -> $Purple
|>;

$colorShortCodes = Apply[Alternatives, Keys @ $colorShortcodeMapping];

applyShortcode[code_, next_] := htmlStyledString[next, {FontColor -> Lookup[$colorShortcodeMapping, code]}];

