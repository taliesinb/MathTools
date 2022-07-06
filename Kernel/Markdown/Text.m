PrivateFunction[textCellToMarkdown]

(* Todo: introduce simple caching *)

$forbiddenStrings = "XXX" | "XXXX";

textCellToMarkdown[e_] := Scope[
  text = StringTrim @ StringJoin @ textCellToMarkdownOuter @ e;
  If[StringContainsQ[text, "\\badDispatch"], PrintBadCell @ text];
  If[StringContainsQ[text, $forbiddenStrings], Return[""]];
  text // StringReplace[$finalStringFixups1] // StringReplace[$finalStringFixups2]
]

$finalStringFixups1 = {
  "  $" -> " $",
  "$  " -> "$ "
};

makeFontSmallCaps[text_] :=
  $rawHTMLTemplate["<span style=\"font-variant: small-caps;\">" <> ToLowerCase[text] <> "</span>"];

$finalStringFixups2 = {
  "$ --" -> "$ --",
  "$ -" -> "$\\-",
  "$ ." -> "$.",
  "$ ," -> "$,",
  "$ ?" -> "$?",
  "$ !" -> "$!",
  "$ :" -> "$:",
  "$ *" -> "\,$ *",
  "\\text{\\,and\\,}" -> "$ and $", (* improves linebreaking *)
  "LHS" :> makeFontSmallCaps["LHS"],
  "RHS" :> makeFontSmallCaps["RHS"],
  "1'st" | "1st" -> "$1^{\\textrm{st}}$",
  "2'nd" | "2nd" -> "$2^{\\textrm{nd}}$",
  "3'rd" | "3rd" -> "$3^{\\textrm{rd}}$",
  "n-ary" -> "$n$-ary",
  (d:DigitCharacter ~~ "'th") :> StringJoin["$", d, "^{\\textrm{th}}$"],
  "n'th"-> "$n^{\\textrm{th}}$",
  "i'th" -> "$i^{\\textrm{th}}$",
  l:("f"|"p") ~~ "-" ~~ r:("vert"|"edge"|"cardinal"|"quiver") :>
    StringJoin["_", l, "_\\-", r]
};

WhiteString = _String ? (StringMatchQ[Whitespace]);

textCellToMarkdownOuter = Case[
  TagBox[box_, "ClassTaggedForm"[tag_]] | BoxData[TagBox[box_, "ClassTaggedForm"[tag_]]] :=
    $classAttributeTemplate[{tag}] @ textCellToMarkdownOuter @ box;
  BoxData[box_] :=
    $multilineMathTemplate @ $katexPostprocessor @ boxesToKatexString @ box;
  TextData @ Cell[BoxData[box_FormBox], ___] :=
    $multilineMathTemplate @ $katexPostprocessor @ boxesToKatexString @ box;
  TextData @ Cell[BoxData[boxes:{Repeated[_FormBox | WhiteString]}], ___] :=
    $multilineMathTemplate @ $katexPostprocessor @ boxesToKatexString @ RowBox @ replaceIndentingNewlines @ boxes;
  other_ :=
    textBoxesToMarkdown @ other;
];

replaceIndentingNewlines[boxes_] :=
  VectorReplace[boxes, s_String :> StringReplace[s, "\[IndentingNewLine]"|"\n" -> "\\\\\n"]];

(**************************************************************************************************)

PrivateFunction[textBoxesToMarkdown]

PrivateVariable[$textPostProcessor]

$textPostProcessor = Identity;

(* TODO: just wrap inline math with a symbolic wrapper, then replace that with inlineMathTemplate later.
this will allow us to interpret $ as coming soley from user and not a result of inlineMathTemplate *)

textBoxesToMarkdown = Case[
  str_String :=
    $textPostProcessor @ str;
  list_List :=
    Map[%, list];
  TextData[e_] :=
    % @ e;
  RowBox[e_List] :=
    % @ e;
  Cell[BoxData[boxes_, ___], ___] :=
    $inlineMathTemplate @ $katexPostprocessor @ boxesToKatexString @ RowBox @ ToList @ boxes;
  Cell[TextData[text_, ___], ___] :=
    % @ text;
  StyleBox[str_String /; StringMatchQ[str, Whitespace], ___] :=
    " ";
  ButtonBox[title_String, BaseStyle -> "Hyperlink", ButtonData -> {URL[url_String], _}, ___] :=
    StringJoin["[", % @ title, "](", url, ")"];
  StyleBox[boxes_, opts___] := Scope[
    {weight, slant, color} = Lookup[{opts}, {FontWeight, FontSlant, FontColor}, None];
    styledMD[% @ boxes, weight === "Bold", slant === "Italic"]
  ];
];

styledMD[e_, False, False] := e;
styledMD[e_, False, True] := wrapWith[e, "*"];
styledMD[e_, True, False] := wrapWith[e, "**"];
styledMD[e_, True, True] := wrapWith[e, "***"];

wrapWith[e_, wrap_] := Scope[
  e = StringJoin @ e;
  p1 = StringStartsQ[e, Whitespace];
  p2 = StringEndsQ[e, Whitespace];
  {If[p1, " ", {}], wrap, StringTrim @ e, wrap, If[p2, " ", {}]}
];