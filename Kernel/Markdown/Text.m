PrivateFunction[textCellToMarkdown]

(* Todo: introduce simple caching *)

$forbiddenStrings = "XXX" | "XXXX";

ToMarkdownString::msgs = "Messages issued during markdown conversion of cell printed below.";
textCellToMarkdown[e_] := Scope[
  $lastExternalCodeCell ^= None;
  If[ContainsQ[e, s_String /; StringContainsQ[s, $forbiddenStrings]], Return[""]];
  Check[
    text = StringTrim @ StringJoin @ textCellToMarkdownOuter @ e;
  ,
    Message[ToMarkdownString::msgs];
    PrintBadCell @ e;
  ];
  If[!StringQ[text], Return["bad text"]];
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
  BoxData[box_]                                                           := toMultilineMath @ box;
  TextData @ Cell[BoxData[box_FormBox], ___]                              := toMultilineMath @ box;
  TextData @ Cell[BoxData[boxes:{Repeated[_FormBox | WhiteString]}], ___] := toMultilineMath @ RowBox @ replaceIndentingNewlines @ boxes;
  other_ := textBoxesToMarkdown @ other;
];

replaceIndentingNewlines[boxes_] :=
  VectorReplace[boxes, s_String :> StringReplace[s, "\[IndentingNewLine]"|"\n" -> "\\\\\n"]];

(**************************************************************************************************)

toMultilineMath[boxes_] := $multilineMathTemplate @ baseToMath @ boxes;
toInlineMath[boxes_]    := $inlineMathTemplate @ baseToMath @ boxes;

baseToMath[box_] /; StringQ[$localKatexDefinitions] := Block[
  {localDefs = $localKatexDefinitions, res},
  Clear[$localKatexDefinitions];
  StringJoin[localDefs, "\n", baseToMath[box]]
];

baseToMath[box_] := $katexPostprocessor @ boxesToKatexString @ box;

(**************************************************************************************************)

PrivateFunction[textBoxesToMarkdown]

PrivateVariable[$textPostProcessor]

$textPostProcessor = Identity;

(* TODO: just wrap inline math with a symbolic wrapper, then replace that with inlineMathTemplate later.
this will allow us to interpret $ as coming soley from user and not a result of inlineMathTemplate *)

$lastSpace = False;
textBoxesToMarkdown = Case[
  str_String :=
    checkLastSpace @ $textPostProcessor @ str;
  list_List :=
    Map[%, list];
  TextData[e_] :=
    % @ e;
  RowBox[e_List] :=
    % @ e;
  Cell[BoxData[boxes_, ___], ___] :=
    ($lastTB = toInlineMath @ RowBox @ ToList @ boxes);
  Cell[TextData[text_, ___], ___] :=
    text;
  StyleBox[str_String /; StringMatchQ[str, Whitespace], ___] :=
    ($lastSpace = True; " ");
  ButtonBox[title_String, BaseStyle -> "Hyperlink", ButtonData -> {URL[url_String], _}, ___] :=
    StringJoin["[", % @ title, "](", url, ")"];
  StyleBox[boxes_, opts___] := styleBoxToMarkdown @ StyleBox[boxes, Apply[Sequence] @ Sort @ List @ FilterOptions[styleBoxToMarkdown, opts]];
];

checkLastSpace[list_List] := Map[checkLastSpace, list];
checkLastSpace[s_String /; StringEndsQ[s, " "]] := ($lastSpace = True; s);
checkLastSpace[other_] := ($lastSpace = False; other);

Options[styleBoxToMarkdown] = {FontWeight -> "Plain", FontSlant -> "Plain", FontColor -> None};

styleBoxToMarkdown = Case[ 
  StyleBox[e_String, FontSlant -> Italic|"Italic"] /; TrueQ[$lastSpace] := wrapWith[e, "*"];
  StyleBox[e_String, FontWeight -> Bold|"Bold"] /; TrueQ[$lastSpace] := wrapWith[e, "**"];
  StyleBox[e_String, FontSlant  -> Italic|"Italic", FontWeight -> Bold|"Bold"] /; TrueQ[$lastSpace] := wrapWith[e, "***"];
  StyleBox[e_, rules___] := $styledSpanTemplate[textBoxesToMarkdown @ e, StringRiffle[toStylePropVal /@ {rules}, ";"]];
]

toStylePropVal = Case[
  FontColor -> (color_? ColorQ) := "color:" <> Image`Utilities`toHEXcolor[color];
  FontWeight -> "Bold"|Bold := "font-weight:bold";
  FontWeight -> Plain|"Plain" := "font-weight:normal";
  FontSlant -> "Italic"|Italic := "font-style:italic";
  FontSlant -> Plain|"Plain" := "font-style:normal";  
  _ := Nothing
]

$styledSpanTemplate = StringFunction @ "<span style='#2'>#1</span>"
$colorSpanTemplate = StringFunction @ "<font color='#2'>#1</font>"

styledMD[e___] := Print["Can't handle: ", e];

wrapWith[e_, wrap_] := Scope[
  e = StringJoin @ e;
  p1 = StringStartsQ[e, Whitespace];
  p2 = StringEndsQ[e, Whitespace];
  {If[p1, " ", {}], wrap, StringTrim @ e, wrap, If[p2, " ", {}]}
];