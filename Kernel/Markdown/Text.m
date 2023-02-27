PrivateFunction[boxesToInputText]

boxesToInputText[BoxData @ boxes_] := boxesToInputText @ boxes;

boxesToInputText[boxes_String] := boxes;

boxesToInputText[boxes_] := Scope[

  boxes = ReplaceAll[boxes, s_String :> StringReplace[s, $nl -> $sentinel]];

  result = MathLink`CallFrontEnd @ FrontEnd`ExportPacket[
    Cell[BoxData @ boxes, "Output"],
    "InputText",
    "AllowExportAutoReplacements" -> False
  ];
  If[!MatchQ[result, {_String, _, _}], ReturnFailed[]];
  StringReplace[$sentinel -> $nl] @ StringReplace[{"\\" <> $nl -> "", $nl ~~ " "... -> "", "\t" -> "    "}] @ First @ result
];

$sentinel = "^^^^!";
$nl = "\n";

$nlPre = {$nl -> $nl2, $inl -> $inl2};
$nlPost = {$nl2 -> $nl, $inl2 -> $inl, "\\" <> $nl -> ""};

(* adapted from https://mathematica.stackexchange.com/questions/213907/control-page-width-when-using-exportpacket-inputtext *)

(**************************************************************************************************)

PrivateFunction[textCellToMarkdown]

(* Todo: introduce simple caching *)

$forbiddenStrings = "XXX" | "XXXX";

ToMarkdownString::msgs = "Messages issued during markdown conversion of cell printed below.";

textCellToMarkdown[e_] := Scope[
  $lastExternalCodeCell ^= None;
  If[ContainsQ[e /. _CompressedData :> Null, s_String /; StringContainsQ[s, $forbiddenStrings]],
    VPrint["Dropping cell with forbidden content"];
    Return[""]
  ];
  Check[
    text = StringTrim @ StringJoin @ textCellToMarkdownOuter @ e;
  ,
    Message[ToMarkdownString::msgs];
    Print[MsgExpr[text, 6, 50]];
    PrintBadCell[e];
  ];
  If[!StringQ[text], Return["### Markdown generation failed"]];
  If[StringContainsQ[text, $forbiddenStrings], Return[""]];
  (* these are disabled for now because they involve the assumption that $ is the katex delimiter, and they
  have other QG-specific things in them *)
(*text //= StringReplace[$finalStringFixups1];
  text //= StringReplace[$finalStringFixups2]; *)
  text //= deblockIndent; (* JS *)
  text
]

$finalStringFixups1 = {
  "  $" -> " $",
  "$  " -> "$ "
};

makeFontSmallCaps[text_] :=
  $rawHTMLTemplate["<span style=\"font-variant: small-caps;\">" <> ToLowerCase[text] <> "</span>"];

$finalStringFixups2 = {
(*
  "$ --" -> "$ --",
  "$ -" -> "$\\-",
  "$ ." -> "$.",
  "$ ," -> "$,",
  "$ ?" -> "$?",
  "$ !" -> "$!",
  "$ :" -> "$:",
  "$ *" -> "\,$ *",
  "\\text{\\,and\\,}" -> "$ and $", (* improves linebreaking *) *)
  "LHS" :> makeFontSmallCaps["LHS"],
  "RHS" :> makeFontSmallCaps["RHS"]
  (* "1'st" | "1st" -> "$1^{\\textrm{st}}$",
  "2'nd" | "2nd" -> "$2^{\\textrm{nd}}$",
  "3'rd" | "3rd" -> "$3^{\\textrm{rd}}$",
  "n-ary" -> "$n$-ary",
  (d:DigitCharacter ~~ "'th") :> StringJoin["$", d, "^{\\textrm{th}}$"],
  "n'th"-> "$n^{\\textrm{th}}$",
  "i'th" -> "$i^{\\textrm{th}}$",
  l:("f"|"p") ~~ "-" ~~ r:("vert"|"edge"|"cardinal"|"quiver") :>
    StringJoin["_", l, "_\\-", r]
 *)
};

WhiteString = _String ? (StringMatchQ[Whitespace]);

textCellToMarkdownOuter = Case[

  BoxData[t:TagBox[_, "ClassTaggedForm"[_]]]                              := % @ t;
  TagBox[box_, "ClassTaggedForm"[tag_]]                                   := $classAttributeTemplate[{tag}] @ textCellToMarkdownOuter @ box;

  BoxData[box_ ? pureTextBoxesQ]                                          := textBoxesToMarkdown @ box;
  BoxData[box_]                                                           := toMultilineMath @ box;

  TextData @ Cell[BoxData[box_FormBox], ___]                              := toMultilineMath @ box;
  TextData @ Cell[BoxData[boxes:{Repeated[_FormBox | WhiteString]}], ___] := toMultilineMath @ RowBox @ replaceIndentingNewlines @ boxes;

  other_ := Block[{$lastSpace = True}, textBoxesToMarkdown @ other];
];

replaceIndentingNewlines[boxes_] :=
  VectorReplace[boxes, s_String :> StringReplace[s, "\[IndentingNewLine]"|"\n" -> "\\\\\n"]];

(**************************************************************************************************)

deblockIndent[str_String] := Scope[
  blocks = StringSplit[str, "\n" ~~ (" " | "\t")... ~~ "\n"];
  StringRiffle[Map[deblockIndent0, blocks], "\n\n"]
];

(* JS: color wrapped around blocks doesn't work *)
deblockIndent0[str_String] /; StringMatchQ[str, "<span style=" ~~ Except[">"].. ~~ ">" ~~ inner___ ~~ "</span>"] && StringCount[str, "</span>"] == 1 :=
  StringReplace[str, StartOfString ~~ style:("<span style=" ~~ Except[">"].. ~~ ">") ~~ inner___ ~~ "</span>" ~~ EndOfString :>
    deblockIndent0[inner]];

deblockIndent0[str_String] := Scope[
  tabSizes = StringCases[str, StartOfLine ~~ Repeated[" ", {0, 4}] ~~ t:"\t"... :> StringLength[t]];
  {min, max} = MinMax[tabSizes];
  If[min > 0 && min == max, str = StringDelete[str, StartOfLine ~~ Repeated[" ", {0, 4}] ~~ StringRepeat["\t", min]]];
  StringReplace[str, StartOfLine ~~ "\[SmallCircle] " ~~ line:Except["\n"].. ~~ EndOfLine :> "  * " <> line]
];

(**************************************************************************************************)

PrivateFunction[mathCellToMarkdown]

mathCellToMarkdown = Case[
  BoxData[b_]   := % @ b;
  FormBox[b_]   := % @ b;
  b_            := toMultilineMath @ b
]

(**************************************************************************************************)

(* this ensures that manual linebreaks inside e.g. Equation cells produce katex line breaks *)
$katexNewline = "\\\\\n";
procTextualNewlines = Case[
  RowBox[a_]                  := RowBox @ % @ a;
  a_List                      := Map[%, a];
  StyleBox[a_, opts___]       := StyleBox[% @ a, opts];
  TextData[a_]                := TextData @ % @ a;
  str_String                  := StringReplace[str, "\n" -> $katexNewline];
  other_                      := other;
];

toMultilineMath[boxes_] := $multilineMathTemplate @ baseToMath @ procTextualNewlines @ boxes;
toInlineMath[boxes_]    := $inlineMathTemplate @ baseToMath @ boxes;

baseToMath[box_] /; StringQ[$localKatexDefinitions] := Block[
  {localDefs = $localKatexDefinitions, res},
  Clear[$localKatexDefinitions];
  StringJoin[localDefs, "\n", baseToMath[box]]
];

baseToMath[box_] := $katexPostprocessor @ boxesToKatexString @ box;

(**************************************************************************************************)
(*
asInlineMath[e_] := StringJoin["\[LeftSkeleton]0", e, "0\[RightSkeleton]"];
asMultilineMath[e_] := StringJoin["\[LeftSkeleton]1", e, "1\[RightSkeleton]"];
 *)
(**************************************************************************************************)

PrivateFunction[textBoxesToMarkdown]

PrivateVariable[$textPostProcessor]
$textPostProcessor = Identity;

(* TODO: just wrap inline math with a symbolic wrapper, then replace that with inlineMathTemplate later.
this will allow us to interpret $ as coming soley from user and not a result of inlineMathTemplate *)

$lastSpace = True;
textBoxesToMarkdown = Case[
  str_String :=
    $textPostProcessor @ StringJoin @ checkLastSpace @ str;

  list_List :=
    Map[%, list];

  TextData[e_] :=
    % @ e;

  RowBox[e_List] :=
    % @ e;

  (* JS notebooks *)
  Cell[BoxData[FormBox[b_ButtonBox, _]]] := % @ b;

  Cell[BoxData[g_GraphicsBox], ___] :=
    complainBoxes[g];

  Cell[BoxData[b_], ___] :=
    toInlineMath @ b;

  Cell[TextData[text_, ___], ___] :=
    text;

  StyleBox[str_String /; StringMatchQ[str, Whitespace], ___] :=
    ($lastSpace = True; " ");

  FormBox[b_ButtonBox, _] := % @ b;

  ButtonBox[title_, BaseStyle -> "Hyperlink"|Hyperlink, ButtonData -> {URL[url_String], _}, ___] := Scope[
    linkText = StringJoin @ % @ title; If[StringTrim[linkText] === "", Return @ ""];
    numNewlines = StringLength[linkText] - StringLength[linkText = StringTrimRight[linkText, "\n"..]];
    StringJoin["[", linkText, "](", url, ")", StringRepeat["\n", numNewlines]]
  ];

  StyleBox[boxes_, opts___] :=
    $textPostProcessor @ StringJoin @ styleBoxToMarkdown @ StyleBox[boxes, Apply[Sequence] @ Sort @ List @ FilterOptions[styleBoxToMarkdown, opts]];

  other_ := complainBoxes[other];
];

complainBoxes[other_] := Scope[
  Message[ToMarkdownString::badmdbox, MsgExpr @ other];
  headStr = ToPrettifiedString @ Head @ other;
  PrintQGStackSymbols[];
  "**CANNOT CONVERT TEXTUAL BOXES TO MARKDOWN**\n```\n" <> ToPrettifiedString[other, MaxDepth -> 3, MaxLength -> 10, MaxIndent -> 3] <> "\n```\n"
];

ToMarkdownString::badmdbox = "Cannot form markdown for boxes that appeared within a textual context: ``."

checkLastSpace[list_List] := Map[checkLastSpace, list];
checkLastSpace[s_String /; StringEndsQ[s, " "|"\t"]] := ($lastSpace = True; s);
checkLastSpace[other_] := ($lastSpace = False; other);

Options[styleBoxToMarkdown] = {FontWeight -> "Plain", FontSlant -> "Plain", FontColor -> None};

wordQ[e_] := StringQ[e] && StringMatchQ[e, WordCharacter..];

styleBoxToMarkdown = Case[ 
  StyleBox[e_String, FontSlant -> Italic|"Italic"] /; TrueQ[$lastSpace] && wordQ[e]                             := wrapWith[e, "*"];
  StyleBox[e_String, FontWeight -> Bold|"Bold"] /; TrueQ[$lastSpace] && wordQ[e]                                := wrapWith[e, "**"];
  StyleBox[e_String, FontSlant  -> Italic|"Italic", FontWeight -> Bold|"Bold"] /; TrueQ[$lastSpace] && wordQ[e] := wrapWith[e, "***"];
  StyleBox[e_, rules___] := $styledSpanTemplate[textBoxesToMarkdown @ e, StringRiffle[toStylePropVal /@ {rules}, ";"]];
]

toStylePropVal = Case[
  FontColor -> (color_? ColorQ)       := "color:" <> Image`Utilities`toHEXcolor[color];
  FontWeight -> "Bold"|Bold           := "font-weight:bold";
  FontWeight -> Plain|"Plain"         := "font-weight:normal";
  FontSlant -> "Italic"|Italic        := "font-style:italic";
  FontSlant -> Plain|"Plain"          := "font-style:normal";
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