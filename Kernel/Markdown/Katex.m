PrivateFunction[toProcessedKatexString]

toProcessedKatexString[boxes_] /; StrQ[$localKatexDefinitions] := Block[
  {localDefs = $localKatexDefinitions, res},
  Clear[$localKatexDefinitions];
  SJoin[localDefs, "\n", toProcessedKatexString @ boxes]
];

toProcessedKatexString[box_] := $katexPostprocessor @ boxesToKatexString @ box;

(**************************************************************************************************)

PublicFunction[ToKatexString]

(* TraditionalForm ensures that application uses parentheses *)
ToKatexString[e_] := boxesToKatexString @ ToBoxes[e, TraditionalForm];

(**************************************************************************************************)

PrivateFunction[boxesToKatexString]

ToKatexString::partial = "Could not fully evaluate katex equivalents.";

boxesToKatexString[boxes_] := Scope[
  $inputBoxes = boxes;
  evalBoxes = cleanupInlineBoxes @ boxes;
  interm = boxToKatex @ evalBoxes;
  flatTerms = Flatten @ List @ RepRep[$katexAppliedRule] @ interm;
  If[!StrVecQ[flatTerms], ReturnFailed["partial"]];
  katexString = SJoin @ flatTerms;
  STrim @ SDelete[$spuriousKatexSubstrings] @ SRep[$scriptToCaligraphic] @ SRep[$WLSymbolToKatexRegex] @ katexString
];

(* InvisibleApplication is from TraditionalForm of f[1,2,3], newline is from grids *)
$spuriousKatexSubstrings = "\[InvisibleApplication]" | "\n"

SetCached[$scriptToCaligraphic, z:AlphabetCharacter["Script"].. :> "\\mathcal{" <> ScriptToRoman[z] <> "}"];

$katexAppliedRule = {
  (s_Str)[args___] :> {"\\" <> s <> "{", Riffle[{args}, "}{"], "}"}
}

PrivateFunction[boxToKatex]

boxToKatex = Case[
  "," := ",";
  " " := " ";
  "_" := "\\_";
  "->"|"\[Rule]" := " \\to ";
  "<|" := " \\left<\\left| ";
  "|>" := " \\right|\\right> ";
  e_Str := e;
  
  KatexFunction[s_Str] := PrefixSlash @ s;
  KatexFunction[s_Str][args___] := Map[%, s[args]];

  (* process results of dispatchTemplateBox: *)
  e_List := Map[%, e];
  e:(_Str[___]) := Map[%, e];

  c_Cell := Block[{$inlineMathTemplate = Id}, textBoxesToMarkdown @ c];

  TemplateBox[{a_, b_}, "katexSwitch"] := % @ b; (* <- shortcircuit, since it is so common *)
  tb:TemplateBox[_List, _Str] := TemplateBoxToKatex[tb];

  StyleBox[e_, "Text"] := {"\\textrm{", boxToInlineText @ e, "}"};
  StyleBox[e_, directives___] := katexStyleOperator[directives] @ % @ e;
  DynamicBox[e_, ___] := % @ e;

  AdjustmentBox[e_, BoxBaselineShift -> n_] := {"\\raisebox{" <> TextString[-n] <> "em}{", % @ e, "}"};
  AdjustmentBox[e_, BoxMargins -> {{l_, r_}, {0, 0}}] := katexPadded[% @ e, {l, r}];
  
  OverscriptBox[e_, "^"] := {"\\hat{", % @ e, "}"};
  OverscriptBox[e_, "\[RightVector]"] := {"\\vector{", % @ e, "}"};
  OverscriptBox[e_, "~"] := {"\\tilde{", % @ e, "}"};
  OverscriptBox[e_, "_"] := {"\\overline{", % @ e, "}"};
  OverscriptBox[e_, "."] := {"\\dot{", % @ e, "}"};
  OverscriptBox[e_, f_] := {"\\overset{", % @ f, "}{", % @ e, "}"};

  UnderscriptBox[e_, "_"] := {"\\underline{", % @ e, "}"};
  UnderscriptBox[e_, "."] := Part[UnderdotBox[% @ e], {1, 2}];
  UnderscriptBox[e_, f_] := {"\\underset{", % @ f, "}{", % @ e, "}"};

  SuperscriptBox[e_, b_] := {% @ e, "^", toBracket @ b};
  SubsuperscriptBox[e_, sub_, sup_] := {% @ e, "_", toBracket @ sub, "^", toBracket @ sup};
  SubscriptBox[e_, b_] := {% @ e, "_", toBracket @ b};
  OverscriptBox[e_, "^"] := {"\\hat{", % @ e, "}"};
  
  RowBox[{h_, "[", RowBox[args_List], "]"}] := {% @ h, "(", Map[%, args], ")"};

  RowBox[{a_, "\[DirectedEdge]", b_}] := % @ TBox[a, b, "DirectedEdgeForm"];
  RowBox[{a_, "\[UndirectedEdge]", b_}] := % @ TBox[a, b, "UndirectedEdgeForm"];
  RowBox[{a_, "\[Function]", b_}] := % @ TBox[a, b, "MapsToForm"];

  RowBox[{"{", e__, "}"}] := {"\\{", % /@ {e}, "\\}"};
  RowBox[{"(", e__, ")"}] := {"\\left(", % /@ {e}, "\\right)"};
  RowBox[e_] := Map[%, e];

  ButtonBox[title_, BaseStyle -> "Hyperlink"|Hyperlink, ButtonData -> {URL[url_Str], _}, ___] :=
    {"\\href{", url, "}{", % @ title, "}"};

  TagBox[e_, __] := % @ e;
  RowBox[{"(", "\[NoBreak]", GridBox[grid_, ___], "\[NoBreak]", ")"}] := {"\\begin{pmatrix}", SRiffle[MatrixMap[%, grid], "\\\\", "&"], "\\end{pmatrix}"};
  UnderoverscriptBox[e_, b_, c_] := % @ SuperscriptBox[SubscriptBox[e, b], c];
  FractionBox[a_, b_] := {"\\frac{", % @ a, "}{", % @ b, "}"};
  RowBox[list_] := Map[%, list];

  other_ := Scope[
    head = ToPrettifiedString @ H @ other;
    Message[ToKatexString::badbox, other];
    Print["OUTER BOXES:"]; Print @ ToPrettifiedString[$inputBoxes, MaxDepth -> 3, MaxLength -> 10];
    Print["RAW BOXES:"]; Print @ ToPrettifiedString[other, MaxDepth -> 3, MaxLength -> 10];
    Print["FORMATTED BOXES:"]; Print @ RawBoxes @ Rep[other, BoxData[bd_] :> bd];
    errorKatex @ SRep[head, $katexEscape]
  ];
];

ToKatexString::badbox = "Box expression `` has no Katex conversion.";

$katexEscape = {
  "\\" -> "/"
}

boxToInlineText[e_] := TextString[e];

(**************************************************************************************************)

katexStyleOperator[args___] := Fold[#1 /* styleToKatexFunction[#2]&, Id, {args}];

styleToKatexFunction := Case[
  (FontColor -> c_) | (c_ ? ColorQ)                           := SJoin["textcolor{#", HexColorString @ c, "}"];
  (FontWeight -> "Bold"|Bold) | "Bold"|Bold                   := "mathbf";
  (FontWeight -> "Italic"|Italic) | "Italic"|Italic           := "mathit";
  Underlined                                                  := "underline";
  Struckthrough                                               := "struckthrough";
  "MathText" | "MathTextFont"                                 := "textrm";
  "RomanMathFont"                                             := "mathrm";
  "FrakturMathFont"                                           := "mathfrak";
  "SansSerifMathFont"                                         := "mathsf";
  "ScriptMathFont"                                            := "mathcal";
  "TypewriterMathFont"                                        := "mathtt";
  "PreformattedCode"                                          := "mathtt";
  _                                                           := Id;
];


toBracket = Case[
  e_Str /; SLen[e] === 1 := e;
  other_ := {"{", boxToKatex @ other, "}"};
];

(**************************************************************************************************)

PrivateFunction[cleanupInlineBoxes]

cleanupInlineBoxes = RightComposition[
  RepRep @ {
    FormBox[e_, TraditionalForm] :> e,
    TemplateBox[a_, b_, __] :> TemplateBox[a, b],
    InterpretationBox[e_, ___] :> e,
    TemplateBox[{a_, RowBox[{b_, rest__}], c_}, "DirectedEdge"] :>
      RowBox[{
        TemplateBox[{a, b, c}, "DirectedEdge"],
        rest
      }]
  }
];

(**************************************************************************************************)

PrivateFunction[errorKatex]

errorKatex[tag_] := SJoin["""\textbf{\textcolor{e1432d}{bad template box: """, tag, "}}"];

(**************************************************************************************************)

PublicFunction[TemplateBoxToKatex]

ToKatexString::badtemplatebox = "TemplateBox `` corresponding to `` has no $TemplateKatexFunction defined.";

TemplateBoxToKatex[TemplateBox[args_List, tag_Str]] := Scope[
  fn = Lookup[$katexDisplayFunction, tag, None];
  If[fn === None && AssocQ[$localKatexDisplayFunction],
    fn = Lookup[$localKatexDisplayFunction, tag, None]];
  If[fn === None,
    (* this is slower than the others, but will allow ToKatexString to pick up local styles when run interactively *)
    fn = CurrentValue[{TaggingRules, "KatexDisplayFunctions", tag}];
    If[fn === Inherited, fn = None];
  ];
  If[fn === None,
    Message[ToKatexString::badtemplatebox, tag, Framed @ RawBoxes[TemplateBox[args, tag]]];
    Return @ errorKatex @ tag;
  ];
  res = fn @@ args;
  boxToKatex @ res (* recurse *)
];

