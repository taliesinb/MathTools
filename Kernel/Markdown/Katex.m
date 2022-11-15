PublicFunction[ToKatexString]

ToKatexString[e_] := boxesToKatexString @ ToBoxes[e, StandardForm];

(**************************************************************************************************)

PrivateFunction[boxesToKatexString]

ToKatexString::partial = "Could not fully evaluate katex equivalents.";

boxesToKatexString[boxes_] := Scope[
  $inputBoxes = boxes;
  interm = boxToKatex @ cleanupInlineBoxes @ boxes;
  flatTerms = Flatten @ List @ ReplaceRepeated[$katexAppliedRule] @ interm;
  If[!StringVectorQ[flatTerms], ReturnFailed["partial"]];
  katexString = StringJoin @ flatTerms;
  StringTrim @ StringReplace[$WLSymbolToKatexRegex] @ katexString
];

$katexAppliedRule = {
  (s_String)[args___] :> {"\\" <> s <> "{", Riffle[{args}, "}{"], "}"}
}

boxToKatex = Case[
  "," := ",";
  " " := " ";
  "_" := "\\_";
  e_String := e;
  
  KatexFunction[s_String] := PrefixSlash @ s;
  KatexFunction[s_String][args___] := Map[%, s[args]];

  (* process results of dispatchTemplateBox: *)
  e_List := Map[%, e];
  e:(_String[___]) := Map[%, e];

  c_Cell := Block[{$inlineMathTemplate = Identity}, textBoxesToMarkdownInner @ c];
  TemplateBox[args_, tag_] := templateBoxToKatex[tag -> args];

  StyleBox[e_, "Text"] := {"\\textrm{", boxToInlineText @ e, "}"};
  StyleBox[e_, directives___] := Fold[applyStyle, % @ e, {directives}];
  DynamicBox[e_, ___] := % @ e;

  AdjustmentBox[e_, BoxBaselineShift -> n_] := {"\\raisebox{" <> TextString[-n] <> "em}{", % @ e, "}"};
  
  OverscriptBox[e_, "^"] := {"\\hat{", % @ e, "}"};
  OverscriptBox[e_, "\[RightVector]"] := {"\\vector{", % @ e, "}"};
  OverscriptBox[e_, "~"] := {"\\tilde{", % @ e, "}"};
  OverscriptBox[e_, "_"] := {"\\overline{", % @ e, "}"};
  OverscriptBox[e_, "."] := {"\\dot{", % @ e, "}"};
  OverscriptBox[e_, f_] := {"\\overset{", % @ f, "}{", % @ e, "}"};

  UnderscriptBox[e_, "_"] := {"\\underline{", % @ e, "}"};
  UnderscriptBox[e_, "."] := {"\\underdot{", % @ e, "}"};
  UnderscriptBox[e_, f_] := {"\\underset{", % @ f, "}{", % @ e, "}"};

  SuperscriptBox[e_, b_] := {% @ e, "^", toBracket @ b};
  SubsuperscriptBox[e_, sub_, sup_] := {% @ e, "_", toBracket @ sub, "^", toBracket @ sup};
  SubscriptBox[e_, b_] := {% @ e, "_", toBracket @ b};
  OverscriptBox[e_, "^"] := {"\\hat{", % @ e, "}"};
  
  RowBox[{a_, "\[DirectedEdge]", b_}] := "de"[% @ a, % @ b];
  RowBox[{a_, "\[UndirectedEdge]", b_}] := "ue"[% @ a, % @ b];
  RowBox[{"{", e__, "}"}] := {"\{", % /@ {e}, "\}"};
  RowBox[e_] := Map[%, e];

  ButtonBox[title_, BaseStyle -> "Hyperlink"|Hyperlink, ButtonData -> {URL[url_String], _}, ___] :=
    {"\\href{", url, "}{", % @ title, "}"};

  TagBox[e_, __] := % @ e;
  RowBox[{"(", "\[NoBreak]", GridBox[grid_, ___], "\[NoBreak]", ")"}] := {"\\begin{pmatrix}", StringRiffle[MatrixMap[%, grid], "\\\\", "&"], "\\end{pmatrix}"};
  UnderoverscriptBox[e_, b_, c_] := % @ SuperscriptBox[SubscriptBox[e, b], c];
  FractionBox[a_, b_] := {"\\frac{", % @ a, "}{", % @ b, "}"};
  RowBox[list_] := Map[%, list];

  other_ := Scope[
    head = ToPrettifiedString @ Head @ other;
    Message[ToKatexString::badbox, MsgExpr @ other];
    Print["OUTER BOXES:"]; Print @ ToPrettifiedString[$inputBoxes, MaxDepth -> 3, MaxLength -> 10];
    Print["RAW BOXES:"]; Print @ ToPrettifiedString[other, MaxDepth -> 3, MaxLength -> 10];
    Print["FORMATTED BOXES:"]; Print @ RawBoxes @ Replace[other, BoxData[bd_] :> bd];
    StringJoin["\\noKatexForm{", StringReplace[head, $katexEscape], "}"]
  ];
];

$katexEscape = {
  "\\" -> "/"
}

boxToInlineText[e_] := TextString[e];

templateBoxToKatex = Case[
  "Naturals" -> {}                  := "\\mathbb{N}";
  "PositiveNaturals" -> {}          := "\\mathbb{N}^+";
  "PositiveReals" -> {}             := "\\mathbb{R}^+";
  "Primes" -> {}                    := "\\mathbb{P}";
  "Integers" -> {}                  := "\\mathbb{Z}";
  "Rationals" -> {}                 := "\\mathbb{Q}";
  "Reals" -> {}                     := "\\mathbb{R}";
  "Complexes" -> {}                 := "\\mathbb{C}";
  "Superscript" -> {a_, b_}         := $ @ SuperscriptBox[a, b];
  "Subscript" -> {a_, b_}           := $ @ SubscriptBox[a, b];
  "DirectedEdge" -> {a_, b_, t_}    := "tde"[$ @ a, $ @ b, $ @ t];
  "UndirectedEdge" -> {a_, b_, t_}  := "ude"[$ @ a, $ @ b, $ @ t];
  "Subsuperscript" -> {a_, b_, c_}  := $ @ SuperscriptBox[SubscriptBox[a, b], c];
  "ContractionProduct" -> t_        := dispatchTemplateBox["ContractionProductForm", t]; (* legacy *)
  "ContractionProductSymbol" -> t_  := dispatchTemplateBox["contractionProductForm", t]; (* legacy *)
  "ContractionSum" -> t_            := dispatchTemplateBox["ContractionSumForm", t]; (* legacy *)
  "ContractionSumSymbol" -> t_      := dispatchTemplateBox["contractionSumForm", t]; (* legacy *)
  "ContractionLatticeSymbolForm" -> t_  := dispatchTemplateBox["ContractionLatticeSymbol", t]; (* legacy *)
  "IsContractedInForm" -> t_        := dispatchTemplateBox["IsContractedForm2", t]; (* legacy *)
  "IsNotContractedInForm" -> t_     := dispatchTemplateBox["IsNotContractedForm2", t]; (* legacy *)
  tag_ -> args_                     := dispatchTemplateBox[tag, args]
,
  {$ -> boxToKatex}
];

ToKatexString::badbox = "Box expression `` has no Katex conversion.";
ToKatexString::badtemplatebox = "TemplateBox `` corresponding to `` has no $TemplateKatexFunction defined.";

dispatchTemplateBox[tag_, args_] := Scope[
  fn = Lookup[$TemplateKatexFunction, tag, Lookup[$templateToKatexFunction, tag, None]];
  If[fn === None && AssociationQ[$localTemplateToKatexFunctions],
    fn = Lookup[$localTemplateToKatexFunctions, tag, None]];
  If[fn === None,
    (* this is slower than the others, but will allow ToKatexString to pick up local styles when run interactively *)
    fn = CurrentValue[{TaggingRules, "TemplateToKatexFunctions", tag}];
    If[fn === Inherited, fn = None];
  ];
  If[fn === None,
    Message[ToKatexString::badtemplatebox, tag, Framed @ RawBoxes[TemplateBox[args, tag]]];
    Return["badDispatch"[tag]];
  ];
  res = fn @@ args;
  boxToKatex @ res (* recurese *)
];

applyStyle = Case[

  Sequence[e_, (FontWeight -> "Bold" | Bold) | "Bold" | Bold]         := {"\\mathbf{", e, "}"};

  Sequence[e_, (FontWeight -> "Italic" | Italic) | "Italic" | Italic] := {"\\mathif{", e, "}"};
  
  Sequence[e_, (FontColor -> c_) | (c_ ? ColorQ)]                     := {"\\textcolor{#", ColorHexString @ c, "}{", e, "}"};

  Sequence[e_, style_] := e;
];


applyInlineStyle[e_, _] := e;

toBracket = Case[
  e_String /; StringLength[e] === 1 := e;
  other_ := {"{", boxToKatex @ other, "}"};
];

cleanupInlineBoxes = RightComposition[
  ReplaceRepeated @ {
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
