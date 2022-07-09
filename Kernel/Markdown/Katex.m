PublicFunction[ToKatexString]

ToKatexString[e_] := boxesToKatexString @ ToBoxes[e, StandardForm];

(**************************************************************************************************)

PrivateFunction[boxesToKatexString]

boxesToKatexString[boxes_] := Scope[
  $currentKatexInputBoxes = cleanupInlineBoxes @ boxes;
  katexString = StringJoin @ ReplaceRepeated[$katexAppliedRule] @ boxToKatex @ $currentKatexInputBoxes;
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

  c_Cell := Block[{$inlineMathTemplate = Identity}, textBoxesToMarkdown @ c];
  TemplateBox[args_, tag_] := templateBoxToKatex[tag -> args];

  StyleBox[e_, "Text"] := {"\\textrm{", boxToInlineText @ e, "}"};
  StyleBox[e_, directives___] := Fold[applyStyle, % @ e, {directives}];
  
  UnderscriptBox[e_, "_"] := {"\\underline{", % @ e, "}"};
  OverscriptBox[e_, "_"] := {"\\overline{", % @ e, "}"};
  SuperscriptBox[e_, b_] := {% @ e, "^", toBracket @ b};
  SubsuperscriptBox[e_, sub_, sup_] := {% @ e, "_", toBracket @ sub, "^", toBracket @ sup};
  SubscriptBox[e_, b_] := {% @ e, "_", toBracket @ b};
  OverscriptBox[e_, "^"] := {"\\hat{", % @ e, "}"};
  
  RowBox[{a_, "\[DirectedEdge]", b_}] := "de"[% @ a, % @ b];
  RowBox[{a_, "\[UndirectedEdge]", b_}] := "ue"[% @ a, % @ b];
  RowBox[{"{", e__, "}"}] := {"\{", % /@ {e}, "\}"};
  RowBox[e_] := Map[%, e];

  TagBox[e_, __] := % @ e;
  RowBox[{"(", "\[NoBreak]", GridBox[grid_, ___], "\[NoBreak]", ")"}] := {"\\begin{pmatrix}", StringRiffle[MatrixMap[%, grid], "\\\\", "&"], "\\end{pmatrix}"};
  UnderoverscriptBox[e_, b_, c_] := % @ SuperscriptBox[SubscriptBox[e, b], c];
  FractionBox[a_, b_] := {"\\frac{", a, "}{", b, "}"};
  RowBox[list_] := Map[%, list];

  other_ := (
    Message[ToKatexString::badbox, InputForm @ other, Framed @ RawBoxes[other]];
    StringJoin["\\noKatexForm{", StringReplace[ToString[Head @ other, InputForm], $katexEscape], "}"]
  );
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
  "DirectedEdge" -> {a_, b_, t_}    := "tde"[$ @ a, $ @ b, $ @ t];
  "UndirectedEdge" -> {a_, b_, t_}  := "ude"[$ @ a, $ @ b, $ @ t];
  "Subsuperscript" -> {a_, b_, c_}  := $ @ SuperscriptBox[SubscriptBox[a, b], c];
  tag_ -> args_                     := dispatchTemplateBox[tag, args]
,
  {$ -> boxToKatex}
];

ToKatexString::badbox = "Box `` correspoding to `` has no Katex conversion.";
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
    AdjustmentBox[a_, ___] :> a,
    TemplateBox[{a_, RowBox[{b_, rest__}], c_}, "DirectedEdge"] :>
      RowBox[{
        TemplateBox[{a, b, c}, "DirectedEdge"],
        rest
      }]
  }
];
