PackageExport["$KatexPrelude"]

importUTF8[localFile_] :=
  Import[FileNameJoin[{$PackageDirectory, localFile}], "Text", CharacterEncoding -> "UTF8"];

(**************************************************************************************************)

PackageExport["$KatexPrelude"]

$KatexPrelude = importUTF8["KatexPrelude.txt"];

(**************************************************************************************************)

PackageExport["$SymbolTranslationTable"]

$SymbolTranslationTable = Block[{str},
  rawString = importUTF8["SymbolTranslation.txt"];
  rawString //= StringReplace[{StartOfLine ~~ " "... ~~ "\n" -> "", " \\" -> " \\\\", "\"" -> "\\\""}];
  parsedString = StringTrim @ ToExpression["\"" <> rawString <> "\"", InputForm];
  table = StringExtract[parsedString, "\n" -> All, " ".. -> All] /. "_" -> None;
  table
];

(**************************************************************************************************)

PackageExport["SymbolTranslationData"]

SymbolTranslationData[assoc_Association] :=
  Association @ SymbolTranslationData[Normal @ assoc];

SymbolTranslationData[schema_] := Scope[
  func = Construct[Function, schema /. {
    "Symbol" -> #1, "InputForm" -> #2, "Katex" -> #3, "Unicode" -> #4
  }];
  results = func @@@ $SymbolTranslationTable;
  Discard[results, ContainsQ[None]]
];

(**************************************************************************************************)

makeLiteralReplacementRule[assoc_, wrap_] := ModuleScope[
  keys = Keys[assoc];
  patt = StringJoin["(", Riffle[keys, "|"], ")"];
  re = RegularExpression @ StringReplace[patt, "$" -> "\\$"];
  $table = assoc;
  If[wrap,
    re :> " " <> $table["$1"] <> " ",
    re :> $table["$1"]
  ]
];

PackageExport["$WLSymbolToKatexRegex"]

$WLSymbolToKatexRegex = makeLiteralReplacementRule[SymbolTranslationData[<|"Symbol" -> "Katex"|>], True]

PackageExport["$WLSymbolToUnicode"]

$WLSymbolToUnicode = makeLiteralReplacementRule[SymbolTranslationData[<|"Symbol" -> "Unicode"|>], False]

(**************************************************************************************************)

PackageScope["$TemplateKatexFunction"]

$TemplateKatexFunction = <||>;

(**************************************************************************************************)

PackageExport["ToKatexString"]

ToKatexString[e_] := Scope[
  boxesToKatexString @ ToBoxes[e, StandardForm]
]

PackageScope["boxesToKatexString"]

boxesToKatexString[e_] :=
  StringTrim @ StringReplace[$WLSymbolToKatexRegex] @ StringJoin @
    ReplaceRepeated[$katexAppliedRule] @ boxToKatex @ cleanupInlineBoxes @ e;

$katexAppliedRule = {
  (s_String)[args___] :> {"\\" <> s <> "{", Riffle[{args}, "}{"], "}"}
}


PackageScope["boxToKatex"]

boxToKatex = Case[
  "," := ",";
  " " := " ";
  "_" := "\\_";
  e_String := e;
  
  (* process results of dispatchTemplateBox: *)
  e_List := Map[%, e];
  e:(_String[___]) := Map[%, e];

  c_Cell := Block[{$inlineMathTemplate = Identity}, iTextCellToMD @ c];
  TemplateBox[args_, tag_] := templateBoxToKatex[tag -> args];
  StyleBox[e_, opts___] := applyInlineStyle[% @ e, Lookup[Select[{opts}, RuleQ], {FontWeight, FontSlant, FontColor}, None]];
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

  TagBox[e_, _] := % @ e;
  RowBox[{"(", "\[NoBreak]", GridBox[grid_, ___], "\[NoBreak]", ")"}] := {"\\begin{pmatrix}", StringRiffle[Map[%, grid, {2}], "\\\\", "&"], "\\end{pmatrix}"};
  UnderoverscriptBox[e_, b_, c_] := % @ SuperscriptBox[SubscriptBox[e, b], c];
  FractionBox[a_, b_] := {"\\frac{", a, "}{", b, "}"};
  RowBox[list_] := Map[%, list];
  
  other_ := "UNRECOGNIZED " <> ToString[other, InputForm];
];

templateBoxToKatex = Case[
  "Naturals" -> {}                  := "\\mathbb{N}";
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

dispatchTemplateBox[tag_, args_] := Scope[
  fn = Lookup[$TemplateKatexFunction, tag, None];
  If[fn === None, Print[tag]; Return["UNHANDLED " <> tag]];
  res = fn @@ args;
  boxToKatex @ res (* recurese *)
];

applyInlineStyle[e_, {_, _, c:$ColorPattern}] :=
 {"\\textcolor{#", ColorHexString @ c, "}{", e, "}"};

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
