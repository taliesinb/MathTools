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
    re :> StringJoin["{", $table["$1"], "}"],
    re :> $table["$1"]
  ]
];

PackageExport["$WLSymbolToKatexRegex"]

$WLSymbolToKatexRegex = makeLiteralReplacementRule[SymbolTranslationData[<|"Symbol" -> "Katex"|>], True]

PackageExport["$WLSymbolToUnicode"]

$WLSymbolToUnicode = makeLiteralReplacementRule[SymbolTranslationData[<|"Symbol" -> "Unicode"|>], False]

(**************************************************************************************************)

PackageExport["ToKatexString"]

ToKatexString[e_] := Scope[
  toKatexString @ ToBoxes[e, StandardForm]
]

PackageScope["boxesToKatexString"]

boxesToKatexString[e_] :=
  StringTrim @ StringReplace[$WLSymbolToKatexRegex] @ StringJoin @ 
    iBoxesToKatexString @ cleanupInlineBoxes @ e;

iBoxesToKatexString = Case[
  RowBox[{"{", e__, "}"}] := {"\{", % /@ {e}, "\}"};
  RowBox[e_] := Map[%, e];
  "," := ",";
  " " := " ";
  "_" := "\\_";
  e_String := e;
  c_Cell := Block[{$inlineMathTemplate = Identity}, iTextCellToMD @ c];
  TemplateBox[{}, "Integers"] := "\\mathbb{Z}";
  TemplateBox[{}, "Rationals"] := "\\mathbb{Q}";
  TemplateBox[{}, "Reals"] := "\\mathbb{R}";
  TemplateBox[{}, "Complexes"] := "\\mathbb{C}";
  StyleBox[e_, opts___] := applyInlineStyle[% @ e, Lookup[{opts}, {FontWeight, FontSlant, FontColor}, None]];
  UnderscriptBox[e_, "_"] := {"\\underline{", % @ e, "}"};
  OverscriptBox[e_, "_"] := {"\\overline{", % @ e, "}"};
  SuperscriptBox[e_, b_] := {% @ e, "^", toBracket @ b};
  SubsuperscriptBox[e_, sub_, sup_] := {% @ e, "_", toBracket @ sub, "^", toBracket @ sup};
  SubscriptBox[e_, b_] := {% @ e, "_", toBracket @ b};
  OverscriptBox[e_, "^"] := {"\\hat{", % @ e, "}"};
  TemplateBox[{e_, b_, c_}, "Subsuperscript"] := % @ SuperscriptBox[SubscriptBox[e, b], c];
  TagBox[e_, _] := % @ e;
  RowBox[{"(", "\[NoBreak]", GridBox[grid_, ___], "\[NoBreak]", ")"}] := {"\\begin{pmatrix}", StringRiffle[Map[%, grid, {2}], "\\\\", "&"], "\\end{pmatrix}"};
  UnderoverscriptBox[e_, b_, c_] := % @ SuperscriptBox[SubscriptBox[e, b], c];
  TemplateBox[{a_, b_, tag_}, "DirectedEdge"] :=
    {% @ a, "\\overset{", % @ tag, "}{\[DirectedEdge]}", % @ b};
  FractionBox[a_, b_] := {"\\frac{", a, "}{", b, "}"};
  RowBox[list_] := Map[%, list];
  other_ := (Print["UNRECOGNIZED: ", other]; Abort[])
];

(* TODO: helper for making latex calls *)

applyInlineStyle[e_, {_, _, c:$ColorPattern}] :=
 {"\\textcolor{#", ColorHexString @ c, "}{", e, "}"};

applyInlineStyle[e_, _] := e;

toBracket = Case[
  e_String /; StringFreeQ[e, " "] := e;
  other_ := {"{", iBoxesToKatexString @ other, "}"};
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
