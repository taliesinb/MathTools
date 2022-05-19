PackageExport["$KatexPrelude"]

importLocalUTF8[localFile__] := ImportUTF8 @ FileNameJoin[{$PackageDirectory, localFile}];

$KatexPrelude = importLocalUTF8["Markdown", "KatexPrelude.tex"];

(**************************************************************************************************)

PackageExport["$SymbolTranslationTable"]

$SymbolTranslationTable = Block[{str},
  rawString = importLocalUTF8["Markdown", "SymbolTranslation.txt"];
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
