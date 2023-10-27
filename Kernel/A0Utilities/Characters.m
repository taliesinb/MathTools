$mathCharacterTable := $mathCharacterTable = computeSymbolTranslationTable[];

(* format: WL string, WL input form, katex, unicode *)
computeSymbolTranslationTable[] := Block[{str},
  rawString = ImportUTF8 @ LocalPath["Kernel", "A0Utilities", "CharacterData.txt"];
  $groups = {};
  rawString //= StringReplace[{StartOfLine ~~ " "... ~~ "\n" -> "", " \\" -> " \\\\", "\"" -> "\\\""}];
  rawString //= StringReplace[{StartOfLine ~~ l:("\\[" ~~ w:LetterCharacter.. ~~ "]") :> StringJoin[l, " ", w], StartOfLine ~~ "_" -> "_ _"}];
  parsedString = StringTrim[ToExpression["\"" <> rawString <> "\"", InputForm], " " | "\n"];
  table = StringExtract[parsedString, "\n" -> All, " ".. -> All] /. "_" -> None;
  Map[parseLine, table]
];

parseLine = Case[
  {"#", groups__} := (Set[$groups, {groups}]; Nothing);
  e:{_, _, _, _, _}  := Append[e, $groups];
  o_              := Print["Unrecognized: ", o]
];

(* parseCharacterLine = Case[
  str_ /; StringStartsQ[str, "#"] := ($currentGroups = StringTrim @ StringSplit @ StringDrop[str, 2]);
  str_ ? WhitespaceQ := Nothing;
  line_ :=
];
 *)
(**************************************************************************************************)

PublicFunction[MathCharacterData]

SetUsage @ "
MathCharacterData[query$] looks up character translation data and gives a list of results.
MathCharacterData[query$, groups$] returns only those elements in the given groups.
* query$ is an expression in which any known field will be substituted with table entries to form a result.
* known fields and an example:
| 'Symbol' | WL string | '\\[RightArrow]' |
| 'Name' | name of WL character | 'RightArrow' |
| 'InputForm' | WL input form | None |
| 'Katex' | Katex expression | '\\rarr' |
| 'Unicode' | corresponding unicode string | '→' |
| 'Code' | integer unicode codepoint | 8594 |
| 'HexCode' | hex form of codepoint | '2192' |
| 'Groups' | list of groups the symbol belongs to | {'Arrow'} |
* the option 'AllowNone' -> True will include entries that do not have a value for one of the requested fields.
"

Clear[MathCharacterData];
Options[MathCharacterData] = {
  "AllowNone" -> False
};

MathCharacterData[assoc_Association, args___] :=
  Association @ MathCharacterData[Normal @ assoc, args];

MathCharacterData[schema_, groups:Except[_Rule]:{}, OptionsPattern[]] := Scope[
  UnpackOptions[allowNone];
  func = Construct[Function, schema] /. {
    "Symbol" -> #1, "Name" -> #2, "InputForm" -> #3, "Katex" -> #4, "Unicode" -> #5, "Groups" -> #6,
    "Code" :> If[#5 === None, None, First @ ToCharacterCode[#5]],
    "HexCode" :> If[#5 === None, None, IntegerString[First @ ToCharacterCode[#5],16, 4]]
  };
  table = $mathCharacterTable;
  If[groups =!= {}, groups //= ToList; table //= Select[SubsetQ[Part[#, 6], groups]&]];
  results = func @@@ table;
  If[!allowNone, results //= Discard[ContainsQ[None]]];
  results
];

(**************************************************************************************************)