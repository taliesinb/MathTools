Begin["`FindSymbol`"];

(*************************************************************************************************)

SymbolGrid[context_String] := Module[{names},
  names = Names[context <> "*"];
  names = Select[names, UpperCaseQ @ StringPart[#, 1]&];
  NiceGridAll[names]
];

(*************************************************************************************************)

FindSymbol::noname = "No symbol matching name \"``\" in \"``\".";

FindSymbol[context_String, name_String] := Block[
  {names, words, pattern, NewSymbolHandler, $NewSymbol, foundName},
  names = FindNames[context, name, NameHasDefinitionsQ];
  names = SortBy[names, {StringCount[#, "`"], StringLength[#]}&];
  If[names =!= {},
    foundName = First @ names;
    If[$lastFoundCache[name] =!= foundName,
      $lastFoundCache[name] = foundName;
      Print["resolving \"", context, name, "\" to \"", foundName, "\"."];
    ];
    First @ names
  ,
    $Failed
  ]
];

NameHasDefinitionsQ[sym_String] := ToExpression[sym, InputForm, System`Private`HasAnyEvaluationsQ];

(*************************************************************************************************)

FindNames::err = "Could not find contexts due to internal error.";
FindNames[str_String] := FindNames @@ StringTakeDrop[str, Max @ StringPosition[str, "`"]];
FindNames[context_String, name_String, filter_:None] := Block[
  {names, tryFind, nameGlob, glob, contextRE, $nameFilter = filter, $ic = False},
  contextRE = contextGlobToRegex[context];
  nameGlob = convertNameGlob[name];
  names = {};
  Which[
    !UpperCaseQ[name] && StringFreeQ[name, "*"] &&
      Length[names = iFindNames[contextRE, True,  name                    ]] == 1, Null,
    StringEndsQ[nameGlob, "*"] &&
      Length[names = iFindNames[contextRE, False, StringDrop[nameGlob, -1]]] >= 1, Null,
      Length[names = iFindNames[contextRE, False, nameGlob                ]] >= 1, Null,
      Length[names = iFindNames[contextRE, False, "*" <> nameGlob         ]] >= 1, Null,
    $ic = True;
      Length[names = iFindNames[contextRE, False, name                    ]] >= 1, Null,
      Length[names = iFindNames[contextRE, False, "*" <> name             ]] >= 1, Null,
      True, Null
  ];
  names
];

contextGlobToRegex[""] := RegularExpression["\\w"];
contextGlobToRegex[glob_String] := Block[
  {res, split},
  split = StringSplit[glob, "`"];
  If[split === {}, Return @ ".*"];
  split = Prepend[firstGlobElemToRegex @ First @ split] @ Map[globElemToRegex, Rest @ split];
  res = StringRiffle[split, "`(?:\\w+`)*"];
  RegularExpression @ If[StringEndsQ[res, "`"], res, res <> "`"]
];

firstGlobElemToRegex["qg" | "mt"] := "(?:MathTools|MTLoader)";
firstGlobElemToRegex["sys"]       := "(?:System|Developer|Language|Internal|PatternConvert|StartUp|StringPattern|Association|Compile|Compiler|CompiledLibrary|ImportExport|LibraryLink|ExternalEvaluate|Data|Documentation|FE|JSONTools|PacletTools|PacletManager|NumericArray|NumericArrayUtilities|NotebookTools|XML|XMLLink|SystemTools|SparseArray|GeometricFunctions|FEPrivate|LocalObjects|GraphComputation|CCompilerDriver|ArchiveTools|DatabaseLink|EntityFramework|Experimental|LinearAlgebra|Java|JLink|Image|GeneralUtilities|Format|Method|ListableFunctionsLibrary|SearchResult|RuntimeTools|StringUtilitiesDump|Documentation|DocumentationSearch|DocumentationSearcher|DateAndTime|Package|Information)";
firstGlobElemToRegex[s_]          := globElemToRegex[s];

globElemToRegex[s_String] /; LowerCaseQ[s] := globElemToRegex @ ToUpperCase[s];
globElemToRegex[s_String] := StringReplace[s, {"Z" -> "\\w*", chunk:$camelChunk :> chunk <> "\\w*"}];

$camelChunk = RegularExpression["[[:upper:]][[:lower:]]*"];

convertNameGlob[glob_] := Which[
  StringContainsQ[glob, "z"],
    StringReplace[glob, "z" -> "*"],
  LowerCaseQ[glob],
    glob,
  UpperCaseQ[glob],
    StringReplace[glob, l:LetterCharacter :> l <> "@"],
  True,
    StringReplace[glob, chunk:$camelChunk :> chunk <> "*"]
];

iFindNames[contextPatt_, isExact_, symbolGlob_String] := Block[
  {candidates, symbolNames, pureNames},
  candidates = Block[{$ContextPath = {}}, Names["**`" <> symbolGlob, IgnoreCase -> $ic]];
  candidates = Select[candidates, !StringStartsQ[#, {"f`", "l`"}]&];
  filter = StringStartsQ[candidates, contextPatt];
  candidates = Pick[candidates, filter];
  pureNames = Part[StringSplit[candidates, "`"], All, -1];
  If[isExact, Return @ Pick[candidates, pureNames, symbolGlob]];
  cands = Pick[candidates, StringMatchQ[pureNames, symbolGlob, IgnoreCase -> $ic]];
  If[$nameFilter =!= None, cands = Select[cands, $nameFilter]];
  SortBy[cands, StringLength]
];

(*************************************************************************************************)

NewSymbolHandler[name_String, context_String ? (StringStartsQ[{"mt`", "gu`", "sp`"}])] /; ($Context === "Global`") := Block[
  {NewSymbolHandler, $NewSymbol},
  setupAliasSymbol[context <> name, context, name]
];

NewSymbolHandler[name_String, context_String ? (StringStartsQ["f`"])] /; ($Context === "Global`") := Block[
  {NewSymbolHandler, $NewSymbol},
  setupAliasSymbol[context <> name, StringDrop[context, 2], name]
];

setupAliasSymbol[alias_, context_, name_] :=
  setupAliasSymbol2[Clear[alias]; alias, context, name, FindSymbol[context, name]];

setupAliasSymbol2[alias_, context_, name_, target_String] :=
  aliasSetter @ ToExpression[{alias, target}, InputForm, HoldComplete];

aliasSetter[{HoldComplete[s_], HoldComplete[t_]}] :=
  (SetDelayed[s, t]; t);

(* if we couldn't find it, make sure we keep retrying to find it *)
setupAliasSymbol2[alias_, context_, name_, _] := With[
  {symbol = Symbol @ alias},
  SetDelayed[symbol, RuleCondition @ setupAliasSymbol[alias, context, name]];
  Fail
];

(*************************************************************************************************)

NewSymbolHandler[name_String, context_String ? (StringStartsQ["l`"])] /; ($Context === "Global`") := Block[
  {NewSymbolHandler, $NewSymbol},
  setupListSymbol[context <> name, StringDrop[context, 2], name]
];

setupListSymbol[lister_, context_, name_] := (
  Clear[lister];
  With[{listerSymbol = Symbol[lister]},
    listerSymbol := FindNames[context, name, NameHasDefinitionsQ]]
);

$NewSymbol = NewSymbolHandler;

End[];