PrivateVariable[$RomanGreekAlphabet]

$RomanGreekAlphabet = Chars @ "abcdefghijklmnopqrstuvwxyz\[Alpha]\[Beta]\[Gamma]\[Delta]\[CurlyEpsilon]\[Zeta]\[Eta]\[Theta]\[Iota]\[Kappa]\[Lambda]\[Mu]\[Nu]\[Xi]\[Omicron]\[Pi]\[Rho]\[Sigma]\[Tau]\[Upsilon]\[CurlyPhi]\[Chi]\[Psi]\[Omega]";
$RomanGreekAlphabet = Join[$RomanGreekAlphabet, ToUpperCase @ $RomanGreekAlphabet];

(**************************************************************************************************)

PublicFunction[AlphabetData, AlphabetNames]

PublicVariable[$AlphabetDataTable]

AlphabetNames := (setupAlphabetData[]; AlphabetNames);
AlphabetData := (setupAlphabetData[]; AlphabetData);

$alphabetProps = {"Lowercase", "Uppercase", "Letters", "Digits", "Characters"};
$alphabetMeta = {"Class", "Plain", "Bold", "Italic"};
$alphabetPropsExt = Join[$alphabetProps, StringJoinRight[$alphabetProps, "Regex"], $alphabetMeta];

$alphabetLetterHeads = {AlphabetLetter, LowercaseAlphabetLetter, UppercaseAlphabetLetter, AlphabetDigit, AlphabetCharacter};

setupAlphabetData[] := Module[{data, romanic, greek},
  Clear[AlphabetData, AlphabetNames];
  data = loadAlphabetData[];
  $alphabets = names = Keys @ data;
  $AlphabetDataTable = data;
  DefineFunctionAutocomplete[AlphabetData, Sort /@ {names, $alphabetPropsExt}];
  DefineFunctionAutocomplete[$alphabetLetterHeads, {Sort @ names}];
  AlphabetData::unknownAlphabet = "`` is not a known alphabet, one of ``.";
  AlphabetData::unknownAlphabetProperty = "`` is not a known property, one of ``.";
  AlphabetData[] = names;
  AlphabetData[a_] := alphabetDataFallback[a, "Characters"];
  AlphabetData[a_, f_] := alphabetDataFallback[a, f];
  {romanic, greek} = SelectDiscard[names, StringFreeQ["Greek"]];
  AlphabetNames["Romanic"] = romanic;
  AlphabetNames["Greek"]   = Comp[greek, {"GreekBasic"}];
  AlphabetNames[list_List] := Inter @@ Map[AlphabetNames, list];
  Scan[Set[AlphabetNames[#], Keys @ Select[Key[#]] @ data]&, {"Bold", "Italic", "Plain"}];
  KVScan[setupAlphabet, data];
];

setupAlphabet[name_, data_] := (
  AlphabetData[name] = data["Characters"];
  AlphabetData[name, field_] := Message[AlphabetData::unknownAlphabetProperty, field, $alphabetPropsExt];
  KVScan[{field, value} |-> Set[AlphabetData[name, field], value], data]
);

loadAlphabetData[] := Scope[

  fileStr = ImportUTF8 @ DataPath["Text", "Alphabets.txt"];
  fileRows = StringSplit[StringTrim @ fileStr, "\n"];
  fileItems = PadRight[#, 4, ""]& /@ StringSplit[fileRows, Whitespace];

  {alphabets, lowercase, uppercase, digits} = Transpose @ fileItems;
  letters = ZipMap[SJoin, lowercase, uppercase];
  characters = ZipMap[SJoin, letters, digits];
  regexLists = {lowercase, uppercase, letters, digits, characters};
  charLists = MatrixMap[ExpandLetterClass, regexLists];
  meta = Transpose @ Map[getAlphabetMeta, alphabets];
  alphabetLists = Join[charLists, regexLists, meta];

  UAssocThread[alphabets, UAssocThread[$alphabetPropsExt] /@ Transpose[alphabetLists]]
];

getAlphabetMeta[name_] := {
  If[StringFreeQ[name, "Greek"], "Romanic", "Greek"],
  StringFreeQ[name, "Bold" | "Italic"],
  StringContainsQ[name, "Bold"],
  StringContainsQ[name, "Italic"]
};

alphabetDataFallback[All,       field_] := alphabetDataFallback[$alphabets, field];
alphabetDataFallback[list_List, field_] := SJoin @ Map[AlphabetData[#, field]&, list];
alphabetDataFallback[name_, ___]        := (Message[AlphabetData::unknownAlphabet, name, $alphabets]; $Failed);

(**************************************************************************************************)

PublicStringPattern[AlphabetLetter, LowercaseAlphabetLetter, UppercaseAlphabetLetter, AlphabetDigit, AlphabetCharacter]

DefineStringLetterClass[
  AlphabetLetter[name_]          :> expandAlphabetClass[name, "LettersRegex"],
  LowercaseAlphabetLetter[name_] :> expandAlphabetClass[name, "LowercaseRegex"],
  UppercaseAlphabetLetter[name_] :> expandAlphabetClass[name, "UppercaseRegex"],
  AlphabetDigit[name_]           :> expandAlphabetClass[name, "DigitsRegex"],
  AlphabetCharacter[name_]       :> expandAlphabetClass[name, "CharactersRegex"]
];

expandAlphabetClass[name_,  field_]  := AlphabetData[name, field];
expandAlphabetClass[name_, "Digits"] := AlphabetData[name, field] // Rep[{} :> (AlphabetDigit::noDigits = "No digits for alphabet ``."; "")];

(**************************************************************************************************)

PublicFunction[AlphabetConvert]

AlphabetConvert[str:(_Str | _List), spec_] := SRep[str, alphabetConversionRules @ spec];

(**************************************************************************************************)

AlphabetConvert::invalidRule = "`` is not a valid conversion spec.";

alphabetConversionRules[spec_] := alphabetConversionRules[spec] = iAlphabetConversionRules @ spec;

(* TODO: support digits *)
iAlphabetConversionRules = Case[
  All      -> tgt_Str := %[Decases[AlphabetNames @ AlphabetData[tgt, "Class"], tgt] -> tgt];
  src_Str  -> tgt_Str := RuleThread[AlphabetData[src, "Letters"], AlphabetData[tgt, "Letters"]];
  src_List -> tgt_Str := Catenate @ Map[%[# -> tgt]&, src];
  other_              := Message[AlphabetConvert::invalidRule, other];
];

(**************************************************************************************************)

PublicFunction[ToNonDecoratedRoman, RomanToSanSerif, ScriptToRoman]

ToNonDecoratedRoman[str_] := AlphabetConvert[str, All -> "Roman"];
RomanToSanSerif[str_]     := AlphabetConvert[str, "Roman" -> "SansSerif"];
ScriptToRoman[str_Str]    := AlphabetConvert[str, {"Script", "ScriptPrivate"} -> "Roman"];

(**************************************************************************************************)

PublicFunction[ToSpelledGreek]

$spelledGreek = "alpha beta gamma delta curlyepsilon epsilon zeta eta theta iota kappa lambda mu nu xi pi rho sigma tau curlyphi phi chi psi omega Gamma Delta Theta Lambda Xi Pi Sigma Phi Psi Omega";
SetCached[$toSpelledGreek, RuleThread[AlphabetData["GreekBasic"], SSplit @ $spelledGreek]];

ToSpelledGreek[str_Str] := SRep[str, $toSpelledGreek];
