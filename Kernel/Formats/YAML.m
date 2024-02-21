PublicFunction[ExportYAMLDict]

(* this does not support nesting of dicts or lists *)
ExportYAMLDict[e_Assoc] := Scope @ CatchMessage[
  $inList = False;
  StringTrim @ StringJoin @ toYamlDict @ e
];

General::badYamlDict = "Value `` is not an association.";

ExportYAMLDict[e_] := (Message[ExportYAMLDict::badYamlDict, MsgExpr @ e]; $Failed);

(*************************************************************************************************)

toYamlDict[assoc_Assoc] := KeyValueMap[
  {k, v} |-> {toYamlDictKey @ k, ": ", toYamlDictValue @ v},
  assoc
];

General::badYamlDictKey = "Association key `` is not a string.";
toYamlDictKey = Case[
  s_Str := s;
  k_    := ThrowMessage["badYamlDictKey", MsgExpr @ k];
]

toYamlDictValue := Case[
  list_List := Scope[$inList = True; {"\n", Map[{"  - ", toYamlAtom @ #, "\n"}&, list]}];
  atom_     := {toYamlAtom @ atom, "\n"};
];

(*************************************************************************************************)

General::badYamlAtom = "Unsupported atom ``."

toYamlAtom = Case[
  None          := "";
  False         := "false";
  True          := "true";
  n_ ? NumberQ  := TextString[n];
  s_Str         := toYamlString @ s;
  d_DateObject  := toYamlDate @ d;
  o_            := ThrowMessage["badYamlAtom", MsgExpr @ o];
];

(*************************************************************************************************)

toYamlString[s_Str] := Which[
  StringStartsQ[s, "["] || StringContainsQ[s, "#" | ":" | "\""], "\"" <> StringReplace[s, "\"" -> "\\\""] <> "\"",
  StringContainsQ[s, "\n"], {"|\n", StringReplace[s, StartOfLine -> If[$inList, "   ", "  "]]},
  True, s
];

(*************************************************************************************************)

toYamlDate = Case[
  DateObject[{y_, m_, d_}, "Day"]                            := ymdStr[y, m, d];
  DateObject[{y_, m_, d_, h_, m2_, s_, ___}, "Instant", ___] := ymdStr[y, m, d] <> "T" <> hmsStr[h, m2, s];
  d_                                                         := ThrowMessage["badYamlAtom", MsgExpr @ d];
]

ymdStr[y_, m_, d_] := StrJoin[IntStr @ y, "-", IntStr2 @ m, "-", IntStr2 @ d];
hmsStr[h_, m_, s_] := StrJoin[IntStr2 @ h, ":", IntStr2 @ m, ":", IntStr2 @ Floor @ s];

(*************************************************************************************************)

PublicFunction[ImportYAMLDict]

(* this does not support nesting of dicts or lists *)
ImportYAMLDict[e_Str] := Scope @ CatchMessage[
  fromYamlDict @ e
]

(*************************************************************************************************)

fromYamlDict[str_] := Scope[
  split = StringSplit[str, (StartOfLine ~~ key:ASCIIWord ~~ ":") :> key];
  Association[fromYamlDictEntry @@@ Partition[split, 2]]
];

fromYamlDictEntry[k_, v_] := k -> fromYamlDictEntry @ StringTrim @ v;

fromYamlDictEntry[e_Str] := Which[
  StringStartsQ[e, "- "],            fromYamlList @ e,
  True,                              fromYamlAtom @ e
];

fromYamlAtom[e_Str] := Which[
  StringMatchQ[e, DigitCharacter..], FromDigits @ e,
  StringMatchQ[e, NumberString],     ToExpression @ e,
  StringLength[e] == 10 && StringMatchQ[e, YAMLDatePattern],     fromYamlDate @ e,
  StringLength[e] == 19 && StringMatchQ[e, YAMLDateTimePattern], fromYamlDateTime @ e,
  e === "true",            True,
  e === "false",           False,
  StringStartsQ[e, "|\n"], fromYamlMultilineStr @ e,
  True,                    fromYamlString @ e
];

(*************************************************************************************************)

fromYamlList[e_] := Scope[
  split = StringTrim @ StringSplit["  " <> e, (StartOfLine ~~ "  - ")];
  fromYamlAtom /@ split
];

(*************************************************************************************************)

fromYamlString[s_] := If[StringStartsEndsQ[s, "\"", "]"], StringTrimLeftRight[s, "\"", "\""], s];

fromYamlMultilineStr[e_] := StringDelete[StringReplace[StringTrimLeft[e, "|"], "\n  " -> "\n"], StartOfLine ~~ "\n"];

(*************************************************************************************************)

fromYamlDate[e_] := FromDateString[e, {"Year", "-", "Month", "-", "Day"}];

fromYamlDateTime[e_] := FromDateString[e, {"Year", "-", "Month", "-", "Day", "T", "Hour", ":", "Minute", ":", "Second"}];

