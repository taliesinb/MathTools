PublicFunction[ExportYAMLDict]

(* this does not support nesting of dicts or lists *)
ExportYAMLDict[e_Assoc] := Scope @ CatchMessage[
  $inList = False;
  STrim @ SJoin @ toYamlDict @ e
];

General::badYamlDict = "Value `` is not an association.";

ExportYAMLDict[e_] := (Message[ExportYAMLDict::badYamlDict, MsgExpr @ e]; $Failed);

(*************************************************************************************************)

toYamlDict[assoc_Assoc] := KVMap[
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
  SStartsQ[s, "["] || SContainsQ[s, "#" | ":" | "\""], "\"" <> SRep[s, "\"" -> "\\\""] <> "\"",
  SContainsQ[s, "\n"], {"|\n", SRep[s, StartOfLine -> If[$inList, "   ", "  "]]},
  True, s
];

(*************************************************************************************************)

toYamlDate = Case[
  DateObject[{y_, m_, d_}, "Day"]                            := ymdStr[y, m, d];
  DateObject[{y_, m_, d_, h_, m2_, s_, ___}, "Instant", ___] := ymdStr[y, m, d] <> "T" <> hmsStr[h, m2, s];
  d_                                                         := ThrowMessage["badYamlAtom", MsgExpr @ d];
]

ymdStr[y_, m_, d_] := SJoin[IntStr @ y, "-", IntStr2 @ m, "-", IntStr2 @ d];
hmsStr[h_, m_, s_] := SJoin[IntStr2 @ h, ":", IntStr2 @ m, ":", IntStr2 @ Floor @ s];

(*************************************************************************************************)

PublicFunction[ImportYAMLDict]

(* this does not support nesting of dicts or lists *)
ImportYAMLDict[e_Str] := Scope @ CatchMessage[
  fromYamlDict @ e
]

(*************************************************************************************************)

fromYamlDict[str_] := Scope[
  split = SSplit[str, (StartOfLine ~~ key:ASCIIWord ~~ ":") :> key];
  Assoc[fromYamlDictEntry @@@ Partition[split, 2]]
];

fromYamlDictEntry[k_, v_] := k -> fromYamlDictEntry @ STrim @ v;

fromYamlDictEntry[e_Str] := Which[
  SStartsQ[e, "- "],            fromYamlList @ e,
  True,                              fromYamlAtom @ e
];

fromYamlAtom[e_Str] := Which[
  SMatchQ[e, DigitCharacter..], FromDigits @ e,
  SMatchQ[e, NumberString],     ToExpression @ e,
  SLen[e] == 10 && SMatchQ[e, YAMLDatePattern],     fromYamlDate @ e,
  SLen[e] == 19 && SMatchQ[e, YAMLDateTimePattern], fromYamlDateTime @ e,
  e === "true",            True,
  e === "false",           False,
  SStartsQ[e, "|\n"], fromYamlMultilineStr @ e,
  True,                    fromYamlString @ e
];

(*************************************************************************************************)

fromYamlList[e_] := Scope[
  split = STrim @ SSplit["  " <> e, (StartOfLine ~~ "  - ")];
  fromYamlAtom /@ split
];

(*************************************************************************************************)

fromYamlString[s_] := If[StringStartsEndsQ[s, "\"", "]"], StringTrimLeftRight[s, "\"", "\""], s];

fromYamlMultilineStr[e_] := SDelete[SRep[StringTrimLeft[e, "|"], "\n  " -> "\n"], StartOfLine ~~ "\n"];

(*************************************************************************************************)

fromYamlDate[e_] := FromDateString[e, {"Year", "-", "Month", "-", "Day"}];

fromYamlDateTime[e_] := FromDateString[e, {"Year", "-", "Month", "-", "Day", "T", "Hour", ":", "Minute", ":", "Second"}];

