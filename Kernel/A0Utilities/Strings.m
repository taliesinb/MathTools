PackageExport["LowerCaseFirst"]
PackageExport["UpperCaseFirst"]

LowerCaseFirst[str_String] := StringJoin[ToLowerCase @ StringTake[str, 1], StringDrop[str, 1]];
UpperCaseFirst[str_String] := StringJoin[ToUpperCase @ StringTake[str, 1], StringDrop[str, 1]];

(**************************************************************************************************)

PackageScope["commaString"]

qs[s_String] := "\"" <> s <> "\"";
qs[e_] := e;
commaString[list_List] := TextString[Row[Map[qs, list], ", "]];

(**************************************************************************************************)

PackageExport["StringReplaceRepeated"]

StringReplaceRepeated[str_String, rules_] := FixedPoint[StringReplace[rules], str];
StringReplaceRepeated[rules_][str_] := StringReplaceRepeated[str, rules];

(**************************************************************************************************)

PackageExport["ExportUTF8"]

ExportUTF8[path_, string_] :=
  Export[path, string, "Text", CharacterEncoding -> "UTF-8"];

(**************************************************************************************************)

PackageExport["ImportUTF8"]

ImportUTF8[path_] :=
  Import[path, "Text", CharacterEncoding -> "UTF8"];

(**************************************************************************************************)

PackageExport["StringFindDelimitedPosition"]

StringFindDelimitedPosition[str_, {start_, mid_, stop_}] := Scope[
  pos = First[StringPosition[str, start ~~ mid ~~ stop, 1], None];
  If[!ListQ[pos], ReturnFailed[]];
  lens = First @ StringCases[
    StringTake[str, pos],
    a:start ~~ mid ~~ z:stop :> {+StringLength[a], -StringLength[z]},
    1
  ];
  pos + lens
]

(**************************************************************************************************)

PackageExport["FirstStringCase"]

SetHoldRest[FirstStringCase];

FirstStringCase[string_, pattern_, else_:None] :=
  First[
    StringCases[string, pattern, 1],
    else
  ]

(**************************************************************************************************)

PackageExport["CommonStringPrefix"]
PackageExport["CommonStringPrefixLength"]

CommonStringPrefix[{}] := None;
CommonStringPrefix[strings_ ? StringVectorQ] :=
  StringTake[First @ strings, CommonStringPrefixLength @ strings];

CommonStringPrefixLength[strings_ ? StringVectorQ] :=
  CommonPrefixLength @ Characters @ strings;

PackageExport["CommonStringSuffix"]
PackageExport["CommonStringSuffixLength"]

CommonStringSuffix[{}] := None;
CommonStringSuffix[strings_ ? StringVectorQ] :=
  StringTake[First @ strings, Minus @ CommonStringSuffixLength @ strings];

CommonStringSuffixLength[strings_ ? StringVectorQ] :=
  CommonSuffixLength @ Characters @ strings;