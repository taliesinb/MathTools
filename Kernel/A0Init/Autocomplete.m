PrivateSpecialFunction[declareFunctionAutocomplete, declareSyntaxInfo]

If[$Notebooks && QuiverGeometryLoader`$AttachSyntaxInformation === True,

declareFunctionAutocomplete[function_Symbol, spec_] := With[
  {functionName = SymbolName[function], spec2 = spec /. {None -> 0, File -> 2, Directory -> 8}},
    FE`Evaluate[FEPrivate`AddSpecialArgCompletion[functionName -> spec2]]
  ];
declareFunctionAutocomplete[___] := Panic["BadArgs"];

toOptionName[sym_Symbol] := SymbolName[sym];
toOptionName[str_String] := str;
toOptionName[_] := Nothing;

declareSyntaxInfo[function_Symbol, argPatterns_List] := Scope[
  info = {"ArgumentsPattern" -> argPatterns};
  If[ContainsQ[argPatterns, Verbatim[OptionsPattern[]]],
    AppendTo[info, "OptionNames" -> Map[toOptionName, OptionKeys @ function]]];
  SyntaxInformation[function] = info;
];

];
