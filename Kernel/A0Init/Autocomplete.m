PrivateFunction[declareFunctionAutocomplete, declareSyntaxInfo]

If[$Notebooks,

declareFunctionAutocomplete[function_Symbol, spec_] := With[
  {functionName = SymbolName[function]},
    FE`Evaluate[FEPrivate`AddSpecialArgCompletion[functionName -> spec]]
  ];
declareFunctionAutocomplete[___] := Panic["BadArgs"];

toOptionName[sym_Symbol] := SymbolName[sym];
toOptionName[str_String] := str;
toOptionName[_] := Nothing;

declareSyntaxInfo[function_Symbol, argPatterns_List] := Scope[
  info = {"ArgumentsPattern" -> argPatterns};
  If[ContainsQ[argPatterns, Verbatim[OptionsPattern[]]],
    AppendTo[info, "OptionNames" -> Map[toOptionName, Keys @ Options @ function]]];
  SyntaxInformation[function] = info;
];

];
