PrivateSpecialFunction[declareFunctionAutocomplete, declareSyntaxInfo]

CacheVariable[$AutocompleteCache, $SyntaxInformationCache]

(* in case the autocomplete possibilities are expensive *)
SetHoldRest[declareFunctionAutocomplete];

declareFunctionAutocomplete[function_Symbol, spec_] := Scope @ IfSynaxInfo[
  functionName = SymbolName[function];
  spec = spec /. {None -> 0, File -> 2, Directory -> 8};
  CacheTo[$AutocompleteCache, {functionName, spec},
    Construct[FE`Evaluate, FEPrivate`AddSpecialArgCompletion[functionName -> spec]];
  ];
];

(* declare multiple identical specs at once, presumably cheaper *)
declareFunctionAutocomplete[functions:{__Symbol}, spec_] := Scope @ IfSynaxInfo[
  functionNames = SymbolName /@ functions;
  spec = spec /. {None -> 0, File -> 2, Directory -> 8};
  CacheTo[$AutocompleteCache, {functionNames, spec},
    Construct[FE`Evaluate, FEPrivate`AddSpecialArgCompletion[# -> spec]& /@ functionNames];
  ];
];

_declareFunctionAutocomplete := BadArguments[];

declareSyntaxInfo[function_Symbol, argPatterns_List] := Scope @ IfSyntaxInfo[
  info = {"ArgumentsPattern" -> argPatterns};
  If[ContainsQ[argPatterns, Verbatim[OptionsPattern[]]],
    AppTo[info, "OptionNames" -> Map[toOptionName, OptionKeys @ function]]];
  CacheTo[$SyntaxInformationCache, {function, info},
    SyntaxInformation[function] = info;
  ];
];

toOptionName[sym_Symbol] := SymbolName[sym];
toOptionName[str_Str] := str;
toOptionName[_] := Nothing;
