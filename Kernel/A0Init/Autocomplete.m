PrivateSpecialFunction[declareFunctionAutocomplete, declareSyntaxInfo]

CacheSymbol[$AutocompleteCache, $SyntaxInformationCache]

$syntaxInfoEnabled := TrueQ[$Notebooks && QuiverGeometryLoader`$AttachSyntaxInformation];

(* in case the autocomplete possibilities are expensive *)
SetHoldRest[declareFunctionAutocomplete];

declareFunctionAutocomplete[function_Symbol, spec_] /; $syntaxInfoEnabled := Scope[
  functionName = SymbolName[function];
  spec = spec /. {None -> 0, File -> 2, Directory -> 8};
  CacheTo[$AutocompleteCache, {functionName, spec},
    Construct[FE`Evaluate, FEPrivate`AddSpecialArgCompletion[functionName -> spec]];
  ];
];

declareSyntaxInfo[function_Symbol, argPatterns_List] /; $syntaxInfoEnabled := Scope[
  info = {"ArgumentsPattern" -> argPatterns};
  If[ContainsQ[argPatterns, Verbatim[OptionsPattern[]]],
    AppendTo[info, "OptionNames" -> Map[toOptionName, OptionKeys @ function]]];
  CacheTo[$SyntaxInformationCache, {function, info},
    SyntaxInformation[function] = info;
  ];
];

toOptionName[sym_Symbol] := SymbolName[sym];
toOptionName[str_String] := str;
toOptionName[_] := Nothing;
