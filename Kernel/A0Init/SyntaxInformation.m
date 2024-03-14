PrivateSpecialFunction[DefineFunctionAutocomplete]

CacheVariable[$AutocompleteCache]

(* in case the autocomplete possibilities are expensive *)
SetHoldRest[DefineFunctionAutocomplete];

DefineFunctionAutocomplete[function_Symbol, spec_] := Scope @ IfSynaxInfo[
  functionName = SymbolName[function];
  spec = spec /. {None -> 0, File -> 2, Directory -> 8};
  CacheTo[$AutocompleteCache, {functionName, spec},
    Construct[FE`Evaluate, FEPrivate`AddSpecialArgCompletion[functionName -> spec]];
  ];
];

(* declare multiple identical specs at once, presumably cheaper *)
DefineFunctionAutocomplete[functions:{__Symbol}, spec_] := Scope @ IfSynaxInfo[
  functionNames = SymbolName /@ functions;
  spec = spec /. {None -> 0, File -> 2, Directory -> 8};
  CacheTo[$AutocompleteCache, {functionNames, spec},
    Construct[FE`Evaluate, FEPrivate`AddSpecialArgCompletion[# -> spec]& /@ functionNames];
  ];
];

_DefineFunctionAutocomplete := BadArguments[];

(**************************************************************************************************)

PrivateSpecialFunction[DefineArgumentsPattern]

DefineArgumentsPattern[function_Symbol, argPatterns_List] := Scope @ IfSyntaxInfo[
  info = {"ArgumentsPattern" -> argPatterns};
  If[ContainsQ[argPatterns, Verbatim[OptionsPattern[]]],
    AppTo[info, "OptionNames" -> Map[toOptionName, OptionKeys @ function]]];
  DefineSyntaxInformation[function, info]
];

_DefineArgumentsPattern := BadArguments[];

toOptionName[sym_Symbol] := SymbolName[sym];
toOptionName[str_Str] := str;
toOptionName[_] := Nothing;

(**************************************************************************************************)

PrivateSpecialFunction[DefineSyntaxInformation]

CacheVariable[$SyntaxInformationCache]

DefineSyntaxInformation[function_Symbol, rules:{__Rule}] := Scope @ IfSyntaxInfo[
  CacheTo[$SyntaxInformationCache, {function, rules},
    SyntaxInformation[function] = rules;
  ];
];

_DefineSyntaxInformation := BadArguments[];
