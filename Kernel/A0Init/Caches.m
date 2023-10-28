PublicDebuggingFunction[CacheSymbolNames]

CacheSymbolNames[] := Names[QuiverGeometryLoader`$CacheContext <> "*"];

(**************************************************************************************************)

PublicDebuggingFunction[ClearCacheSymbols]

ClearCacheSymbols[] := Scope[
  cacheNames = CacheSymbolNames[];
  Clear @@ cacheNames;
  ToExpression[#, InputForm, SetOperator[UAssociation[]]]& /@ cacheNames;
  cacheNames
];

(**************************************************************************************************)

PublicDebuggingFunction[CacheSymbolCounts]

CacheSymbolCounts[] := Scope[
  names = CacheSymbolNames[];
  lengths = Length /@ ToExpression[names, InputForm];
  AssociationThread[names, lengths]
];