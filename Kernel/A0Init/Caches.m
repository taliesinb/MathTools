PublicVariable[$EnableCaching]

SetInitialValue[$EnableCaching, True];

(**************************************************************************************************)

PrivateSpecialFunction[CachedInto]

SetUsage @ "
CachedInto[sym$, key$, body$] is similar to %CacheTo.
* it will not cache $Failed.
* it only acts if $EnableCaching is True.
* a %Return within the body will bypass cache insertion.
* Verbose mode will print cache hits and insertions.
"

SetHoldAll[CachedInto];

CachedInto[cacheSymbol_, keyExpr_, resultExpr_] /; TrueQ[$EnableCaching] := Module[
  {cacheKey, cacheResult},

  If[$EnableCaching && !MissingQ[cacheResult = cacheSymbol[cacheKey = keyExpr]],
    printCacheHit[cacheSymbol, cacheKey, cacheResult];
    Return @ cacheResult
  ];

  cacheResult = resultExpr;

  If[$EnableCaching && !FailureQ[cacheResult],
    printCacheInsert[cacheSymbol, cacheKey, cacheResult];
    AssociateTo[cacheSymbol, cacheKey -> cacheResult]
  ];

  cacheResult
];

CachedInto[_, _, val_] := val;

(**************************************************************************************************)

SetHoldAll[printCacheHit, printCacheInsert];
printCacheHit[sym_, key_, res_] /; TrueQ[$verbose] :=
  VPrint["Cache hit in ", HoldSymbolName @ sym, " for ", MsgExpr @ key];

printCacheInsert[sym_, key_, res_] /; TrueQ[$verbose] :=
  VPrint["Inserting into ", HoldSymbolName @ sym, " value ", MsgExpr @ res];

(**************************************************************************************************)

PublicDebuggingFunction[CacheSymbolNames]

CacheSymbolNames[] := Names[QuiverGeometryLoader`$CacheContext <> "*"];

(**************************************************************************************************)

PublicDebuggingFunction[ClearCacheSymbols]

ClearCacheSymbols[] := Scope[
  cacheNames = CacheSymbolNames[];
  Clear @@ cacheNames;
  ToExpression[#, InputForm, SetOperator[UAssoc[]]]& /@ cacheNames;
  cacheNames
];

(**************************************************************************************************)

PublicDebuggingFunction[CacheSymbolCounts]

CacheSymbolCounts[] := Scope[
  names = CacheSymbolNames[];
  lengths = Len /@ ToExpression[names, InputForm];
  AssociationThread[names, lengths]
];


