PublicVariable[$EnableCaching]

SetInitialValue[$EnableCaching, True];

(**************************************************************************************************)

PrivateCacheFunction[CachedInto]

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
    printCacheHit[cacheSymbol, cacheKey];
    Return @ cacheResult
  ];

  cacheResult = resultExpr;

  If[$EnableCaching && !FailureQ[cacheResult],
    printCacheInsert[cacheSymbol, cacheKey, cacheResult];
    AssociateTo[cacheSymbol, cacheKey -> cacheResult]
  ];

  cacheResult
];

CachedInto[_, _, resultExpr_] := resultExpr;

(**************************************************************************************************)

PrivateCacheFunction[CachedIntoMX]

SetUsage @ "
CachedIntoMX[sym$, 'dir$', key$, body$] is similar to %CachedIntoTo.
CachedIntoMX[None, 'dir$', key$, body$] dispenses with the cache symbol.
* first, sym$[key$] is returned if it exists.
* otherwise, if 'dir$/key.mx' exists, it is loaded into sym$ and returned.
* otherwise, the result is computed and saved to sym$[key$] and to the MX path.
* if key$ is a list, subdirectories will be used.
* $Failed results are not cached.
* a %Return within the body will bypass cache insertion.
* Verbose mode will print cache hits and insertions.
"

SetHoldAll[CachedIntoMX];

CachedIntoMX::cachedMXFileCorrupt = "Local MX cache file `` is corrupt, recomputing.";

CachedIntoMX[cacheSymbol_, dir_, keyExpr_, resultExpr_] /; TrueQ[$EnableCaching] := Module[
  {cacheKey = keyExpr, cachePath, result, mxPath, mxExists},

  If[$EnableCaching && cacheSymbol =!= None && !MissingQ[result = cacheSymbol[cacheKey]],
    printCacheHit[cacheSymbol, cacheKey];
    Return @ result
  ];

  If[$EnableCaching && (mxExists = FileExistsQ[mxPath = toMXPath[dir, keyExpr]]),
    printFileHit[mxPath, cacheKey];
    result = Quiet @ Check[ImportMX @ mxPath, $Failed];
    If[FailureQ[result],
      Message[CachedIntoMX::cachedMXFileCorrupt, MsgPath @ mxPath];
      result = resultExpr;
    ];
  ,
    result = resultExpr;
  ];

  If[$EnableCaching && !FailureQ[result],
    If[!mxExists,
      printFileInsert[mxPath, cacheKey, result];
      ExportMX[mxPath, result];
    ];
    printCacheInsert[cacheSymbol, cacheKey, result];
    AssociateTo[cacheSymbol, cacheKey -> result]
  ];

  result
];

CachedIntoMX[_, _, _, resultExpr_] := resultExpr;

(**************************************************************************************************)

(* we assume there won't be millions of these so we cache the path lookup and avoid
the cost of EnsureDirectory, etc.  *)
toMXPath[dir_, key_] := toMXPath[dir, key] = Module[{path},
  path = If[ListQ[key], FileNameJoin[TextString /@ key], TextString @ key];
  path = LocalPath["Data", dir, path <> ".mx"];
  EnsureDirectory @ FileNameDrop @ path;
  path
];

(**************************************************************************************************)

SetHoldFirst[printCacheHit, printCacheInsert];

printCacheHit[sym_Symbol, key_] /; TrueQ[$verbose] :=
  VPrint["Cache hit in ", HoldSymbolName @ sym, " for ", MsgExpr @ key];

printCacheInsert[sym_Symbol, key_, res_] /; TrueQ[$verbose] :=
  VPrint["Inserting into ", HoldSymbolName @ sym, " value ", MsgExpr @ res];

printFileHit[path_Str, key_] /; TrueQ[$verbose] :=
  VPrint["Cache hit at ", MsgPath @ path, " for ", MsgExpr @ key, ", filesize ", FileByteCount @ path];

printFileInsert[path_Str, key_, res_] /; TrueQ[$verbose] :=
  VPrint["Writing to ", MsgPath @ path, " value of size ", ByteCount @ res];

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


