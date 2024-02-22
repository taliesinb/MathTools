PublicIOFunction[MacOSTextSubstitutions]

CacheVariable[$MacOSCache]

MacOSTextSubstitutions[] := Scope @ CachedInto[$MacOSCache, "MacOSTextSubstitutions",
  globalDefulats = MacOSGlobalDefaults[];
  Assoc @ SCases[globalDefulats, $keyReplacePattern]
];

parsePlistUTF16[s_Str] := SRep[s,
  ("\\U" ~~ hex1:Repeated[HexadecimalCharacter, 4] ~~ "\\U" ~~ hex2:Repeated[HexadecimalCharacter, 4]) :>
    Join[hex1, hex2]
];

$keyReplacePattern = SExpr[
  XMLSpan["key", "replace"], Whitespace, XMLSpan["string", from:LineFragment], Whitespace,
  XMLSpan["key", "with"],    Whitespace, XMLSpan["string", to:LineFragment]
] :> Rule[from, to];

(**************************************************************************************************)

PublicIOFunction[MacOSGlobalDefaults]

MacOSGlobalDefaults[] := CachedInto[$MacOSCache, "NSGlobalDomainPListContents", nsgGlobalDomainPListContents[]];

nsgGlobalDomainPListContents[] := Scope @ CachedInto[$MacOSCache, "NSGlobalDomainPListContents",
  outFile = MakeTemporaryFile["PList", "NSGlobalDomain.plist"];
  If[FileExistsQ[outFile], TrashFile[outFile]];
  If[!RunUTF8["defaults export NSGlobalDomain ", outFile], ReturnFailed[]];
  If[!FileExistsQ[outFile], ReturnFailed[]];
  If[!RunUTF8["plutil -convert xml1 ", outFile], ReturnFailed[]];
  ImportUTF8 @ outFile
];

