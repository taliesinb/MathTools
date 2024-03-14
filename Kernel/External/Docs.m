PublicVariable[$PDFPath, $DocsPath]

SetInitialValue[$DocsPath, With[{path = NormalizePath @ "~/Dropbox/doc/"}, If[FileExistsQ[path], path, DataPath["Docs"]]]];
SetInitialValue[$PDFPath, With[{path = NormalizePath @ "~/Dropbox/doc/paper"}, If[FileExistsQ[path], path, DataPath["Docs"]]]];

(**************************************************************************************************)

PrivateVariable[$DocsIndex]

SetDelayedInitialValue[$DocsIndex, RebuildDocsIndex[]];

(**************************************************************************************************)

PublicFunction[FindDoc]

SetListable[FindDoc];

FindDoc[title_Str] := $DocsIndex[title];
FindDoc[None] := None;

(**************************************************************************************************)

PublicIOFunction[RebuildDocsIndex]

RebuildDocsIndex::failed = "Failed to build docs index for ``.";

RebuildDocsIndex[] := Module[{files, ind},
  files = FileNames["*.pdf", $DocsPath, Infinity];
  Print["Building docs index against ", Len @ files, " files."];
  ind = CreateMetaIndex[files, {Identity, ToLowerCase, StringWordSequence}, FileBaseName];
  If[Head[ind] =!= MetaIndexObject,
    $DocsIndex = $Failed&;
    ReturnFailed["failed", MsgPath @ $DocsPath]];
  $DocsIndex = ind
];

(**************************************************************************************************)

PublicFunction[StringWordSequence]

StringWordSequence[str_Str] := ToLowerCase @ StringCases[RemoveDiacritics @ str, RomanCharacter.. | DQuote];

