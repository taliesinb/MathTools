PublicVariable[$PDFPath, $DocsPath]

SetInitialValue[$DocsPath, With[{path = NormalizePath @ "~/Dropbox/doc/"}, If[FileExistsQ[path], path, DataPath["Docs"]]]];
SetInitialValue[$PDFPath, With[{path = NormalizePath @ "~/Dropbox/doc/paper"}, If[FileExistsQ[path], path, DataPath["Docs"]]]];

(**************************************************************************************************)

PrivateVariable[$DocsIndex]

SetDelayedInitialValue[$DocsIndex, RebuildDocsIndex[]];

(**************************************************************************************************)

PublicFunction[FindDoc]

SetListable[FindDoc];

FindDoc[title_Str, th_:0.2] := Scope[match = $DocsIndex[title]; If[LikelyTitleMatchQ[title, match, th], match, None]];
FindDoc[None, ___] := None;

(**************************************************************************************************)

PublicFunction[LikelyTitleMatchQ]

LikelyTitleMatchQ[orig_, path_, th_] := Scope[
  If[StringContainsQ[orig, "/"], orig //= pathToTitle];
  If[StringContainsQ[path, "/"], path //= pathToTitle];
  cutoff = Max[SLen @ {orig, path}] * th;
  orig //= ToLowerCase;
  path //= ToLowerCase;
  orig1 = StringDropUntil[": "] @ orig;
  orig2 = StringDropUntil[" - "] @ orig;
  path1 = StringDropUntil[" - "] @ path;
  equal = Apply[Or] @ Flatten @ Outer[softEqualQ[co], {orig, orig1, orig2}, {path, path1}];
  equal
];

softEqualQ[co_][a_, b_] := EditDistance[a, b] < Max[SLen @ a, SLen @ b] * cutoff;

(**************************************************************************************************)

pathToTitle[e_] := AccentMarksToLetters @ FileBaseName @ e;

(**************************************************************************************************)

PublicIOFunction[RebuildDocsIndex]

RebuildDocsIndex::failed = "Failed to build docs index for ``.";

RebuildDocsIndex[] := Module[{files, ind},
  files = FileNames["*.pdf", $DocsPath, Infinity];
  Print["Building docs index against ", Len @ files, " files."];
  ind = CreateMetaIndex[files, {Identity, ToLowerCase, StringWordSequence}, pathToTitle];
  If[Head[ind] =!= MetaIndexObject,
    $DocsIndex = $Failed&;
    ReturnFailed["failed", MsgPath @ $DocsPath]];
  $DocsIndex = ind
];

(**************************************************************************************************)

PublicFunction[StringWordSequence]

StringWordSequence[str_Str] := ToLowerCase @ StringCases[RemoveDiacritics @ str, RomanCharacter.. | DQuote];

