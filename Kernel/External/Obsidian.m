PublicVariable[$DefaultObsidianVault]

SetInitialValue[$DefaultObsidianVault, NormalizePath["~/Obsidian"]];

(*************************************************************************************************)

PublicIOFunction[CreateObsidianNote]

PublicOption[ObsidianVault, NoteFolder, FrontMatter, CreationTime, ModificationTime]

Options[CreateObsidianNote] = {
  ObsidianVault -> Automatic,
  ReplaceExisting -> False,
  DuplicateTarget -> False,
  DryRun -> False,
  NoteFolder -> None,
  FrontMatter -> None,
  CreationTime -> None,
  ModificationTime -> None
};

General::noteExists = "Will not create note with title \"``\" because one already exists, and DuplicateTarget -> False.";
General::duplicateNoteExists = "Will not create note with title \"``\" because a duplicate already exists.";
General::invalidNoteTitleChars = "Invalid note title \"``\"."
General::badNoteFrontMatter = "Note with title `` has invalid front matter ``.";

CreateObsidianNote[title_Str, contents_Str, OptionsPattern[]] := Scope[
  UnpackOptions[duplicateTarget, replaceExisting,
    obsidianVault, noteFolder,
    frontMatter, creationTime, modificationTime, $dryRun];
  SetAutomatic[obsidianVault, $DefaultObsidianVault];
  title //= sanitizeTitle;
  If[StringContainsQ[title, $obsidianTitleCharacters], ReturnFailed["invalidNoteTitleChars", title]];
  notePath = toNotePath[title, noteFolder, obsidianVault];
  If[FileExistsQ[notePath],
    If[duplicateTarget,
      notePath = StringInsert[notePath, " (copy)", -4];
      If[FileExistsQ[notePath] && !replaceExisting, ReturnFailed["duplicateNoteExists", title]]
    ,
      If[!replaceExisting, ReturnFailed["noteExists", title]];
    ];
  ];
  If[frontMatter =!= None && frontMatter =!= Assoc[],
    If[StringVectorQ[frontMatter["tags"]], frontMatter["tags"] //= StringRiffle[#, " "]&];
    frontMatterString = ExportYAMLDict @ frontMatter;
    If[!StringQ[frontMatterString], ReturnFailed["badNoteFrontMatter", MsgExpr @ title, MsgExpr @ frontMatter]];
    contents = StringJoin["---\n", frontMatterString, "\n---\n", contents];
  ];
  whenWet[
    If[!FileExistsQ[noteParentDir = FileNameDrop[notePath]], CreateDirectory[noteParentDir]];
    ExportUTF8[notePath, contents];
    SetFileTime[notePath, {creationTime, modificationTime}];
  ];
  notePath
];

_CreateObsidianNote := BadArguments[];

toNotePath[title_, folder_, vault_] := FileNameJoin[{obsidianVault, ReplaceNone[folder, Nothing], title <> ".md"}];

$obsidianTitleCharacters = {"\\", "/"};
