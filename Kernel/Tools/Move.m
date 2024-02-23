PublicIOFunction[MoveFile]

SetUsage @ "
MoveFile['src$', 'tgt$'] combines %RenameFile and %RenameDirectory.
* The option OverwriteTarget can be given.
"

MoveFile::badmove = "Could not move `` to ``."
MoveFile::badsource = "Source `` does not exist."
MoveFile::badtargetdir = "Parent of target `` does not exist."
MoveFile::targetex = "Target `` already exists."

Options[MoveFile] = {
  OverwriteTarget -> False
};

MoveFile[source_Str, target_Str, OptionsPattern[]] := Scope[
  source //= NormalizePath;
  target //= NormalizePath;
  UnpackOptions[overwriteTarget];
  VPrint["Moving ", MsgPath @ source, " to ", MsgPath @ target];
  If[!FileExistsQ[source], ReturnFailed["badsource", MsgPath @ source]];
  isDir = DirectoryQ[source];
  If[FileExistsQ[target],
    If[overwriteTarget,
      VPrint["Target exists, trashing."];
      TrashFile[target]
    ,
      ReturnFailed["targetex", MsgPath @ target]
    ]
  ];
  If[!DirectoryQ[FileNameDrop[target]], ReturnFailed["badtargetdir", MsgPath @ target]];
  If[$dryRun,
    success = True
  ,
    success = If[ASCIIQ[source] && ASCIIQ[target],
      fn = If[isDir, RenameDirectory, RenameFile];
      VPrint["Calling ", fn];
      Quiet @ Check[fn[source, target]; True, False]
    ,
      RunTool["mv", source, target]
    ]
  ];
  If[!success, ReturnFailed["badmove", MsgPath @ source, MsgPath @ target]];
];

MoveFile[_, _] := $Failed;

(**************************************************************************************************)

PublicIOFunction[TrashFile]

SetUsage @ "
TrashFile[path$] deletes a file or directory by moving it into the system's trash directory.
* a random suffix is added to esnure existing trash is not overwritten.
"

TrashFile::nofile = "File `` does not exist.";

SetCached[$trashPath, If[$MacOSQ, ExpandFileName["~/.Trash"], EnsureDirectory @ FileTemporaryPath["Trash"]]];

TrashFile[path_Str] := Scope[
  path //= NormalizePath;
  If[!FileExistsQ[path], ReturnFailed["nofile", MsgPath @ path]];
  trashName = FileNameTake[path] <> "." <> RandomString[6];
  trashPath = PathJoin[$trashPath, trashName];
  VPrint["Trashing ", MsgPath @ path, " to ", MsgPath @ trashPath];

  (* try avoid recursing, because MoveFile and TrashFile have a mutual recursion situation *)
  If[ASCIIQ[path] && ASCIIQ[trashPath],
    If[DirectoryQ[path], RenameDirectory, RenameFile][path, trashPath],
    MoveFile[path, trashPath, OverwriteTarget -> True];
  ];

  trashPath
];