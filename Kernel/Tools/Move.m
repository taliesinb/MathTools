PublicFunction[MoveFile]

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
      Quiet @ Check[If[isDir, RenameDirectory, RenameFile][source, target]; True, False]
    ,
      RunTool["mv", source, target]
    ]
  ];
  If[!success, ReturnFailed["badmove", MsgPath @ source, MsgPath @ target]];
];

MoveFile[_, _] := $Failed;

(**************************************************************************************************)

PublicFunction[TrashFile]

TrashFile::nofile = "File `` does not exist.";

TrashFile[path_Str] := Scope[
  If[!FileExistsQ[path], ReturnFailed["nofile", MsgPath @ path]];
  trashName = FileNameTake[path] <> "." <> RandomString[6];
  trashPath = TemporaryPath["Trash", trashName];
  MoveFile[path, trashPath];
  trashPath
];