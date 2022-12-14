PublicFunction[MoveFile]

MoveFile::badmove = "Could not move `` to ``."
MoveFile::badsource = "Source `` does not exist."
MoveFile::badtargetdir = "Parent of target `` does not exist."
MoveFile::targetex = "Target `` already exists."

MoveFile[source_String, target_String] := Scope[
  source //= NormalizePath;
  target //= NormalizePath;
  VPrint["Moving ", MsgPath @ source, " to ", MsgPath @ target];
  If[!FileExistsQ[source], ReturnFailed["badsource", MsgPath @ source]];
  isDir = DirectoryQ[source];
  If[FileExistsQ[target], ReturnFailed["targetex", MsgPath @ target]];
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
