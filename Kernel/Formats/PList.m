PublicIOFunction[ImportPList]

ImportPList[path_String] := Scope[
  If[!FileExistsQ[path], ReturnFailed[]];
  baseName = FileNameTake @ path;
  hashStr = IntStr @ FileHash[path, "MD5"];
  tmpFile = MakeTemporaryFile["PList", hashStr <> ".plist"];
  If[FileExistsQ[tmpFile], Return @ ImportJSON @ tmpFile];
  CopyFile[path, tmpFile, OverwriteTarget -> True];
  VPrint["Converting ", MsgPath @ tmpFile, " in place."];
  If[!RunTool["plutil", "-convert", "json", tmpFile], ReturnFailed[]];
  ImportJSON @ tmpFile
];
