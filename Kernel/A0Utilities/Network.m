PublicFunction[CachedURLFetch]

CacheSymbol[$URLFetchCache]

CachedURLFetch[url_] := MaybeCacheTo[$URLFetchCache, url, WithInternet @ URLFetch[url, $urlFetchOptions]];

(**************************************************************************************************)

PublicFunction[SafeURLDownload]

General::baddl = "Failed to download `` to ``."

Options[SafeURLDownload] = {
  OverwriteTarget -> False
}

SafeURLDownload[url_String, path_String, OptionsPattern[]] := Scope[
  UnpackOptions[overwriteTarget];
  path //= NormalizePath;
  If[FileExistsQ[path] && !overwriteTarget,
    VPrint["Local file ", MsgPath @ url, " alread exists, returning."];
    Return @ path
  ];
  VPrint["Downloading ", MsgPath @ url, " to ", MsgPath @ path];
  whenWet[
    tmpPath = TemporaryPath["Downloads", FileNameTake[path] <> "." <> RandomString[6]];
    result = WithInternet @ Check[URLDownload[url, tmpPath, $urlDownloadOptions], $Failed];
    If[!MatchQ[result, File[_]] || !FileExistsQ[tmpPath],
      ReturnFailed["baddl", MsgPath @ url, MsgPath @ path]];
    MoveFile[tmpPath, path, OverwriteTarget -> overwriteTarget];
  ];
  path
];

(**************************************************************************************************)

$userAgent = "Mozilla/5.0 (X11; Linux x86_64; rv:27.0) Gecko/20100101 Firefox/27.0";

$urlFetchOptions = Sequence[
  "ConnectTimeout" -> 5,
  "ReadTimeout" -> 600,
  "UserAgent" -> $userAgent
];

$urlDownloadOptions = Sequence[
  TimeConstraint -> <|"Connecting" -> 10, "Reading" -> 600|>,
  "UserAgent" -> $userAgent
];
