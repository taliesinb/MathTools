PublicIOFunction[GitClone]

PublicOption[ShallowCheckout]

Options[GitClone] = JoinOptions[
  ShallowCheckout -> False,
  $genericToolOpts
];

GitClone[path_Str, remote_Str, opts:OptionsPattern[]] := Scope[
  UnpackOptions[shallowCheckout];
  path = EnsureDirectoryShallow @ path;
  parent = ParentDirectory @ path;
  RunTool[
    "git", "clone",
    If[shallowCheckout, "--depth" -> 1, Sequence @@ {}],
    remote, FileNameTake @ path,
    WorkingDirectory -> parent, FilterOptions @ opts
  ]
]

(**************************************************************************************************)

PublicIOFunction[DownloadGithubRepo]

PublicOption[GitBranch]

Options[DownloadGithubRepo] = JoinOptions[
  GitBranch -> "main",
  $genericToolOpts
];

DownloadGithubRepo::baddownload = "Could not download repo from `` to ``.";
DownloadGithubRepo::badzip = "Downloaded zip `` appears to be corrupt.";
DownloadGithubRepo::badcontents = "Zip contents was not a single sub-directory as expected."

DownloadGithubRepo[userName_Str, repo_Str, OptionsPattern[]] := Scope[
  UnpackOptions[gitBranch];
  url = $githubZipURLTemplate[userName, repo, gitBranch];
  localFile = $githubZipLocalFileTemplate[userName, repo, gitBranch];
  localDir = ReplaceFileExtension[localFile, None];
  If[DirectoryQ[localDir], Goto["SkipDownload"]];
  VPrint["Downloading ", MsgPath @ url, " to ", MsgPath @ localFile];
  WithInternet[res = URLDownload[url, localFile, All, TimeConstraint -> 20]];
  If[res["StatusCode"] =!= 200 || !MatchQ[res["File"], _File],
    Quiet @ DeleteFile[localFile];
    ReturnFailed["baddownload", url, MsgPath @ localFile]
  ];
  If[FailureQ[ExtractArchive[localFile, localDir]],
    Quiet @ DeleteDirector[localDir];
    ReturnFailed["badzip", MsgPath @ localFile];
  ];
  Label["SkipDownload"];
  subDir = F[
    FileNames["*", localDir],
    Quiet @ DeleteDirector[localDir];
    ReturnFailed["badcontents"]
  ];
  subDir
]

$githubZipLocalFileTemplate = Fn[DataPath["Github", #1 <> "_" <> #2 <> "_" <> #3 <> ".zip"]];
$githubZipURLTemplate = StringFunction @ "https://github.com/#1/#2/archive/refs/heads/#3.zip"

