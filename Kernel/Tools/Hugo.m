PublicFunction[HugoNewSite]

PublicOption[HugoTheme]

Options[HugoNewSite] = JoinOptions[
  SiteName -> "New Site",
  HugoTheme -> "taliesinb/hugo-math/master",
  $genericToolOpts
];

HugoNewSite::exists = "A Hugo site already exists at ``.";
HugoNewSite::noparent = "Cannot create a Hugo site at `` because parent directory does not exist.";
HugoNewSite::invalidtheme = "HugoTheme -> `` should be a string of the form \"username/repo\".";

HugoNewSite[dir_Str, opts:OptionsPattern[]] := Scope[

  UnpackOptions[siteName, hugoTheme, $verbose, $dryRun];
  SetAutomatic[$verbose, $dryRun];

  dir //= NormalizePath;
  configFile = PathJoin[dir, "config.toml"];
  If[FileExistsQ[dir],
    If[!DirectoryQ[dir], ReturnFailed[]];
    If[DirectoryQ[dir] && FileExistsQ[configFile], ReturnFailed["exists", MsgPath @ dir]];
  ];
  parent = FileNameDrop[dir];
  If[!DirectoryQ[parent], ReturnFailed["noparent", MsgPath @ dir]];

  If[!StringQ[hugoTheme] || StringFreeQ[hugoTheme, "/"], ReturnFailed["invalidtheme", hugoTheme]];
  {themeProvider, hugoTheme, themeBranch} = PadRight[StringSplit[hugoTheme, "/", 3], 3, None];
  SetNone[themeBranch, "master"];
  themeDir = DownloadGithubRepo[themeProvider, hugoTheme, GitBranch -> themeBranch, Verbose -> $verbose, DryRun -> $dryRun];
  If[FailureQ[themeDir], ReturnFailed[]];

  res = RunTool["hugo", "new", "site", dir, "--force", ReplaceInSequence[$hugoOptionsRules][opts], Verbose -> $verbose, DryRun -> $dryRun];
  If[!TrueQ[res], ReturnFailed[]];

  config = $hugoConfigTemplate[<|"SiteName" -> siteName, "HugoTheme" -> hugoTheme|>];

  whenWet @ ExportUTF8[configFile, config];

  themeTarget = PathJoin[dir, "themes", hugoTheme];
  VPrint[If[$PosixQ, "Symlinking", "Copying"], " theme from ", MsgPath @ themeDir, " to ", MsgPath @ themeTarget];
  whenWet @ SymLink[themeDir, themeTarget];

  dir
];

$hugoConfigTemplate = StringFunction @ """
baseURL = 'http://example.org/'
languageCode = 'en-us'
title = '#SiteName'

theme = "#HugoTheme"

[markup.goldmark.renderer]
  unsafe = true

[params]
  BookSearch = false
""";

(**************************************************************************************************)

PublicFunction[HugoBuild]

Options[HugoBuild] = {
  HTMLPath -> Automatic,
  MarkdownPath -> Automatic
};

HugoBuild[dir_Str, opts:OptionsPattern[]] :=
  falseIsFail @ RunTool["hugo", ReplaceInSequence[$hugoOptionsRules][opts], "--ignoreCache", "--cleanDestinationDir", WorkingDirectory -> dir];

(**************************************************************************************************)

PublicFunction[HugoServe]

Options[HugoServe] = {
  HTMLPath -> Automatic,
  MarkdownPath -> Automatic,
  ServingPort -> Automatic
};

HugoServe[dir_Str, opts:OptionsPattern[]] :=
  falseIsFail @ RunTool["hugo", "server", "--ignoreCache", "--noHTTPCache", ReplaceInSequence[$hugoOptionsRules][opts], WorkingDirectory -> dir, StandaloneTerminal -> True];

$hugoOptionsRules = toolKeyTranslationRules @ {
  HTMLPath|"HTMLPath" -> "--destination",
  MarkdownPath|"MarkdownPath" -> "--contentDir",
  ServingPort|"ServingPort" -> "--port"
};

(**************************************************************************************************)

PublicFunction[HugoMathTheme]


