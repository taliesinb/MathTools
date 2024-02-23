PublicIOFunction[HugoNewSite]

PublicOption[HugoTheme]

Options[HugoNewSite] = JoinOptions[
  SiteName -> "New Site",
  HugoTheme -> "taliesinb/hugo-math/master",
  $genericToolOpts
];

HugoNewSite::notPath = "First arg `` should be a path on disk.";
HugoNewSite::exists = "A Hugo site already exists at ``.";
HugoNewSite::noparent = "Cannot create a Hugo site at `` because parent directory does not exist.";
HugoNewSite::invalidtheme = "HugoTheme -> `` should be a string of the form \"username/repo\".";

HugoNewSite[dir_Str, opts:OptionsPattern[]] := Scope[

  UnpackOptions[siteName, hugoTheme, $verbose, $dryRun];
  SetAuto[$verbose, $dryRun];

  If[SFreeQ[dir, $PathnameSeparator], ReturnFailed["notPath", dir]];
  dir //= NormalizePath;
  configFile = PathJoin[dir, "hugo.toml"];
  If[FileExistsQ[dir],
    If[!DirectoryQ[dir], ReturnFailed[]];
    If[DirectoryQ[dir] && FileExistsQ[configFile], ReturnFailed["exists", MsgPath @ dir]];
  ];
  parent = FileNameDrop[dir];
  If[!DirectoryQ[parent], ReturnFailed["noparent", MsgPath @ dir]];

  If[!StrQ[hugoTheme] || SFreeQ[hugoTheme, "/"], ReturnFailed["invalidtheme", hugoTheme]];
  {themeProvider, hugoTheme, themeBranch} = PadRight[SSplit[hugoTheme, "/", 3], 3, None];
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

$hugoConfigTemplate = StringFunction @ STrim @ """
baseURL = 'http://example.org/'
languageCode = 'en-us'
title = '#SiteName'

theme = "#HugoTheme"

[markup.goldmark.renderer]
  unsafe = true

# this allows {.table-no-header} to work after a table
[markup.goldmark.parser.attribute]
  block = true

[params]
  BookSearch = false
""";

(**************************************************************************************************)

PublicIOFunction[HugoBuild]

Options[HugoBuild] = {
  HTMLPath -> Auto,
  MarkdownPath -> Auto
};

HugoBuild[dir_Str, opts:OptionsPattern[]] :=
  falseIsFail @ RunTool["hugo", ReplaceInSequence[$hugoOptionsRules][opts], "--ignoreCache", "--cleanDestinationDir", WorkingDirectory -> dir];

(**************************************************************************************************)

PublicIOFunction[HugoServe]

Options[HugoServe] = {
  HTMLPath -> Auto,
  MarkdownPath -> Auto,
  ServingPort -> Auto
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

