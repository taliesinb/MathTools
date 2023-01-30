PublicFunction[YoutubeDL]

$youtubeDLArgData = ExpressionTable[
  "Output"           None          "output"
  "GetURL"           False         "get-url"
  "GetTitle"         False         "get-title"
  "GetThumbnail"     False         "get-thumbnail"
  "GetDescription"   False         "get-description"
  "GetFilename"      False         "get-filename"
  "GetDuration"      False         "get-duration"
  "GetFormat"        False         "get-format"
  "DumpJSON"         False         "dump-json"
  "DumpSingleJSON"   False         "dump-single-json"
  "FlatPlaylist"     False         "flat-playlist"
  "PlaylistStart"    Automatic     "playlist-start"
  "PlaylistEnd"      Automatic     "playlist-end"
  "PlaylistItems"    Automatic     "playlist-items"
  "MatchTitle"       Automatic     "match-title"
  "RejectTitle"      Automatic     "reject-title"
  "MaxDownloads"     None          "max-downloads"
  "MinFilesize"      None          "min-filesize"
  "MaxFilesize"      None          "max-filesize"
  "DateBefore"       None          "date-before"
  "DateAfter"        None          "date-after"
  "BatchFile"        None          "batch-file"
  "NoOverwrites"     False         "no-overwrites"
  "WriteDescription" False         "write-description"
  "WriteInfoJSON"    False         "write-info-json"
  "WriteAnnotations" False         "write-annotations"
  "WriteThumbnail"   False         "write-thumbnail"
  "Quiet"            False         "quiet"
  "NoWarnings"       False         "no-warnings"
  "Simulate"         False         "simulate"
  "SkipDownload"     False         "skip-download"
  "PrintJSON"        False         "print-json"
  "NoProgress"       False         "no-progress"
  "Format"           Automatic     "format"
  "WriteSub"         False         "write-sub"
  "WriteAutoSub"     False         "write-auto-sub"
  "AllSubs"          False         "all-subs"
  "SubLang"          Automatic     "sub-lang"
  "EmbedSebs"        False         "embed-subs"
  "AddMetadata"      False         "add-metadata"
  "Xattrs"           False         "xattrs"
  "NoMtime"          False         "no-mtime"
];

Options[YoutubeDL] = #1 -> #2& @@@ $youtubeDLArgData;

$optRename = Association[#1 -> #3& @@@ $youtubeDLArgData];

YoutubeDL::badopt = "Unknown options ``."
YoutubeDL[url_String, opts___Rule] := Scope[
  opts = DeleteCases[{opts}, Alternatives @@ Options[YoutubeDL]];
  args = MapApply[
    {key, val} |-> (
      arg = "--" <> Lookup[$optRename, #1, ReturnFailed["badopt", #1]];
      If[val === True, arg, Splice[{arg, val}]]
    )&,
    opts
  ];
  ZRunTool["youtube-dl", url, Sequence @@ args]
]

