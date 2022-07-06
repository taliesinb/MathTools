PublicFunction[PreviewInIAWriter]

Options[PreviewInIAWriter] = $genericExportMarkdownOptions;

$IAWriterPath := $IAWriterPath = FileNameJoin[{
  First @ FileNames["~/Library/Mobile Documents/*~pro~writer/Documents"],
  "Preview"
}];

PreviewInIAWriter[opts:OptionsPattern[]] :=
  PreviewInIAWriter[EvaluationNotebook[], opts];

PreviewInIAWriter[nb_, opts:OptionsPattern[]] := Scope[
  path = ExportToMarkdown[nb,
    $IAWriterPath,
    MarkdownFlavor -> "IAWriter",
    KatexPreludePath -> "assets/prelude.md",
    RasterizationPath -> "assets", HeadingDepthOffset -> 1,
    NotebookCaching -> False, Verbose -> True
  ];
  If[StringQ[path],
    Run["osascript -e 'tell app \"iA Writer\" to open ( POSIX file \"" <> path <> "\" )'"];
    Run["osascript -e 'tell app \"iA Writer\" to activate'"]
  ];
  path
];
