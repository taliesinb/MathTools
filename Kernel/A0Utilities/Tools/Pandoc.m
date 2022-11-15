PublicFunction[PandocConvert]

PandocConvert::failed = "Pandoc failed on input `` and output ``.";

PandocConvert[infile_String, outfile_String] := CatchMessage @ Scope[
	res = RunTool["pandoc", "-i", infile, "-o", outfile, "--standalone", "--katex"];
	If[FailureQ[res], ReturnFailed["failed", MsgPath[infile], MsgPath[outfile]]];
	outfile
];

(**************************************************************************************************)

PublicFunction[PandocConvertToHTML]

PandocConvertToHTML[inpath_String] := PandocConvert[inpath, ReplaceFileExtension[inpath, "html"]];

(**************************************************************************************************)

PublicFunction[PandocExportMDToHTML]

SetUsage @ "
PandocExportMDToHTML[indir$, outdir$] converts all markdown files in indir$ to matching HTML files in outdir$.
PandocExportMDToHTML[indir$ -> {file$1, file$2, $$}, outdir$] converts specific files.
* If outdir$ does not exist it is created.
"

PandocExportMDToHTML[indir_String, outpath_String] :=
	PandocExportMDToHTML[indir -> FileNames["*.md", NormalizePath @ indir, Infinity], outpath];

PandocExportMDToHTML[inpath_String -> infiles:{___String}, outpath_String] := Scope[
	inpath //= NormalizePath;
	outpath //= NormalizePath;
	EnsureDirectory[outpath];
	inlen = StringLength @ inpath;
	Map[
		infile |-> (
			outfile = FileNameJoin[{outpath, ReplaceFileExtension[StringDrop[infile, inlen], "html"]}];
			PandocConvert[infile, outfile]
		),
		NormalizePath /@ infiles
	]
]

PandocExportMDToHTML[___] := $Failed;