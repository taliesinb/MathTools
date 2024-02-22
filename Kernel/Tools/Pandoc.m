PublicIOFunction[PandocConvert]

PandocConvert::failed = "Pandoc failed on input `` and output ``.";

PandocConvert[infile_Str, outfile_Str] := CatchMessage @ Scope[
	res = RunTool["pandoc", "-i", infile, "-o", outfile, "--standalone", "--katex"];
	If[!TrueQ[res], ReturnFailed["failed", MsgPath[infile], MsgPath[outfile]]];
	outfile
];

(**************************************************************************************************)

PublicIOFunction[PandocConvertToHTML]

PandocConvertToHTML[inpath_Str] := PandocConvert[inpath, ReplaceFileExtension[inpath, "html"]];

(**************************************************************************************************)

PublicIOFunction[PandocExportMDToHTML]

SetUsage @ "
PandocExportMDToHTML[indir$, outdir$] converts all markdown files in indir$ to matching HTML files in outdir$.
PandocExportMDToHTML[indir$ -> {file$1, file$2, $$}, outdir$] converts specific files.
* If outdir$ does not exist it is created.
"

PandocExportMDToHTML[indir_Str, outpath_Str] :=
	PandocExportMDToHTML[indir -> FileNames["*.md", NormalizePath @ indir, Inf], outpath];

PandocExportMDToHTML[inpath_Str -> infiles:{___Str}, outpath_Str] := Scope[
	inpath //= NormalizePath;
	outpath //= NormalizePath;
	EnsureDirectory[outpath];
	inlen = SLen @ inpath;
	Map[
		infile |-> (
			outfile = PathJoin[outpath, ReplaceFileExtension[SDrop[infile, inlen], "html"]];
			PandocConvert[infile, outfile]
		),
		NormalizePath /@ infiles
	]
]

PandocExportMDToHTML[___] := $Failed;