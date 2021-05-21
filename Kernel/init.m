
If[$VersionNumber < 12.2,
  General::packagewlversion = "Package `` requires Wolfram Langauge version `` or later to operate.";
  Message[General::packagewlversion, "GraphTools`", 12.2]
,
  If[!IntegerQ[GraphToolsPackageLoader`$LoadCount],
    Get[FileNameJoin[{FileNameDrop @ $InputFileName, "Loader.m"}]]];

  PreemptProtect @ GraphToolsPackageLoader`Load[];

  If[!MemberQ[$ContextPath, "GraphTools`"], AppendTo[$ContextPath, "GraphTools`"]];
];