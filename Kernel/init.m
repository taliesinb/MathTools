System`EdgeThickness;

QuiverGeometryPackageLoader`$initFile = $InputFileName;

If[$VersionNumber < 12.2,
  General::packagewlversion = "Package `` requires Wolfram Langauge version `` or later to operate.";
  Message[General::packagewlversion, "QuiverGeometry`", 12.2]
,
  If[!IntegerQ[QuiverGeometryPackageLoader`$LoadCount] || TrueQ[QuiverGeometryPackageLoader`NeedsSelfLoad[]],
    If[TrueQ[QuiverGeometryPackageLoader`$Verbose], Print["Loading loader.m"]];
    Get[FileNameJoin[{FileNameDrop @ $InputFileName, "Loader.m"}]]];

  Block[{CellPrint}, QuiverGeometryPackageLoader`Load[]];

  If[!MemberQ[$ContextPath, "QuiverGeometry`"], AppendTo[$ContextPath, "QuiverGeometry`"]];
];