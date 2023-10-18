System`EdgeThickness;

QuiverGeometryLoader`$initFile = $InputFileName;

Block[{$AllowInternet = False},

If[$VersionNumber < 12.2,
  General::packagewlversion = "Package `` requires Wolfram Langauge version `` or later to operate.";
  Message[General::packagewlversion, "QuiverGeometry`", 12.2]
,
  If[!IntegerQ[QuiverGeometryLoader`$LoadCount] || TrueQ[QuiverGeometryLoader`NeedsSelfLoad[]],
    If[TrueQ[QuiverGeometryLoader`$Verbose], Print["Loading loader.m"]];
    Get[FileNameJoin[{FileNameDrop @ $InputFileName, "Loader.m"}]]];

  QuiverGeometryLoader`Load[True, True];

  If[!MemberQ[$ContextPath, "QuiverGeometry`"],
    AppendTo[$ContextPath, "QuiverGeometry`"];
    AppendTo[$ContextPath, "QuiverGeometryCaches`"];
  ];
];

];