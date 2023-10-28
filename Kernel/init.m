QuiverGeometryLoader`$InitFile = $InputFileName;

Block[{$AllowInternet = False, QuiverGeometryLoader`$MinimumVersion = 13.3},

  If[$VersionNumber < QuiverGeometryLoader`$MinimumVersion,
    General::packagewlversion = "Package `` requires Wolfram Langauge version `` or later to operate.";
    Message[General::packagewlversion, "QuiverGeometry`", QuiverGeometryLoader`$MinimumVersion];
  ,
    If[!IntegerQ[QuiverGeometryLoader`$LoadCount] || TrueQ[QuiverGeometryLoader`NeedsSelfLoad[]],
      If[TrueQ[QuiverGeometryLoader`$Verbose], Print["Loading loader.m"]];
      Get[FileNameJoin[{FileNameDrop @ $InputFileName, "Loader.m"}]]];

    QuiverGeometryLoader`LoadSource[True, True];
  ];

];