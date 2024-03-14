MTLoader`$InitFile = $InputFileName;

Block[{$AllowInternet = False, MTLoader`$MinimumVersion = 13.3},

  If[$VersionNumber < MTLoader`$MinimumVersion,
    General::packagewlversion = "Package `` requires Wolfram Langauge version `` or later to operate.";
    Message[General::packagewlversion, "MathTools`", MTLoader`$MinimumVersion];
  ,
    If[!IntegerQ[MTLoader`$LoadCount] || TrueQ[MTLoader`NeedsSelfLoad[]],
      If[TrueQ[MTLoader`$LoadVerbose], Print["Loading loader.m"]];
      Get[FileNameJoin[{FileNameDrop @ $InputFileName, "Loader.m"}]]];

    MTLoader`LoadSource[True, False];
  ];

];