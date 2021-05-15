
If[$VersionNumber < 12.2,
  General::packagewlversion = "Package `` requires Wolfram Langauge version `` or later to operate.";
  Message[General::packagewlversion, "GraphTools`", 12.2]
,

  ClearAll["GraphTools`*"];
  ClearAll["GraphTools`**`*"];

  Block[{Echo = Identity, Print = Hold},
    Get @ FileNameJoin[{FileNameDrop @ $InputFileName, "A0Utilities.m"}]
  ];
];