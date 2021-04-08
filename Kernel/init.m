
If[$VersionNumber < 12.2,
  General::packagewlversion = "Package `` requires Wolfram Langauge version `` or later to operate.";
  Message[General::packagewlversion, "GraphTools`", 12.2]
,

  System`Negated;

  ClearAll["GraphTools`*"];
  ClearAll["GraphTools`**`*"];

  (*If[Contexts["ArrayTools`"] === {}, Get["~/git/taliesinb/ArrayTools/Kernel/init.m"]];*)
  (*Get["~/git/GraphTools/Kernel/init.m"];*)
  Get @ FileNameJoin[{FileNameDrop @ $InputFileName, "AUtilities.m"}];
];