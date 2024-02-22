PrivateSymbol[$markdownLinkRegexp, $markdownLinkReplacement]

$markdownLinkRegexp = RegularExpression["""\[\[\[([^]@]+)\]\]\]"""];

$markdownLinkReplacement = $markdownLinkRegexp :> $inlineLinkTemplate["$1"];

(**************************************************************************************************)

PrivateFunction[defaultLinkTemplate]

defaultLinkTemplate[title_] := Scope[
  If[SContainsQ[title, "#"],
    {title, anchor} = SSplit[title, "#", 2],
    anchor = None;
  ];
  If[SContainsQ[title, ":"],
    {label, title} = SSplit[title, ":", 2],
    label = title
  ];
  If[label == "", label = title];
  relPath = If[title === "","", "../" <> titleToURL[title]];
  If[anchor =!= None, relPath = SJoin[relPath, "#", toAnchorString @ anchor]];
  SJoin["[", label, "](", relPath, ")"]
];

