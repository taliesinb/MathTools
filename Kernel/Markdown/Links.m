PrivateSymbol[$markdownLinkRegexp, $markdownLinkReplacement]

$markdownLinkRegexp = RegularExpression["""\[\[\[([^]@]+)\]\]\]"""];

$markdownLinkReplacement = $markdownLinkRegexp :> $inlineLinkTemplate["$1"];

(**************************************************************************************************)

PrivateFunction[defaultLinkTemplate]

defaultLinkTemplate[title_] := Scope[
  If[StringContainsQ[title, "#"],
    {title, anchor} = StringSplit[title, "#", 2],
    anchor = None;
  ];
  If[StringContainsQ[title, ":"],
    {label, title} = StringSplit[title, ":", 2],
    label = title
  ];
  If[label == "", label = title];
  relPath = If[title === "","", "../" <> titleToURL[title]];
  If[anchor =!= None, relPath = StringJoin[relPath, "#", toAnchorString @ anchor]];
  StringJoin["[", label, "](", relPath, ")"]
];

