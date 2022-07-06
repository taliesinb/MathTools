PrivateFunction[outputCellToMarkdown]

outputCellToMarkdown[cell_] := Scope[

  If[$rasterizationFunction === None, Return["#### Placeholder for image"]];

  thisTag = Nothing;
  cell = cell /. TagBox[contents_, "ClassTaggedForm"[tag_]] :> (thisTag = tag; contents);
  rasterizationResult = $rasterizationFunction @ cell;

  If[!AssociationQ[rasterizationResult],
    Print["RasterizationFunction did not return an association: ", Head @ rasterizationResult];
    Return["#### Invalid rasterization result"];
  ];

  rasterizationResult["classlist"] = StringRiffle[{"raster", thisTag}, " "];
  rasterizationResult["caption"] = Replace[$lastCaption, None -> ""];

  markdown = Switch[rasterizationResult["type"],
    "String",
      $stringImageTemplate @ rasterizationResult,
    "File",
      $fileImageTemplate @ rasterizationResult,
    _,
      Print["RasterizationFunction returned invalid association."];
      "#### Invalid rasterization result"
  ];

  markdown
];