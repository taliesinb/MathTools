PublicFunction[BundleSectionPlot]

Options[BundleSectionPlot] = {
  Method -> "Color",
  ImageSize -> Automatic,
  $ExtendedGraphOptions
};

PrivateVariable[$bundleSectionPlotMethodPattern]

$bundleSectionPlotMethodPattern = Alternatives[HoldPattern @ "Color", "Base", "Total", "Array", "Line"];

BundleSectionPlot::method = "Method should be one of ``.";

BundleSectionPlot[expr_, OptionsPattern[]] := Scope[

  UnpackOptions[method, $imageSize];

  plotter = Switch[method,
    "Color"|"Base", BundleSectionPlotColor,
    "Total",        BundleSectionPlotTotal,
    "Line",         BundleSectionPlotLine,
    "Array",        BundleSectionPlotArray,
    _,              ReturnFailed["method", Select[$bundleSectionPlotMethodPattern, StringQ]]
  ];
  ReplaceAll[
    expr,
    sec:BundleSection[_Association, _Integer] :> RuleCondition @ plotter @ sec
  ]
];

BundleSectionPlotColor[BundleSection[sec_Association, hash_]] :=
  ExtendedGraphPlot[
    hashBaseGraph[hash],
    VertexColorFunction -> (sec /* hashColorFunc[hash]),
    GraphTheme -> "BundleSectionPlot",
    If[$imageSize === Automatic, Seq[], ImageSize -> $imageSize]
  ];

BundleSectionPlotTotal[BundleSection[sec_Association, hash_]] :=
  ExtendedGraphPlot[
    hashBundleGraph[hash],
    RegionColorRules -> {ConnectedSubgraph[Map[Point, bundleSectionVertices[sec]]] -> $Red, All -> $LightGray},
    GraphTheme -> "BundleSectionPlot",
    LayoutDimension -> 2,
    If[$imageSize === Automatic, Seq[], ImageSize -> $imageSize]
  ];

BundleSectionPlotLine[BundleSection[sec_Association, hash_]] :=
  ExtendedGraphPlot[
    hashBundleGraph[hash],
    RegionColorRules -> {ConnectedSubgraph[Map[Point, bundleSectionVertices[sec]]] -> $Red, All -> Transparent},
    VertexSize -> 5, ImagePadding -> 10,
    GraphTheme -> "BundleSectionPlot",
    LayoutDimension -> 2,
    If[$imageSize === Automatic, Seq[], ImageSize -> $imageSize]
  ];

BundleSectionPlotArray[BundleSection[sec_Association, hash_]] := Scope[
  bn = VertexCount @ hashBaseGraph[hash]; fn = (VertexCount @ hashBundleGraph[hash]) / bn;
  vals = Lookup[sec, hashBaseVerts[hash]];
  cols = ToRGB @ hashColorFunc[hash] @ vals;
  If[bn > 8 && IntegerQ @ Sqrt[bn],
    FadedMeshImage[Partition[cols, Sqrt[bn]], 4],
    FadedMeshImage[List @ cols, 6]
  ]
];

hashBundleGraph[hash_] := bundleHashLookup[hash, "BundleGraph"];
hashBaseGraph[hash_] := bundleHashLookup[hash, "BaseGraph"];
hashBaseVerts[hash_] := bundleHashLookup[hash, "BaseVertices"];
hashColorFunc[hash_] := bundleHashLookup[hash, "FiberVertexColorFunction"];

(**************************************************************************************************)

bundleSectionBaseVertices[assoc_] := Keys @ assoc;
bundleSectionVertices[assoc_] := KeyValueMap[BundleVertex, assoc];

(**************************************************************************************************)

$BundleSectionPlotThemeRules = {
  AspectRatioClipping -> False,
  ArrowheadSize -> 0,
  VertexSize -> 8,
  Frame -> True,
  ImagePadding -> 10,
  ImageSize -> "Edge" -> 20
};

$GraphThemeData["BundleSectionPlot"] := $BundleSectionPlotThemeRules;
