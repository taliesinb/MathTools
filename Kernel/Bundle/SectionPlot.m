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
    EdgeColorFunction -> None, EdgeStyle -> $LightGray,
    ImagePadding -> 15, VertexSize -> 8, EdgeThickness -> 2,
    If[$imageSize === Automatic, Seq[], ImageSize -> $imageSize]
  ];

BundleSectionPlotTotal[BundleSection[sec_Association, hash_]] := Scope[
  bundle = hashBundleGraph[hash];
  ExtendedGraphPlot[
    bundle,
    RegionColorRules -> {ConnectedSubgraph[Map[Point, bundleSectionVertices[sec]]] -> $Purple, All -> GrayLevel[0.6, 0.2]},
    VertexColorRules -> {(Alternatives @@ bundleSectionVertices[sec]) -> Darker[$Purple, .2], All -> Transparent},
    ImagePadding -> 10, VertexShapeFunction -> "Point", VertexStyle -> GrayLevel[0.2, 1],
    EdgeSetback -> 0,
    GraphTheme -> "BundleSectionPlot", EdgeSetback -> 0.15, VertexSize -> 6,
    If[$imageSize === Automatic, Seq[], ImageSize -> $imageSize]
  ]
];

BundleSectionPlotLine[BundleSection[sec_Association, hash_]] := Scope[
  bundle = hashBundleGraph[hash];
  ExtendedGraphPlot[
    hashBundleGraph[hash],
    RegionColorRules -> {ConnectedSubgraph[Map[Point, bundleSectionVertices[sec]]] -> $Purple, All -> Transparent},
    VertexSize -> 5, ImagePadding -> 10,
    GraphTheme -> "BundleSectionPlot",
    If[$imageSize === Automatic, Seq[], ImageSize -> $imageSize]
  ]
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

DefineGraphTheme["BundleSectionPlot",
  VertexSize -> 6,
  ImageSize -> ("Edge" -> 20),
  ArrowheadPosition -> 0.75,
  EdgeSetback -> 0.15,
  ExtendImagePadding -> False,
  ArrowheadShape -> None,
  EdgeColorFunction -> "Cardinal", EdgeStyle -> Opacity[1],
  VertexStyle -> GrayLevel[0.3, 1],
  ViewOptions -> {"ShrinkWrap" -> True},
  Frame -> True
];


