PublicTypesettingBoxFunction[DScatterPlotBox]

Options[DScatterPlotBox] = JoinOptions[
  $baseDPlotOptions,
  PlotRange -> Automatic,
  Ticks -> True
];

(* todo: choose new plotrange whenever filter changes *)

DScatterPlotBox[data_, opts:OptionsPattern[]] := ModuleScope[

  $dvaluePrefix = "XY";
  UnpackOptions[ticks];
  UnpackAssociation[
    procDPlotOptions[DScatterPlotBox, {opts, AdditionalImagePadding -> {Right -> 20, Bottom -> 20}}, "ScaleFactor" -> 1],
    dFilter, dSelection, otherData, imageSize
  ];

  (* this is a static DValue to save space and time *)
  dData = DValue[data, "Data"];
  pData = DGetInitialValue @ data;
  pDataT = Transpose @ pData;
  dDataTrans = DValue[pDataT, "DataTrans"];

  UnpackOptions[plotRange];
  SetAuto[plotRange, CoordinateBounds[pData, Scaled[0.05]]];

  plotBoxes = DBoxes @ Cons[PointBox, Part[dData, dFilter]];

  dBL = DValue[None, "BL"];
  dTR = DValue[None, "TR"];
  selectionBoxes = StyleBox[
    DBoxes @ If[dBL === None, {}, RectangleBox[dBL, dTR]],
    FaceForm @ $dPlotSelectionColor, EdgeForm @ Darker @ SetOpaque @ $dPlotSelectionColor
  ];

  tickBoxes = If[ticks === False, None,
    plotSize = Dist @@@ plotRange;
    scale = imageSize / plotSize;
    GridLineBoxes[plotRange, MaxGridLines -> 100, MinGridLineSpacing -> 10, GraphicsScale -> scale,
      TickOptions -> {LabelSpacing -> {0, 5}}]
  ];

  dStart = DValue[None, "Start"];
  handler = makeDCoordsHandler[Id,
    dStart = $,
    {dBL, dTR} = CoordinateBoundingBox @ {dStart, $},
    If[$ === dStart, dBL = dTR = None],
    dSelection = calcXYSelection[dBL, dTR, dDataTrans]
  ];

  makeDGraphicsBox[
    "PlotBoxes"      -> plotBoxes,
    "SelectionBoxes" -> selectionBoxes,
    "TickBoxes"      -> tickBoxes,
    "PlotRange"      -> plotRange,
    "FramePadding"   -> {{0, 0}, {0, 0}},
    "Handler"        -> handler,
    "OtherData"      -> otherData
  ]
];

_DScatterPlotBox := BadArguments[];

(*************************************************************************************************)

calcXYSelection[___] := All;
calcXYSelection[{x1_, y1_}, {x2_, y2_}, {xs_, ys_}] := Pick[
  Range @ Len @ xs,
  Sign[ClipUnit[xs - x1] * ClipUnit[ys - y1] * ClipUnit[x2 - xs] * ClipUnit[y2 - ys]],
  1
];

