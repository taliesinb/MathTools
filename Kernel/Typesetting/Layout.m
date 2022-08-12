PublicOption[MaxWidth, ItemFunction, LabelFunction, LabelSpacing]

SetUsage @ "MaxWidth is an option to SpacedRow."
SetUsage @ "ItemFunction is an option to SpacedRow."
SetUsage @ "LabelFunction is an option to SpacedRow."
SetUsage @ "LabelSpacing is an option to SpacedRow."

(**************************************************************************************************)

PublicFunction[SpacedColumn]

SpacedColumn[args___] := SpacedRow[args, Transposed -> True];

(**************************************************************************************************)

PublicFunction[ClickCopyRow]

ClickCopyRow[args___] := Framed[
  SpacedRow[args, ItemFunction -> ClickCopy, SpliceForms -> False],
  Background -> RGBColor[{1,1,1}*0.95], FrameStyle -> None];

(**************************************************************************************************)

PublicFunction[ClickCopy]

ClickCopy[e_] := With[
  {copyExpr = Cell[BoxData @ ToBoxes @ TraditionalForm @ e, FormatType -> TraditionalForm]},
  MouseAppearance[
  EventHandler[Framed[e, Background -> GrayLevel[0.99], FrameStyle -> GrayLevel[0.95], ImageMargins -> {{5, 5}, {5, 5}}], {"MouseClicked" :> CopyToClipboard[copyExpr]}],
  "LinkHand"
]];

(**************************************************************************************************)

PublicOption[Transposed, RiffleItem, ForceGrid]

SetUsage @ "Transposed is an option to %SpacedRow, %AlgebraicRow, etc."
SetUsage @ "RiffleItem is an option to %SpacedRow, %AlgebraicRow, etc."
SetUsage @ "ForceGrid is an option to %SpacedRow, %AlgebraicRow, etc."

(**************************************************************************************************)

PublicFunction[SpacedColumnRow]

SpacedColumnRow[items___] := Scope[
  $srColumnRow = True;
  SpacedRow[items]
];

(**************************************************************************************************)

PublicFunction[SpacedRow]
PublicOption[SpliceForms, IndexTooltip, ClickFunction]

$srColumnRow = False;

Options[SpacedRow] = {
  Spacings -> ($srSpacings = 20),
  RowSpacings -> ($srRowSpacings = 5),
  MaxItems -> ($srMaxItems = Infinity),
  MaxWidth -> ($srMaxWidth = Infinity),
  LabelStyle -> ($srLabelStyle = $LabelStyle),
  BaseStyle -> ($srBaseStyle = {}),
  ItemStyle -> ($srItemStyle = {}),
  ItemFunction -> ($srItemFunction = Identity),
  LabelFunction -> ($srLabelFunction = Identity),
  LabelSpacing -> ($srLabelSpacing = 5),
  Transposed -> ($srTransposed = False),
  IndexTooltip -> ($srIndexTooltip = False),
  Alignment -> ($srAlignment = Center),
  LabelPosition -> ($srLabelPosition = Automatic),
  ForceGrid -> ($srForceGrid = False),
  RiffleItem -> ($srRiffleItem = None),
  FontSize -> ($srLabelFontSize = 15),
  SpliceForms -> ($srSpliceForms = True),
  ClickFunction -> ($srClickFunction = None)
};

(* this is because i don't trust OptionsPattern to not capture rules used as label specs.
i might be wrong though *)

SpacedRow[elems__, MaxWidth -> n_] := Block[{$srMaxWidth = n}, SpacedRow[elems]];
SpacedRow[elems__, MaxItems -> n_] := Block[{$srMaxItems = n}, SpacedRow[elems]];
SpacedRow[elems__, Spacings -> n_] := Block[{$srSpacings = n}, SpacedRow[elems]];
SpacedRow[elems__, RowSpacings -> n_] := Block[{$srRowSpacings = n}, SpacedRow[elems]];
SpacedRow[elems__, LabelStyle -> style_] := Block[{$srLabelStyle = style}, SpacedRow[elems]];
SpacedRow[elems__, BaseStyle -> s_] := Block[{$srBaseStyle = s}, SpacedRow[elems]];
SpacedRow[elems__, ItemStyle -> s_] := Block[{$srItemStyle = s}, SpacedRow[elems]];
SpacedRow[elems__, ItemFunction -> f_] := Block[{$srItemFunction = wrappedItemFunc @ f}, SpacedRow[elems]];
SpacedRow[elems__, LabelFunction -> f_] := Block[{$srLabelFunction = f}, SpacedRow[elems]];
SpacedRow[elems__, LabelSpacing -> s_] := Block[{$srLabelSpacing = s}, SpacedRow[elems]];
SpacedRow[elems__, LabelPosition -> s_] := Block[{$srLabelPosition = s}, SpacedRow[elems]];
SpacedRow[elems__, Alignment -> a_] := Block[{$srAlignment = a}, SpacedRow[elems]];
SpacedRow[elems__, "IndexTooltip" -> t_] := Block[{$srIndexTooltip = t}, SpacedRow[elems]];
SpacedRow[elems__, Transposed -> t_] := Block[{$srTransposed = t}, SpacedRow[elems]];
SpacedRow[elems__, ForceGrid -> fg_] := Block[{$srForceGrid = fg}, SpacedRow[elems]];
SpacedRow[elems__, RiffleItem -> item_] := Block[{$srRiffleItem = item}, SpacedRow[elems]];
SpacedRow[elems__, FontSize -> sz_] := Block[{$srLabelFontSize = sz}, SpacedRow[elems]];
SpacedRow[elems__, SpliceForms -> b_] := Block[{$srSpliceForms = b}, SpacedRow[elems]];
SpacedRow[elems__, ClickFunction -> c_] := Block[{$srClickFunction = c}, SpacedRow[elems]];

wrappedItemFunc[f_][EndOfLine] := EndOfLine;
wrappedItemFunc[f_][e_] := f @ e;

SpacedRow[labels_List -> items_List] /; SameLengthQ[labels, items] :=
  SpacedRow[RuleThread[labels, items]];

SpacedRow[elems__] := Scope[
  items = DeleteCases[Null] @ Flatten @ {elems};
  If[$srSpliceForms, items //= Map[procInlineForms]];
  items = canonicalizeItem /@ Take[items, UpTo @ $srMaxItems];
  If[$srRiffleItem =!= None, items = Riffle[items, $srRiffleItem]];
  If[$srColumnRow && Length[items] > (maxWidth = Replace[$srMaxWidth, Infinity -> 4]),
    Return @ SpacedColumn[
      Map[SpacedRow, Partition[items, UpTo[maxWidth]]],
      Spacings -> $srRowSpacings
    ];
  ];
  If[$srIndexTooltip, items //= MapIndex1[NiceTooltip]];
  hasLabels = MemberQ[items, _Labeled];
  tooLong = IntegerQ[$srMaxWidth] && Length[items] > $srMaxWidth;
  alignment = $srAlignment;
  If[!ListQ[alignment], alignment = {alignment, alignment}];
  rowSpacings = $srRowSpacings / 10;
  labelSpacing = $srLabelSpacing / 10;
  labelPosition = $srLabelPosition;
  SetAutomatic[labelPosition, If[$srTransposed, Before, After]];
  labelIsBefore = labelPosition === Before;
  If[ListQ[$srMaxWidth],
    items = Insert[items, EndOfLine, List /@ TakeWhile[1 + (Accumulate @ $srMaxWidth), LessEqualThan[Length @ items]]]
  ];
  hasEndOfLines = MemberQ[items, EndOfLine];
  If[tooLong || hasLabels || $srForceGrid || hasEndOfLines,
    Which[
      hasEndOfLines,
        items = VectorReplace[items, EndOfLine -> $nextRow],
      tooLong,
        items = Flatten @ Riffle[Partition[items, UpTo[$srMaxWidth]], {$nextRow}],
      True,
        Null
    ];
    If[hasLabels,
      items //= Map[toGridRowPair /* If[labelIsBefore, Reverse, Identity]];
      entries = unfoldRow /@ SequenceSplit[items, {$nextRow}];
      vspacings = {labelSpacing, rowSpacings};
      itemStyle = {{$srItemStyle, $srLabelStyle}};
      If[labelIsBefore, itemStyle //= Map[Reverse]];
    ,
      vspacings = {rowSpacings};
      entries = SequenceSplit[items, {$nextRow}];
      itemStyle = {$srItemStyle};
    ];
    hspacings = {$srSpacings/10};
    
    If[$srTransposed,
      (* i don't think this is needed, but just in case. i can enable it.
      maxLen = Max[Length /@ entries];
      entries = PadRight[#, maxLen, ""]& /@ entries;
      *)
      entries //= Transpose;
      styles = {itemStyle, {}};
      {hspacings, vspacings} = {vspacings * 1.5, hspacings * 0.5};
      alignment //= Reverse;
    ,
      styles = {{}, itemStyle};
    ];
    Grid[
      entries,
      Alignment -> alignment,
      Spacings -> {{0, hspacings}, {0, vspacings}},
      ItemStyle -> styles,
      BaseStyle -> $srBaseStyle
    ]
  ,
    If[$srTransposed,
      Column[items,
        Spacings -> $srSpacings/20, BaseStyle -> ToList[$srItemStyle, $srBaseStyle],
        Alignment -> alignment
      ],
      Row[items, Spacer[$srSpacings],
        BaseStyle -> ToList[$srItemStyle, $srBaseStyle]
      ]
    ]
  ]
];

procInlineForms = Case[
  (head_Symbol)[args___] /; KeyExistsQ[$infixFormCharacters, head] :=
    Splice @ Riffle[{args}, LargeSymbolForm @ $infixFormCharacters @ head];

  other_ := other;
];

canonicalizeItem = Case[
  l_ -> i_        := % @ Labeled[i, l];
  Labeled[i_, l_] := Labeled[clickFunc[$srClickFunction] @ $srItemFunction @ i, $srLabelFunction @ l];
  other_          := clickFunc[$srClickFunction] @ $srItemFunction @ other;
];

toGridRowPair = Case[
  Labeled[item_, label_, ___] := {item, Style[label, FontSize -> $srLabelFontSize]};
  $nextRow := $nextRow;
  item_ := {item, ""};
];

unfoldRow[pairs_] :=
  Splice @ Transpose @ pairs;

clickFunc[None] := Identity;
clickFunc[f_][e_] := ClickForm[e, f[e]];

(**************************************************************************************************)

PublicVariable[$LargeEllipsis]

$LargeEllipsis = Style["\[Ellipsis]", $LabelStyle, Gray, 18]

(**************************************************************************************************)

PublicVariable[$LargeVerticalEllipsis]

$LargeVerticalEllipsis = Style["\[VerticalEllipsis]", $LabelStyle, Gray, 18]

(**************************************************************************************************)

PublicFunction[MakeArrow]

MakeArrow[w_:50, h_:15, thickness_:1, style_:Black] =
  makeNotationArrow[w, h, thickness, style];

(**************************************************************************************************)

PublicFunction[SpacedArrow]

SpacedArrow[l__, "ArrowColor" -> color_, r___] := Block[{$arrowColor = color},
  SpacedArrow[l, r]
];

SpacedArrow[l__, "ArrowThickness" -> thick_, r___] := Block[{$arrowThickness = thick},
  SpacedArrow[l, r]
];

SpacedArrow[a_, b_, rest___] :=
  SpacedRow[a, $smallNotationArrow, b, rest];

makeNotationArrow[w_, h_, thickness_, style___, opts___Rule] := Scope[
  h2 = h/2;
  line = Line[{{-w, 0}, Offset[{-thickness, 0}, {0,0}]}];
  head = Line[{{-h2, -h2}, {0, 0}, {-h2, h2}}];
  Graphics[{
    CapForm["Round"], JoinForm["Round"], AbsoluteThickness[thickness], $DarkGray,
    style, line, head},
    opts,
    ImageSize -> {w + 2, h + 2}, PlotRangePadding -> 0, ImagePadding -> {{1, 1}, {1, 1}},
    BaselinePosition -> Center
  ]
];

$arrowThickness = 1.1;
$arrowColor = $LightGray;
$smallNotationArrow := MakeArrow[30,10, $arrowThickness, $arrowColor];

(**************************************************************************************************)

PublicFunction[Gallery]

Options[Gallery] = {
  ImageSize -> 1000,
  Spacings -> Automatic
};

Gallery[elems_, OptionsPattern[]] := Scope[
  UnpackOptions[imageSize, spacings];
  {w, h} = ToNumericImageSize[imageSize, 1];
  elems = Flatten @ List @ elems;
  n = Length[elems];
  elems = Map[graphToGraphics, elems];
  size = estimateItemSize @ First @ elems;
  If[n > 16,
    m = Floor[N[w / size]],
    m = SelectFirst[{10, 9, 8, 7, 6, 5, 4, 3, 2, 2}, Divisible[n, #] && (size * #) < w&, Floor[N[w / size]]];
  ];
  Grid[
    Partition[attachEventHandlers @ elems, UpTo[m]],
    Alignment -> {Center, Top},
    Spacings -> spacings
  ]
];

attachEventHandlers[elems_] := MapIndexed[
  ClickForm[#, Print[First @ #2]]&,
  elems
];

graphToGraphics[Labeled[g_, x_]] := Labeled[graphToGraphics @ g, x];
graphToGraphics[g_Graph] := ExtendedGraphPlot @ g;
graphToGraphics[e_] := e;

lookupImageWidth[g_] := First @ LookupImageSize @ g;

estimateItemSize = Case[
  g_Graphics | g_Graphics3D := lookupImageWidth[g];
  Labeled[g_, _]            := %[g];
  Legended[g_, _]           := %[g] + 50;
  other_                    := First[Rasterize[other, "RasterSize"]] * 2;
];
