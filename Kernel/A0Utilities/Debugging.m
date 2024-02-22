PublicFunction[ReapGraphics]

$dbEnabled = False;

SetHoldFirst[ReapGraphics];

ReapGraphics[body_, opts___Rule] := Scope[
  $dgBag = Bag[]; $extraDg = {};
  $dbEnabled = True;
  result = body;
  debugPrims = BagPart[$dgBag, All];
  Print @ Row @ Pre[$extraDg, Graphics[
    {FaceForm[None], EdgeForm[Opacity[0.5]], debugPrims},
    opts,
    PlotRangePadding -> Scaled[0.15],
    Frame -> True, FrameTicks -> False,
    ImageSize -> 200
  ]];
  result
];

(**************************************************************************************************)

PublicTypesettingForm[BoxesDebugForm]

DefineStandardTraditionalForm[f_BoxesDebugForm :> boxesDebugFormBoxes[f]];

(* TODO: disable the ShowAutoStyles workaround, which is to make it readable in DarkMode *)
boxesDebugFormBoxes[BoxesDebugForm[g_]] := Scope[
  contentBoxes = MakeBoxes @ g;
  framedContentBoxes = FrameBox[contentBoxes, FrameStyle -> GrayLevel[0.9]];
  framedContentBoxes = PaneBox[framedContentBoxes, ImageSize -> {{250, 800}, Full}];
  codeBoxes = ToBoxes @ PrettyCodeForm @ contentBoxes;
  framedCodeBoxes = FrameBox[
    StyleBox[codeBoxes, "Input", 10, Background -> None, FontColor -> Black, ShowAutoStyles -> False],
    Background -> GrayLevel[0.95], FrameStyle -> GrayLevel[0.9]
  ];
  GridBox[{{framedContentBoxes, "     ", framedCodeBoxes}}, RowAlignments -> Center]
];

(**************************************************************************************************)

PublicTypesettingForm[PrettyCodeForm]

DefineStandardTraditionalForm[f_PrettyCodeForm :> prettyCodeBoxes[f]];

prettyCodeBoxes[PrettyCodeForm[e_, opts___Rule]] := Scope[
  codeBoxes = MakeBoxes @ CompactPrettyFullForm[e, opts, CompactingWidth -> 100];
  FrameBox[
    StyleBox[codeBoxes, "Code", 10, Background -> None, FontColor -> Black, ShowAutoStyles -> False],
    Background -> GrayLevel[0.95], FrameStyle -> GrayLevel[0.9]
  ]
];

(**************************************************************************************************)

PublicSpecialFunction[TraceFrontendCalls]

SetHoldFirst[TraceFrontendCalls];

TraceFrontendCalls[body_] := Flatten @ Trace[body, _MathLink`CallFrontEnd, TraceInternal -> True];

(**************************************************************************************************)

PublicDebuggingFunction[SowGraphics, EchoDebugGraphics]

SetHoldAll[SowGraphics];

SowGraphics[e__] /; TrueQ[$dbEnabled] := sowDGexpr[e];
SowGraphics[__] := Null;

EchoDebugGraphics[e_] /; TrueQ[$dbEnabled] := (SowGraphics[e]; e)
EchoDebugGraphics[e_] := e;

$graphicsHead = Point | Polygon | Circle | Disk | Rectangle;
sowDGexpr = Case[
  g_Graphics                := AppTo[$extraDg, ReplaceOptions[g, {ImageSize -> 200, Frame -> True, FrameTicks -> False}]];
  Seq[c:($ColorPattern|_Int), p_] := % @ Style[p, EdgeForm @ ToRainbowColor @ c];
  p:$Coord2P                := % @ Point @ p;
  p:$CoordMat2P             := % @ Point @ p;
  p_                        := StuffBag[$dgBag, p];
];

(**************************************************************************************************)

PrivateSpecialFunction[VPrint, VBlock]

$vprintDepth = 0;

SetHoldAllComplete[VPrint];
VPrint[args___] :=
  If[TrueQ[$verbose], Print[SRepeat["\t", $vprintDepth], If[TrueQ[$dryRun], Style["> ", LightGray], ""], args]];

SetHoldAllComplete[VBlock];
VBlock[body_] := Block[{$vprintDepth = $vprintDepth + 1}, body];

(**************************************************************************************************)

PrivateSpecialFunction[VMessage]

SetHoldFirst[VMessage];

(* TODO: fix case where message starts with a ``. *)
VMessage[msg_, args___] := If[TrueQ[$verbose],
  Print @ Style[StringForm[msg, args], $DarkRed, Bold],
  Message[msg, args]
];

(**************************************************************************************************)

PublicFunction[Verbosely]

SetHoldAllComplete[Verbosely]

Verbosely[e_] := Block[{$verbose = True}, e];

(**************************************************************************************************)

PublicDebuggingFunction[EchoEdgeList]

EchoEdgeList = EchoFunction[EdgeList]

(**************************************************************************************************)

PublicDebuggingFunction[EchoGraphicsScope, EchoGraphics]

SetHoldAllComplete[EchoGraphicsScope];
EchoGraphicsScope[e_] := Scope[
  $prims = {};
  res = e;
  n = Len[$prims]; $i = 1;
  Graphics[
    Map[{Opacity[$i++ / n], #}&, $prims],
    Frame -> True, PlotRangePadding -> Scaled[0.1]
  ];
  res
];

(**************************************************************************************************)

PrivateTypesettingBoxFunction[ToGraphicsBoxes, ToGraphics3DBoxes]

ToGraphicsBoxes[{}] := {};
ToGraphicsBoxes[e_] := Typeset`MakeBoxes[e, StandardForm, Graphics] //. $gboxSimpRules;
ToGraphics3DBoxes[e_] := Typeset`MakeBoxes[e, StandardForm, Graphics3D] //. $gboxSimpRules;

$gboxSimpRules = {InterpretationBox[b_, _] :> b, Typeset`Hold[h_] :> h};

(**************************************************************************************************)

PublicDebuggingFunction[DynamicPointGraphics]

circlePoints[2] := {{-1, 0}, {1, 0}};
circlePoints[n_] := Rev @ CirclePoints[n];

DynamicPointGraphics[n_Int, fn_] := Rep[
  ConstructHoldComplete[fn, \[FormalX]],
  HoldC[body_] :>
    DynamicModule @@ Hold[
      {\[FormalX] = circlePoints[n]},
      If[QuiverGeometryLoader`$CurrentlyLoading, "LOADING",
        LocatorPane[Dynamic[\[FormalX]], Graphics[
          Dynamic[safeDynamicEval @ body, TrackedSymbols :> {\[FormalX]}],
          PlotRange -> 1.1, Frame -> True, FrameTicks -> None, Axes -> None
        ]]
      ],
      Initialization :> $PackageInitializer,
      TrackedSymbols :> {\[FormalX]}
    ]
];

DynamicPointGraphics[{n_Int, specSeq__}, fn_] := With[
  {specList = {specSeq}},
  {specData = MapThread[toDynSpec, {{specSeq}, Take[$formals, Len @ specList]}]},
  {specVars = Pre[\[FormalX]] @ Part[specData, All, 1],
   initList = Pre[N @ circlePoints[n]] @ Part[specData, All, 2]},
  {specSets = MapThread[SET, {specVars, initList}],
   controls = Part[specData, All, 3],
   heldFn = PostComposeFunction[fn, InternalHoldForm]},
  Rep[
    ConstructHoldComplete[fn, Sequence @@ specVars],
    HoldC[body_] :>
      Apply[DynamicModule, Hold[
        specSets,
        Labeled[
          If[QuiverGeometryLoader`$CurrentlyLoading, "LOADING", LocatorPane[Dynamic[\[FormalX]], Graphics[
            Dynamic[body, TrackedSymbols :> specVars],
            PlotRange -> 1.1, Frame -> True, FrameTicks -> None, Axes -> None
          ]]],
          Row[{
            Button[Style["\[DownArrow]", Bold], CellPrint @ Cell[BoxData @ ToPrettifiedString @ specVars, "Code"], Appearance -> None],
            "  ", Column[controls], "  ",
            Button[Style["\[DownArrow]", Bold], CellPrint @ Cell[BoxData @ ToPrettifiedString @ (heldFn @@ specVars), "Code"], Appearance -> None]
          }]
        ],
        Initialization :> $PackageInitializer,
        TrackedSymbols :> specVars
      ] /. SET -> Set]
  ]
];

$formals = {\[FormalA], \[FormalB], \[FormalC], \[FormalD], \[FormalE], \[FormalF], \[FormalD]}

toDynSpec[{min_ ? NumericQ, max_ ? NumericQ}, sym_] := {sym, Avg[min, max], Slider[Dynamic @ sym, {min, max}]};
toDynSpec[max_ ? NumericQ, sym_] := toDynSpec[{0, max}, sym];
toDynSpec[list_List, sym_] := {sym, F @ list, RadioButtonBar[Dynamic @ sym, list]};

(* DynamicPointGraphics[3, {Red, Map[Disk[#, .1] &, #]} &] *)

(**************************************************************************************************)

SetHoldFirst[safeDynamicEval];
safeDynamicEval[body_] := Block[{eval, boxes, Print, Echo, EchoSet},
  Catch[
    WithMessageHandler[
      eval = TimeConstrained[body, .2];
      boxes = ToBoxes @ eval
    ,
      throwMessageTag
    ],
    $throwMessageTag
  ]
];

throwMessageTag[_] := Throw[$Failed, $throwMessageTag];

(**************************************************************************************************)

PublicDebuggingFunction[PrintQGStack]

$stackFile := $stackFile = TemporaryPath["stack.m"];

PrintQGStack[label_:None] := StackInhibit @ Block[
  {stack, cells},
  stack = Stack[_];
  Beep[];
  $isFirst = True;
  cells = toStackCell /@ stack;
  CreateDocument[cells, Background -> Lighter[$Blue, 0.95]];
];

(**************************************************************************************************)

SetHoldAllComplete[toStackCell];

toStackCell[HoldForm[e_]] := toStackCell @ e;

toStackCell[tssRhs:(head_Symbol[___])] /; QGSymbolQ[head] := Module[{boxes},
  boxes = clickCopyBox[lhsEchoStr @ tssRhs, Uneval @ tssRhs];
  Cell[
    BoxData @ boxes, "Output",
    ShowCellLabel -> False, ShowStringCharacters -> True,
    CellGroupingRules -> {"ItemGrouping", If[$isFirst, $isFirst = False; 180, 200]}, CellDingbat -> None
  ]
];

toStackCell[_] := Nothing;

(**************************************************************************************************)

SetHoldAllComplete[extractHead];

extractHead[HoldForm[e_]] := extractHead @ e;
extractHead[Catch[_, _, QuiverGeometry`Init`Macros`ThrownMessageHandler[func_Symbol]]] := HoldForm[func];
extractHead[CompoundExpression[first_, Null]] := extractHead @ first;
extractHead[head_Symbol[___]] := If[QGSymbolQ[head], HoldForm[head], Nothing];
extractHead[head_[___]] := extractHead[head];
extractHead[_] := Nothing;

QGSymbolQ[s_Symbol ? HoldAtomQ] := SStartsQ[Context @ Uneval @ s, "QuiverGeometry`"] && HasAnyEvaluationsQ[s];
QGSymbolQ[_] := False;

(**************************************************************************************************)

SetHoldAllComplete[debugStr];

debugStr[lhs_] := ToPrettifiedString[Uneval @ lhs, MaxDepth -> 4, MaxLength -> 24, MaxIndent -> 3]

(**************************************************************************************************)

PublicDebuggingFunction[FindDefinitionsContaining]

FindDefinitionsContaining[context_, pattern_] := Scope[
  symbols = Names[{context <> "*", context <> "**`*"}];
  Select[ContainsQ[pattern]] @ Catenate @ Map[Definitions, symbols]
]

(**************************************************************************************************)

PublicDebuggingFunction[FindMatchingDownValue]

SetHoldAllComplete[FindMatchingDownValue]

FindMatchingDownValue[head_Symbol[args___]] := Block[
  {dvs = DownValues[head], head, res},
  DownValues[head] = App[MapIndexed[{rule, ind} |-> replaceRHS[rule, F[ind]], dvs], HoldP[_head] -> None];
  res = head[args];
  If[res === None, Return @ None];
  If[!IntQ[res], Return[$Failed]];
  Part[dvs, res]
];

replaceRHS[lhs_ :> rhs_, num_] := lhs :> num;
replaceRHS[lhs_ :> (Verbatim[Condition][rhs_, cond_]), num_] := lhs :> Condition[num, cond];
replaceRHS[_] := Nothing;

(**************************************************************************************************)

PublicDebuggingFunction[EchoGraphics]

EchoGraphics[e_] := (AppTo[$prims, e]; e);
EchoGraphics[{x_ ? RealVectorQ, y_ ? RealVectorQ}] := (EchoGraphics @ Trans[x, y]; {x, y});
EchoGraphics[points_ ? RealMatrixQ] := (AppTo[$prims, Point @ points]; points);

(**************************************************************************************************)

PublicDebuggingFunction[EchoDimensions]

EchoDimensions[e_] := (Echo[Row[Dims @ e, "\[Times]", BaseStyle -> $DarkBlue]]; e);

(**************************************************************************************************)

PublicTypesettingForm[MsgExpr]

$msgExprOpts = Sequence[FullSymbolContext -> False, MaxStringLength -> 32, CompressLargeSubexpressions -> False, ElideAtomicHeads -> True, InlineColors -> True, CompactRealNumbers -> True];
MsgExpr[p_MsgPath] := p;
MsgExpr[e_] := ToPrettifiedString[InternalHoldForm @ e, MaxDepth -> 3, MaxLength -> 4, MaxIndent -> 0, $msgExprOpts];
MsgExpr[e_, n_] := ToPrettifiedString[InternalHoldForm @ e, MaxDepth -> n, MaxLength -> 4, MaxIndent -> 0, $msgExprOpts];
MsgExpr[e_, n_, m_] := ToPrettifiedString[InternalHoldForm @ e, MaxDepth -> n, MaxLength -> m, MaxIndent -> 0, $msgExprOpts];

(**************************************************************************************************)

PublicTypesettingForm[MsgPath]

MsgPath[p_MsgFile] := p;
MsgPath[File[p_]] := MsgPath[p];
MsgPath[l_List] := Map[MsgPath, l];

MsgPath /: SystemOpen[MsgPath[s_, n_:None]] := openMsgPath[s, n];

Format[MsgPath[s_Str], OutputForm] := SJoin["\"", s, "\""];
Format[MsgPath[s_Str, n_Int], OutputForm] := SJoin["\"", s, ":", IntStr @ n, "\""];
MakeBoxes[MsgPath[s_Str], StandardForm] := msgPathBoxes[s];
MakeBoxes[MsgPath[s_Str], TraditionalForm] := msgPathBoxes[s];

MakeBoxes[MsgPath[s_Str, n_Int], StandardForm] := msgPathBoxes[s, n];
MakeBoxes[MsgPath[s_Str, n_Int], TraditionalForm] := msgPathBoxes[s, n];

msgPathBoxes[path_Str, line_:None] := With[
  {type = If[SStartsQ[path, ("http" | "https" | "git" | "file" | "ssh") ~~ ":"], "URL", Quiet @ FileType @ path]},
  {color = Switch[type, None, $LightRed, Directory, $LightBlue, File, GrayLevel[0.9], "URL", $LightPurple, _, $LightRed]},
  ToBoxes @ ClickForm[
    RawBoxes @ tightColoredBoxes[If[IntQ[line], SJoin[shortenPath @ path, ":", IntStr @ line], shortenPath @ path], color],
    openMsgPath[path, line]
  ]
];

shortenPath[str_] := Scope[
  str = SRep[str, $HomeDirectory <> $PathnameSeparator -> "~/"];
  If[SLen[str] <= 36, Return @ str];

  n = 0;
  segs = Rev @ FileNameSplit @ str;
  segs2 = Rev @ TakeWhile[segs, (n += SLen[#]) < 36&];
  If[segs2 === {}, segs2 = Take[segs, 1]];
  If[Len[segs2] < Len[segs], PreTo[segs2, "\[Ellipsis]"]];
  str2 = FileNameJoin @ segs2;
  If[SLen[str2] < SLen[str], str2, str]
];

(**************************************************************************************************)

PrivateFunction[openMsgPath]

openMsgPath[path_Str, None] := If[
  ModifierKeysPressedQ[],
  Beep[]; CopyToClipboard @ ToString[path, InputForm],
  trySystemOpen @ path
];

openMsgPath[path_Str, line_Int] :=
  SystemOpen @ FileLine[path, line];

trySystemOpen[s_Str] := Scope[
  If[SStartsQ[s, "http://" | "https://"], Return @ SystemOpen @ s];
  If[FileExistsQ[s],                           Return @ sysOpen @ s];
  If[FileExistsQ[s = FileNameDrop @ s],        Return @ sysOpen @ s];
  If[FileExistsQ[s = FileNameDrop @ s],        Return @ sysOpen @ s];
  If[FileExistsQ[s = FileNameDrop @ s],        Return @ sysOpen @ s];
];

$textFileP = "m" | "wl" | "md" | "txt";

sysOpen[s_Str] := Switch[
  FileExtension[s],
  "nb" | $textFileP, SystemOpen @ FileLine[s, 1],
  "mx",              SetSelectedNotebook @ CreateDocument @ TextCell[ImportMX @ s, "Input"],
  _,                 SystemOpen @ NormalizePath @ s
];

(**************************************************************************************************)

PrivateFunction[tightColoredBoxes]

tightColoredBoxes[str_Str, color_, sz_:10] := ToBoxes @ Framed[
  Style[str, FontFamily -> "Source Code Pro", FontSize -> sz, Bold, FontColor -> Black],
  Background -> color, FrameStyle -> Darker[color, .2],
  ContentPadding -> False, RoundingRadius -> 2,
  ImageSize -> {Auto, 13}, FrameMargins -> {{5, 5}, {0, 0}},
  BaselinePosition -> Baseline
];

(**************************************************************************************************)

PublicFunction[GoodBeep, BadBeep]

If[$MacOSQ,
  GoodBeep[] := Beep[];
  BadBeep[]  := Run["afplay /System/Library/Sounds/Sosumi.aiff&"];
,
  GoodBeep[] := Beep[];
  BadBeep[]  := (Beep[]; Pause[0.1]; Beep[]);
];

(**************************************************************************************************)

PrivateFunction[ModifierKeysPressedQ]

ModifierKeysPressedQ[] := $Notebooks && (CurrentValue["ModifierKeys"] =!= {});

PublicDebuggingFunction[PerformSelfLinting]

PerformSelfLinting[] := Scope[
  Decases[{} | <||>] @ Assoc[
    "MissingPackageScopes" -> findMissingPackageScopes[],
    "SuspiciousPackageLines" -> QuiverGeometryLoader`$SuspiciousPackageLines
  ]
];

findMissingPackageScopes[] := Scope[
  privateSymbols = Names["QuiverGeometry`**`*"];
  privateSymbolNames = L /@ SSplit[privateSymbols, "`"];
  moduleSymbols = Select[Dedup @ privateSymbolNames, SEndsQ["$"]];
  moduleSymbols = Join[moduleSymbols, SDrop[moduleSymbols, -1]];
  privateSymbolAssoc = AssocThread[privateSymbols, privateSymbolNames];
  privateSymbolAssoc //= Select[SLen[#] >= 4&];
  privateSymbolAssoc //= Discard[ElementQ[moduleSymbols]];
  collisionsAssoc = Select[PositionIndex[privateSymbolAssoc], Len[#] > 1&];
  collisionsAssoc //= Select[possibleMissingPackageScope];
  collisionsAssoc
]

possibleMissingPackageScope[names_] :=
  CountDistinct[ToExpression[names, InputForm, HasAnyEvaluationsQ]] > 1;

(**************************************************************************************************)

PublicVariable[$XMLElementFormatting]

SetInitialValue[$XMLElementFormatting, True];

Unprotect[XMLElement];
MakeBoxes[el:XMLElement[_Str, _List, _List], StandardForm] /; TrueQ[$XMLElementFormatting] := xmlElementBoxes[el];
Protect[XMLElement];

xmlStyleBox[b_, c1_, c2_, f_:Bold] :=
  FrameBox[StyleBox[DeployBox @ b, f], Background -> c1, FrameStyle -> c2, Alignment -> Baseline, FrameMargins -> {{5, 5}, {2, 0}}];

xmlLeafCount = Case[
  _ := 1;
  XMLElement[_, _, list_List] := 1 + Total[% /@ list];
];

xmlElementHeadBox[xml:XMLElement[str_Str, attrs_List, content_], showCount_:True] := With[
  {ci = Mod[Hash @ str, 9]},
  {c1 = Part[$LightColorPalette, ci], c2 = Part[$ColorPalette, ci]},
  {lc = xmlLeafCount[xml], ec = Len @ ToList @ content},
  {ls = Which[!showCount, "", lc <= 1, "", ec == lc, lc, True, {ec, "/", lc}]},
  {b1 = xmlStyleBox[str, c1, c2]},
  {b1 = If[ls === "", b1, RowBox[{b1, " ", StyleBox[TextString[Row @ Flatten @ {"\"(", ls, ")\""}], Plain, Smaller, c2]}]]},
  If[attrs === {}, b1,
    NiceTooltipBoxes[b1, ToBoxes[Grid[Transpose[MapFirst[Map[Style[#, Bold]&], KeysValues @ attrs]], Alignment -> Left, Spacings -> {1, .5}]], 1000]
  ]
];

printXML[xml_] := Block[{$XMLElementFormatting = False}, CellPrint @ TextCell[xml, "Input"]];

xmlOpenerBox[xml_, a_, b_List] := With[
  {a1 = ClickBox[a, If[CurrentValue["ShiftKey"], printXML[xml], open$$ = !TrueQ[open$$]]]},
  {a1b = TightColumnGridBox[Pre[b, a1]]},
  DynamicModuleBox[
    {open$$ = 1},
    DynamicBox[
      If[TrueQ @ open$$, a1b, a1],
      TrackedSymbols :> {open$$}
    ],
    DynamicModuleValues -> {open$$}
  ]
];

ClearAll[xmlElementBoxes];

xmlElementBoxes[xml:XMLElement[str_Str, attrs_List, {content_} | content_Str]] := With[
  {b1 = ClickBox[xmlElementHeadBox[xml, False], If[CurrentValue["ShiftKey"], printXML @ xml]]},
  TightRowGridBox[{b1, xmlElementBoxes[content]}]
];

xmlElementBoxes[xml:XMLElement[str_Str, attrs_List, {}]] :=
  ClickBox[xmlElementHeadBox[xml], If[CurrentValue["ShiftKey"], printXML @ xml]];

xmlElementBoxes[xml:XMLElement[str_Str, attrs_List, content_List]] := With[
  {b1 = xmlElementHeadBox[xml]},
  xmlOpenerBox[xml,
    b1,
    Map[TightRowGridBox[{TemplateBox[{5}, "Spacer1"], xmlElementBoxes[#]}]&, content]
  ]
];

xmlElementBoxes[s2_Str] := With[{s = STrim @ s2}, ClickBox[
  xmlStyleBox[ToBoxes @ If[SLen[s] > 20, STake[s, 20] <> "\[Ellipsis]", s], GrayLevel[0.9], $Gray, Plain],
  If[CurrentValue["ShiftKey"], printXML[s]]
]];

xmlElementBoxes[o_] := "?";

(**************************************************************************************************)

PublicDebuggingFunction[CompareGraphics]

CompareGraphics[a_, b_] := Image[ImageApply[toDiffColor, {makeImage[a], makeImage[b]}], Magnification -> 3];

toDiffColor[a_, b_] := Which[a == b, {a, a, a}, a < b, {.9, a, a}, True, {b, .9, b}];

makeImage = Case[
  i_Image := ImageApply[Min, i];
  o_      := ImageApply[Min, MakeImage[o]];
];

(**************************************************************************************************)

PublicDebuggingFunction[SymbolDependancyGraph]

$lastLoadCount = -1;
$currentDependencyGraph = None;

hasEvalQ[HoldC[s_]] := HasAnyEvaluationsQ[s];

SymbolDependancyGraph[] := Scope[
  If[$lastLoadCount === QuiverGeometryLoader`$LoadCount,
    Return @ $currentDependencyGraph];
  $packages = QuiverGeometryLoader`ReadSource[False, True, False];
  $packages = $packages /. _MessageName -> Null;
  definingHeads = _Set | _SetDelayed | _DeclareGraphicsPrimitive;
  positions = Position[$packages, definingHeads];
  symbolNames = Union[Names["QuiverGeometry`*"], Names["QuiverGeometry`Private`*"], Names["QuiverGeometry`**`*"]];
  symbolNames = Comp[symbolNames, {"Case"}, QuiverGeometryLoader`$SymbolGroups["DebuggingFunction"]];
  symbols = ToExpression[symbolNames, InputForm, HoldC];
  symbols = Select[symbols, hasEvalQ];
  symbolsAssoc = UAssoc[# -> True& /@ symbols];
  $symbolExtractR = sym_Symbol /; KeyQ[symbolsAssoc, HoldC[sym]] :> HoldForm[sym];
  edges = Flatten @ Map[processPosition, positions];
  $lastLoadCount ^= QuiverGeometryLoader`$LoadCount;
  $currentDependencyGraph ^= Graph[edges,
    GraphLayout -> None, VertexLabels -> Placed[Auto, Tooltip],
    EdgeShapeFunction -> symbolDependancyEdge
  ]
];

processPosition[pos:{i_, 3, j_, ___}] := Scope[
  file = Part[$packages, i, 1];
  line = Part[$packages, i, 3, j, 1];
  expr = Extract[$packages, pos, HoldC];
  fileLine = FileLine[file, line];
  lhs = Extract[expr, {1, 1}, HoldC];
  rhs = Extract[expr, {1, 2;;}, HoldC];
  defining = DeepCases[lhs, $symbolExtractR];
  If[defining === {}, Return @ Nothing];
  definedBy = DeepCases[rhs, $symbolExtractR];
  If[definedBy === {}, Return @ Nothing];
  DirectedEdge @@@ Tuples[{defining, definedBy, {fileLine}}]
];

symbolDependancyEdge[path_, DirectedEdge[a_, b_, fileLine_]] :=
  drawDependencyEdge[path, a, b, fileLine, 20];

drawDependencyEdge[path_, a_, b_, fileLine_, scale_] :=
  ClickForm[
    Tooltip[
      ExtendedArrow[path,
        Setback -> (8 / scale),
        ArrowheadShape -> "FilledTriangle",
        ArrowheadLength -> (9 / scale),
        ArrowheadPosition -> 0.5,
        ArrowColor -> $Gray,
        ArrowheadOpacity -> 1],
      a -> b
    ],
    SystemOpen @ fileLine
  ];

(**************************************************************************************************)

$intOrInfP = (_Integer | Inf | -Inf);

PublicDebuggingFunction[SymbolDependancies]

SymbolDependancies[sym_Symbol | HoldC[sym_Symbol], n:$intOrInfP] := Scope[
  graph = SymbolDependancyGraph[];
  fn = If[n < 0, VertexInComponent, VertexOutComponent];
  fn[graph, HoldForm[sym], Abs[n]]
]

SymbolDependancyGraph[sym_Symbol | HoldC[sym_Symbol] | HoldForm[sym_Symbol], n:$intOrInfP] := Scope[
  vertices = SymbolDependancies[sym, n];
  subgraph = Subgraph[
    RemoveSelfLoops @ $currentDependencyGraph, vertices,
    VertexLabels -> "Name" -> shortSymbolName,
    VertexLabelStyle -> {FontSize -> 8},
    EdgeShapeFunction -> Fn[drawDependencyEdge[#Coordinates, #Source, #Target, #Cardinal, #GraphicsScale]]
  ];
  ExtendedGraph[
    subgraph,
    GraphicsScale -> 100, VertexSize -> 10,
    VertexTooltips -> "Name" -> longSymbolName,
    VertexColorFunction -> vertexColor
  ]
];

vertexColor[HoldForm[sym_]] := Switch[Context[sym],
  "QuiverGeometry`", $Green,
  "QuiverGeometry`Private`", $Orange,
  _, $Red
];

longSymbolName[HoldForm[sym_]] := SymbolName[Uneval[sym]];
shortSymbolName[HoldForm[sym_]] := shortCamelName @ SymbolName[Uneval[sym]];

shortCamelName[str_] := shortCamelName[str] =
  SJoin @ SCases[str, $shortCamelPatterns]

$shortCamelPatterns = {
  StartOfString ~~ LetterCharacter,
  LowercaseLetter ~~ u:UppercaseLetter :> u,
  DigitCharacter, "$" ~~ LetterCharacter
};

(**************************************************************************************************)

PublicDebuggingFunction[HighlightStringCases]

HighlightStringCases::overlaps = "Cannot display cases that overlap. Some will not be shown.";

HighlightStringCases[str_String, patt:(_Rule | _RuleDelayed)] :=
  HighlightStringCases[str, {patt}];

HighlightStringCases[str_String, patts_List] := Scope[
  {spans, payloads} = KeysValues @ Flatten[i = 1; getPosAndPayload[i++, str, #]& /@ patts];
  If[spans === {}, Return @ str];
  parts = clarifyMatch /@ STake[str, spans];
  highlights = ZipMap[highlightStrMatch, parts, payloads];
  SRepPart[str, highlights, spans]
];

getPosAndPayload[i_, str_, patt_] := Scope[
  pos = SFind[str, patt];
  Thread[pos -> Part[$ColorPalette, i]]
];

getPosAndPayload[i_, str_, patt:(_Rule | _RuleDelayed)] := Scope[
  pos = SFind[str, F @ patt];
  pay = VectorReplace[
    SCases[str, patt],
    m:Except[_Style | $ColorPattern] :> Style[m, Part[$ColorPalette, i]]
  ];
  RuleThread[pos, pay]
];

HighlightStringCases[str_String, patt_] := Scope[
  pos = SFind[str, patt, Overlaps -> False];
  If[pos === {}, Return @ str];
  parts = clarifyMatch /@ STake[str, pos];
  colors = PadRight[$ColorPalette, Len @ pos, $Red];
  highlights = ZipMap[highlightStrMatch, parts, colors];
  SRepPart[str, highlights, pos]
];

(**************************************************************************************************)

clarifyMatch[""] := "▮";
clarifyMatch[""] := "\[NegativeMediumSpace]\[VerticalSeparator]\[NegativeMediumSpace]";

$clarificationRules = {"\n" -> "\[ReturnIndicator]\n", " " -> "\[SpaceIndicator]", "\t" -> "\[TabKey]"};
clarifyMatch[str_Str] := SRep[str, $clarificationRules];

highlightStrMatch[match_Str, color:$ColorPattern] :=
  ToLinearSyntax @ Style[match, color, Bold];

highlightStrMatch[match_Str, Style[payload_, col:$ColorPattern]] :=
  ToLinearSyntax @ Tooltip[Style[match, col], payload];

highlightStrMatch[match_Str, payload_] :=
  ToLinearSyntax @ Tooltip[match, payload];


