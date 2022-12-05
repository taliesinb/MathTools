PrivateFunction[VPrint]

SetHoldAllComplete[VPrint];
VPrint[args___] /; TrueQ[$verbose] :=
  Print[If[TrueQ[$dryRun], Style["> ", LightGray], ""], args];

(**************************************************************************************************)

PublicFunction[EchoEdgeList]

EchoEdgeList = EchoFunction[EdgeList]

(**************************************************************************************************)

PublicFunction[EchoGraphicsScope, EchoGraphics]

SetHoldAllComplete[EchoGraphicsScope];
EchoGraphicsScope[e_] := Scope[
  $prims = {};
  res = e;
  n = Length[$prims]; $i = 1;
  Print @ Graphics[
    Map[{Opacity[$i++ / n], #}&, $prims],
    Frame -> True, PlotRangePadding -> Scaled[0.1]
  ];
  res
];

(**************************************************************************************************)

PublicFunction[DynamicPointGraphics]

DynamicPointGraphics[n_Integer, fn_] := Replace[
  ConstructHoldComplete[fn, \[FormalX]],
  HoldComplete[body_] :>
    DynamicModule @@ Hold[
      {\[FormalX] = CirclePoints[n]},
      LocatorPane[Dynamic[\[FormalX]], Graphics[Dynamic @ body, PlotRange -> 1.1, Frame -> True, FrameTicks -> None, Axes -> None]],
      Initialization :> $PackageInitializer
    ]
];

(* DynamicPointGraphics[{n_Integer, {min_, max_}}, fn_] := Replace[
  ConstructHoldComplete[fn, \[FormalX], \[FormalY]],
  HoldComplete[body_] :>
    DynamicModule @@ Hold[
      {\[FormalX] = CirclePoints[n], \[FormalY] = Avg[min, max]},
      Labeled[
      LocatorPane[Dynamic[\[FormalX]],
        Graphics[
          Dynamic[body, TrackedSymbols :> {\[FormalX], \[FormalY]}],
          PlotRange -> 1.1, Frame -> True, FrameTicks -> None, Axes -> None
        ]
      ], Slider[Dynamic @ \[FormalY], {min, max}]]
    ]
];
 *)
DynamicPointGraphics[{n_Integer, specSeq__}, fn_] := With[
  {specList = {specSeq}},
  {specData = MapThread[toDynSpec, {{specSeq}, Take[$formals, Length @ specList]}]},
  {specVars = Prepend[\[FormalX]] @ Part[specData, All, 1],
   initList = Prepend[CirclePoints[n]] @ Part[specData, All, 2],
   controls = Part[specData, All, 3]},
  {specSets = MapThread[SET, {specVars, initList}]},
  Replace[
    ConstructHoldComplete[fn, Sequence @@ specVars],
    HoldComplete[body_] :>
      Apply[DynamicModule, Hold[
        specSets,
        Labeled[
          LocatorPane[Dynamic[\[FormalX]], Graphics[
            Dynamic[body, TrackedSymbols :> specVars],
            PlotRange -> 1.1, Frame -> True, FrameTicks -> None, Axes -> None
          ]],
          Column[controls]
        ],
        Initialization :> $PackageInitializer
      ] /. SET -> Set]
  ]
];

$formals = {\[FormalA], \[FormalB], \[FormalC], \[FormalD], \[FormalE], \[FormalF], \[FormalD]}

toDynSpec[{min_ ? NumericQ, max_ ? NumericQ}, sym_] := {sym, Avg[min, max], Slider[Dynamic @ sym, {min, max}]};
toDynSpec[max_ ? NumericQ, sym_] := toDynSpec[{0, max}, sym];
toDynSpec[list_List, sym_] := {sym, First @ list, RadioButtonBar[Dynamic @ sym, list]};

(* DynamicPointGraphics[3, {Red, Map[Disk[#, .1] &, #]} &] *)

(**************************************************************************************************)

PublicFunction[PrintQGStack]

$stackFile = FileNameJoin[{$TemporaryDirectory, "qg_stack.m"}];

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
  boxes = clickCopyBox[lhsEchoStr @ tssRhs, Unevaluated @ tssRhs];
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

QGSymbolQ[s_Symbol ? Developer`HoldAtomQ] := StringStartsQ[Context @ Unevaluated @ s, "QuiverGeometry`"] && System`Private`HasAnyEvaluationsQ[s];
QGSymbolQ[_] := False;

(**************************************************************************************************)

SetHoldAllComplete[debugStr];

debugStr[lhs_] := ToPrettifiedString[Unevaluated @ lhs, MaxDepth -> 4, MaxLength -> 24, MaxIndent -> 3]

(**************************************************************************************************)

PublicFunction[FindMatchingDownValue]

SetHoldAllComplete[FindMatchingDownValue]

FindMatchingDownValue[head_Symbol[args___]] := Block[
  {dvs = DownValues[head], head, res},
  DownValues[head] = Append[MapIndexed[{rule, ind} |-> replaceRHS[rule, First[ind]], dvs], HoldPattern[_head] -> None];
  res = head[args];
  If[res === None, Return @ None];
  If[!IntegerQ[res], Return[$Failed]];
  Print[res];
  Part[dvs, res]
];

replaceRHS[lhs_ :> rhs_, num_] := lhs :> num;
replaceRHS[lhs_ :> (Verbatim[Condition][rhs_, cond_]), num_] := lhs :> Condition[num, cond];
replaceRHS[_] := Nothing;

(**************************************************************************************************)

PublicFunction[EchoGraphics]

EchoGraphics[e_] := (AppendTo[$prims, e]; e);
EchoGraphics[{x_ ? RealVectorQ, y_ ? RealVectorQ}] := (EchoGraphics @ Trans[x, y]; {x, y});
EchoGraphics[points_ ? RealMatrixQ] := (AppendTo[$prims, Point @ points]; points);

(**************************************************************************************************)

PublicFunction[EchoDimensions]

EchoDimensions[e_] := (Echo[Row[Dimensions @ e, "\[Times]", BaseStyle -> $DarkBlue]]; e);

(**************************************************************************************************)

PublicHead[MsgExpr]

MsgExpr[p_MsgPath] := p;
MsgExpr[e_] := ToPrettifiedString[e, MaxDepth -> 3, MaxLength -> 4, MaxIndent -> 0];
MsgExpr[e_, n_] := ToPrettifiedString[e, MaxDepth -> n, MaxLength -> 4, MaxIndent -> 0];

(**************************************************************************************************)

PublicHead[MsgPath]

MsgPath[p_MsgFile] := p;
MsgPath[File[p_]] := MsgPath[p];
MsgPath[l_List] := Map[MsgPath, l];

MakeBoxes[MsgPath[s_String], StandardForm] := msgPathBoxes[s];
MakeBoxes[MsgPath[s_String], TraditionalForm] := msgPathBoxes[s];

msgPathBoxes[path_String] := With[
  {type = If[StringStartsQ[path, ("http" | "https" | "git" | "file" | "ssh") ~~ ":"], "URL", Quiet @ FileType @ path]},
  {color = Switch[type, None, $LightRed, Directory, $LightBlue, File, $LightGray, "URL", $LightPurple, _, $LightRed]},
  ToBoxes @ ClickForm[
    tightColoredBoxes[path, color],
    If[
      ModifierKeysPressedQ[],
      trySystemOpen @ path,
      Beep[]; CopyToClipboard @ ToString[path, InputForm]
    ]
  ]
];

trySystemOpen[s_String] := Scope[
  If[FileExistsQ[s],                    Return @ sysOpen @ s];
  If[FileExistsQ[s = FileNameDrop @ s], Return @ sysOpen @ s];
  If[FileExistsQ[s = FileNameDrop @ s], Return @ sysOpen @ s];
];

sysOpen[s_String] := Switch[
  FileExtension[s],
  "nb",      SetSelectedNotebook @ NotebookOpen[s, Visible -> True],
  _,         SystemOpen @ NormalizePath @ s
];

(**************************************************************************************************)

PrivateFunction[tightColoredBoxes]

tightColoredBoxes[str_String, color_] := Framed[
  Style[str, FontFamily -> "Source Code Pro", FontSize -> 10, Bold, FontColor -> Black],
  Background -> color, FrameStyle -> Darker[color, .2],
  ContentPadding -> False, RoundingRadius -> 2,
  ImageSize -> {Automatic, 16}, FrameMargins -> {{5, 5}, {0, 0}},
  BaselinePosition -> Baseline
];

(**************************************************************************************************)

PublicFunction[GoodBeep, BadBeep]

If[$OperatingSystem === "MacOSX",
GoodBeep[] := Beep[];
BadBeep[] := Run["afplay /System/Library/Sounds/Sosumi.aiff&"];
,
GoodBeep[] := Beep[];
BadBeep[] := (Beep[]; Pause[0.1]; Beep[]);
];

(**************************************************************************************************)

PrivateFunction[ModifierKeysPressedQ]

ModifierKeysPressedQ[] := $Notebooks && (CurrentValue["ModifierKeys"] =!= {});

PublicFunction[PerformSelfLinting]

PerformSelfLinting[] := Scope[
  DeleteCases[{} | <||>] @ Association[
    "MissingPackageScopes" -> findMissingPackageScopes[],
    "SuspiciousPackageLines" -> QuiverGeometryPackageLoader`$SuspiciousPackageLines
  ]
];

findMissingPackageScopes[] := Scope[
  privateSymbols = Names["QuiverGeometry`**`*"];
  privateSymbolNames = Last /@ StringSplit[privateSymbols, "`"];
  moduleSymbols = Select[DeleteDuplicates @ privateSymbolNames, StringEndsQ["$"]];
  moduleSymbols = Join[moduleSymbols, StringDrop[moduleSymbols, -1]];
  privateSymbolAssoc = AssociationThread[privateSymbols, privateSymbolNames];
  privateSymbolAssoc //= Select[StringLength[#] >= 4&];
  privateSymbolAssoc //= Discard[ElementQ[moduleSymbols]];
  collisionsAssoc = Select[PositionIndex[privateSymbolAssoc], Length[#] > 1&];
  collisionsAssoc //= Select[possibleMissingPackageScope];
  collisionsAssoc
]

possibleMissingPackageScope[names_] :=
  CountDistinct[ToExpression[names, InputForm, System`Private`HasAnyEvaluationsQ]] > 1;

