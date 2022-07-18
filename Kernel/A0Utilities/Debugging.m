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

PublicFunction[EchoGraphics]

EchoGraphics[e_] := (AppendTo[$prims, e]; e);
EchoGraphics[{x_ ? RealVectorQ, y_ ? RealVectorQ}] := (EchoGraphics @ Trans[x, y]; {x, y});
EchoGraphics[points_ ? RealMatrixQ] := (AppendTo[$prims, Point @ points]; points);

(**************************************************************************************************)

PublicHead[MsgPath]

MsgPath[p_MsgFile] := p;
MsgPath[File[p_]] := MsgPath[p];
MsgPath[l_List] := Map[MsgPath, l];

MakeBoxes[MsgPath[s_String], StandardForm] := msgPathBoxes[s];
MakeBoxes[MsgPath[s_String], TraditionalForm] := msgPathBoxes[s];

msgPathBoxes[path_String] := With[
  {type = FileType[path]},
  {color = Switch[Quiet @ FileType @ path, None, $LightRed, Directory, $LightBlue, File, $LightGray, True, $LightRed]},
  ToBoxes @ MouseAppearance[EventHandler[
    Framed[Style[path, FontFamily -> "Source Code Pro", FontSize -> 10, Bold, FontColor -> Black],
      Background -> color, FrameStyle -> Darker[color, .2],
      ContentPadding -> False, RoundingRadius -> 2,
      ImageSize -> {Automatic, 16}, FrameMargins -> {{5, 5}, {0, 0}},
      BaselinePosition -> Baseline
    ],
    {"MouseClicked" :> If[
      ModifierKeysPressedQ[],
      trySystemOpen @ path,
      Beep[]; CopyToClipboard @ ToString[path, InputForm]
    ]}
  ], "LinkHand"]
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

PublicFunction[WebpageOpen]

(* works best if you have "Fast Duplicate Tab Closer" extension installed *)
WebpageOpen[s_String] := Run["open -a \"Google Chrome.app\" " <> s];

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