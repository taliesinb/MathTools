PublicFunction[GenerateStylesheet]

GenerateStylesheet[] := Scope[
  template = Get @ FileNameJoin[{$StylesheetDirectory, "QuiverGeometry.template.nb"}];
  template = DeleteCases[template, ExpressionUUID -> _, {0, Infinity}];
  cells = KeyValueMap[makeStyleCell, $templateBoxFunctions];
  template //= ReplaceAll[Cell[StyleData["Dummy"], ___] :> Splice[cells]];

  hash = Base36Hash[template];
  targetPath = FileNameJoin[{$StylesheetDirectory, "QuiverGeometry-" <> hash <> ".nb"}];
  If[FileExistsQ[targetPath], Return[$StylesheetPath ^= targetPath]];

  Export[targetPath, template];
  Put[targetPath, $latestPathFile];

  $StylesheetPath ^= targetPath
];

makeStyleCell[name_, fn_] := Cell[
  StyleData[name, StyleDefinitions -> StyleData["QuiverGeometryBase"]],
  TemplateBoxOptions -> {DisplayFunction -> fn}
];

(**************************************************************************************************)

PublicVariable[$StylesheetDirectory]
PublicVariable[$StylesheetPath]

$StylesheetDirectory = FileNameJoin[{ParentDirectory @ QuiverGeometryPackageLoader`$Directory, "StyleSheets"}];
$latestPathFile = FileNameJoin[{$StylesheetDirectory, "latest.m"}];
$StylesheetPath := $StylesheetPath = Quiet @ Check[Import @ $latestPathFile, None];

(**************************************************************************************************)

PublicFunction[ApplyQuiverGeometryNotebookStyles]

ApplyQuiverGeometryNotebookStyles[] :=
  ApplyQuiverGeometryNotebookStyles @ EvaluationNotebook[];

ApplyQuiverGeometryNotebookStyles[nb_NotebookObject] := (SetOptions[nb,
  StyleDefinitions -> $StylesheetPath,
  DockedCells -> createQGNotebookDockedCells[]
]; nb);

ApplyQuiverGeometryNotebookStyles[dir_String ? DirectoryQ] :=
  Map[ApplyQuiverGeometryNotebookStyles, Select[StringFreeQ["XXX"]] @ FileNames["*.nb", dir, Infinity]];

ApplyQuiverGeometryNotebookStyles[file_String ? FileQ] := Scope[
  nb = NotebookOpen[file, Visible -> False];
  ApplyQuiverGeometryNotebookStyles[nb];
  SetOptions[nb, Visible -> True];
  NotebookSave[nb];
  NotebookClose[nb];
  file
];

(**************************************************************************************************)

PublicVariable[UpdateLegacyNotebook]

createLegacyReplacementRules[] := Scope[
  keys = Select[Keys @ $templateBoxFunctions, LowerCaseQ[StringTake[#, 1]]&];
  symbolRules = Map[
    UpperCaseFirst[#] -> #&,
    Select[Keys @ $templateBoxFunctions, LowerCaseQ[StringTake[#, 1]]&]
  ];
  colorRules = {
    "RedGreenForm" -> "TealForm", "RedBlueForm" -> "PinkForm", "GreenBlueForm" -> "OrangeForm"
  };
  colorRules = {
    colorRules,
    colorRules /. s_String :> StringJoin["Light", s],
    colorRules /. s_String :> StringJoin["Dark", s]
  };
  Map[toTemplateRule, Flatten @ {symbolRules, colorRules}]
];

toTemplateRule[l_String -> r_String] :=
  TemplateBox[z_, l] (* /; (Print["Replacing ", l -> r]; True) *) :> TemplateBox[z, r];

$legacyReplacementRules := $legacyReplacementRules = createLegacyReplacementRules[];

UpdateLegacyNotebook[] := UpdateLegacyNotebook @ EvaluationNotebook[];

UpdateLegacyNotebook[nb_NotebookObject] := Scope[
  content = NotebookGet[nb];
  result = UpdateLegacyNotebook[content];
  NotebookPut[result, nb]
];

UpdateLegacyNotebook[nb_Notebook] := Scope[
  nb = DeleteCases[nb, StyleDefinitions -> _];
  AppendTo[nb, StyleDefinitions -> $StylesheetPath];
  ReplaceRepeated[nb, $legacyReplacementRules]
];

UpdateLegacyNotebook::fail = "Could not update ``."
UpdateLegacyNotebook[path_String] /; StringEndsQ[path, ".nb"] := Scope[
  Quiet @ CopyFile[path, path <> ".backup"];
  nb = NotebookOpen[path, Visible -> False];
  UpdateLegacyNotebook @ nb;
  ApplyQuiverGeometryNotebookStyles @ nb;
  SetOptions[nb, Visible -> True];
  NotebookSave @ nb;
  NotebookClose @ nb;
  path
];

UpdateLegacyNotebook[dir_String] /; FileType[dir] === Directory :=
  Map[UpdateLegacyNotebook, FileNames["*.nb", dir, Infinity]];

(**************************************************************************************************)

PublicFunction[NotebookStyleDataNames]
PublicFunction[NotebookTemplateBoxNames]
PublicFunction[FindMissingTemplateBoxDefinitions]

(* $defaultStyleSheetStyleNames := $defaultStyleSheetStyleNames =
  getStyleDataNames @ System`Convert`CommonDump`GetStyleSheet["Core.nb"];
 *)

NotebookTemplateBoxNames[nb_:Automatic] :=
  DeepUniqueCases[ToNotebookExpression @ nb, TemplateBox[_, name_String] :> name];

NotebookStyleDataNames[nb_:Automatic] :=
  DeepUniqueCases[ToNotebookExpression @ nb, Cell[StyleData[style_String, ___], ___] :> style];

$builtinTemplateBoxNames = {"RowWithSeparators", "RowWithSeparator", "Spacer1", "Ceiling", "Floor"};

FindMissingTemplateBoxDefinitions[] := FindMissingTemplateBoxDefinitions @ Automatic;
FindMissingTemplateBoxDefinitions[nb_, ref_:Automatic] := Scope[
  SetAutomatic[ref, $StylesheetPath];
  availableNames = Join[NotebookStyleDataNames @ ref, $builtinTemplateBoxNames];
  notebookNames = NotebookTemplateBoxNames @ nb;
  Complement[notebookNames, availableNames]
];

PublicFunction[SetQuiverGeometryStylesheet]

SetQuiverGeometryStylesheet[] := (
  SetOptions[EvaluationNotebook[], StyleDefinitions -> $StylesheetPath];
);

(**************************************************************************************************)

PublicFunction[CreateQuiverGeometryNotebook]

CreateQuiverGeometryNotebook[] :=
  CreateDocument[
    {Cell["Title", "Title"],
     Cell["Chapter", "Chapter"],
     Cell["Section", "Section"],
     Cell["Lorem ipsum dolor sit amet.", "Text"],
     Cell["Graphics[Disk[], ImageSize -> 50]", "Code"],
     Cell["Subsection", "Subsection"],
     Cell["Item 1", "Item"],
     Cell["Item 2", "Item"]},
    StyleDefinitions -> $StylesheetPath,
    DockedCells -> createQGNotebookDockedCells[]
  ];

If[!TrueQ[QuiverGeometryPackageLoader`$menuModified],
With[{qgPath = QuiverGeometryPackageLoader`$initFile},
  LinkWrite[$ParentLink, FrontEnd`AddMenuCommands["MenuListSaveItems", {
    MenuItem[
      "&Save and Export",
      KernelExecute[QuiverGeometry`QGNotebookSaveAndExport[]],
      FrontEnd`MenuKey["s", FrontEnd`Modifiers -> {"Command", "Option"}],
      MenuEvaluator -> Automatic, Method -> "Queued"
    ],
    MenuItem[
      "Load QG",
      KernelExecute[Get[qgPath]],
      FrontEnd`MenuKey["g", FrontEnd`Modifiers -> {"Command", "Option"}],
      MenuEvaluator -> Automatic, Method -> "Queued"
    ],
    MenuItem[
      "Fast load QG",
      KernelExecute[QuiverGeometryPackageLoader`Load[False]],
      FrontEnd`MenuKey["g", FrontEnd`Modifiers -> {"Command", "Option", "Shift"}],
      MenuEvaluator -> Automatic, Method -> "Queued"
    ]
    
  }]];
  QuiverGeometryPackageLoader`$menuModified = True;
]];

createQGNotebookDockedCells[] := With[
  {qgPath = QuiverGeometryPackageLoader`$initFile},
  {getQG = Function[If[DownValues[QuiverGeometryPackageLoader`Load] === {}, Get[qgPath]]]},
  {buttons = Map[makeButton, {
    " open: ",
    " md "              :> (getQG[]; RunAsyncWithMessagePopup @ rebuildCurrentPageThen @ SystemOpen[QGSiteMarkdownPath[]]),
    " web "             :> (getQG[]; RunAsyncWithMessagePopup @ rebuildCurrentPageThen @ OpenQGSiteWebpageURL[]),
    " iA "              :> (getQG[]; RunAsyncWithMessagePopup @ PreviewInIAWriter @ SelectedCellGroup[]),
    " \[UpArrow] "      :> (getQG[]; ExtendCellSelection[]),
    " site: ",
    " clear "           :> (getQG[]; RunAsyncWithMessagePopup @ ClearQGSite[]),
    " deploy "          :> (getQG[]; RunAsyncWithMessagePopup @ DeployQGSite[]),
    " load: ",
    " QG "              :> Module[{res = Check[Get[qgPath], $Failed]}, If[res =!= $Failed, ApplyQuiverGeometryNotebookStyles[]; Beep[], Pause[0.1]; Beep[]; Pause[0.1]; Beep[]; SetSelectedNotebook @ First @ Notebooks["Messages"]]],
    " QG fast "         :> Module[{res = Check[QuiverGeometryPackageLoader`Load[False], $Failed]}, If[res =!= $Failed, Beep[], Pause[0.1]; Beep[]; Pause[0.1]; Beep[]; SetSelectedNotebook @ First @ Notebooks["Messages"]]]
  }]},
  Cell[BoxData @ ToBoxes @ Row[buttons, "   "], "DockedCell"]
];

makeButton[None] := Spacer[5];

makeButton[txt_String] := Style[txt, "Text"];

makeButton[txt_ :> code_] := Button[
  Style[" " <> txt <> " ", White],
  code,
  Appearance -> FrontEndResource["FEExpressions", "OrangeButtonNinePatchAppearance"]
]

SetHoldFirst[rebuildCurrentPageThen];
rebuildCurrentPageThen[body_] := Scope[
  NotebookSave[];
  res = BuildQGSite[EvaluationNotebook[], Verbose -> "KeyModifiers", NotebookCaching -> False];
  If[FailureQ[res], ReturnFailed[]];
  body
];

SetHoldRest[chainNotFailed];
chainNotFailed[$Failed, b_] := $Failed
chainNotFailed[a_, b_] := b;

(**************************************************************************************************)

PublicFunction["RunAsyncWithMessagePopup"]

SetHoldFirst[RunAsyncWithMessagePopup];

RunAsyncWithMessagePopup[body_] := Block[{res, msg},
  msg = First @ Notebooks["Messages"];
  Quiet @ NotebookDelete @ Cells[msg];
  res = Check[body, $Failed];
  If[ModifierKeysPressedQ[], Print[res]];
  If[FailureQ[res],
    BadBeep[];
    Quiet @ SetSelectedNotebook @ msg
  ,
    GoodBeep[];
  ];
  res
];
