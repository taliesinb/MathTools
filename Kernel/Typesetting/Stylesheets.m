PublicFunction[UpdateQuiverGeometryStylesheet]

UpdateQuiverGeometryStylesheet::open = "Could not open existing stylesheet at ``.";
UpdateQuiverGeometryStylesheet::replace = "Could not replace existing stylesheet at ``.";
UpdateQuiverGeometryStylesheet::save = "Could not save existing stylesheet at ``.";

UpdateQuiverGeometryStylesheet[] := Scope[
  template = Get @ LocalPath["StyleSheets", "QuiverGeometry.template.nb"];
  template = DeleteCases[template, ExpressionUUID -> _, {0, Infinity}];
  cells = KeyValueMap[makeTemplateBoxStyleCell, $templateBoxDisplayFunction];
  template //= ReplaceAll[Cell[StyleData["Dummy"], ___] :> Splice[cells]];

  hash = Base36Hash[template];
  targetPath = LocalPath["StyleSheets", "QuiverGeometry-" <> hash <> ".nb"];
  If[!FileExistsQ[targetPath],
    Export[targetPath, template];
    Put[targetPath, $latestPathFile];
  ];

  nb = NotebookOpen[$QuiverGeometryStylesheetPath, Visible -> True];
  If[Head[nb] =!= NotebookObject, ReturnFailed["open", $QuiverGeometryStylesheetPath]];
  If[FailureQ[NotebookPut[template, nb]], ReturnFailed["replace", $QuiverGeometryStylesheetPath]];
  If[FailureQ[NotebookSave[nb]], ReturnFailed["save", $QuiverGeometryStylesheetPath]];
  (* NotebookClose[nb]; *)
];

PrivateFunction[GeneratePrivateQuiverGeometryStylesheet]

GeneratePrivateQuiverGeometryStylesheet[] := Scope[
 template = Notebook[{
      Cell[StyleData[StyleDefinitions -> $QuiverGeometryStylesheetPath]],
      Cell[StyleData["Dummy"]]
    },
    FrontEndVersion -> "13.1 for Mac OS X x86 (64-bit) (June 16, 2022)",
    StyleDefinitions -> "PrivateStylesheetFormatting.nb"
  ];
  cells = KeyValueMap[makeTemplateBoxStyleCell, $templateBoxDisplayFunction];
  template //= ReplaceAll[Cell[StyleData["Dummy"], ___] :> Splice[cells]];
  template
];

PrivateFunction[makeTemplateBoxStyleCell]

makeTemplateBoxStyleCell[name_, fn_] := Cell[
  StyleData[name, StyleDefinitions -> StyleData["QuiverGeometryBase"]],
  TemplateBoxOptions -> {DisplayFunction -> fn}
];



(**************************************************************************************************)

PublicVariable[$QuiverGeometryStylesheetPath]

$latestPathFile = LocalPath["StyleSheets", "latest.m"];
$QuiverGeometryStylesheetPath := $QuiverGeometryStylesheetPath = Check[Import @ $latestPathFile, None];
$QuiverGeometryStylesheetPath = LocalPath["StyleSheets", "QuiverGeometry.nb"];

(**************************************************************************************************)

PublicFunction[ApplyQuiverGeometryNotebookStyles]

ApplyQuiverGeometryNotebookStyles[] :=
  ApplyQuiverGeometryNotebookStyles @ EvaluationNotebook[];

ApplyQuiverGeometryNotebookStyles[nb_NotebookObject] := (SetOptions[nb,
  StyleDefinitions -> $QuiverGeometryStylesheetPath,
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
  keys = Select[Keys @ $templateBoxDisplayFunction, LowerCaseQ[StringTake[#, 1]]&];
  symbolRules = Map[
    UpperCaseFirst[#] -> #&,
    Select[Keys @ $templateBoxDisplayFunction, LowerCaseQ[StringTake[#, 1]]&]
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
  AppendTo[nb, StyleDefinitions -> $QuiverGeometryStylesheetPath];
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
  SetAutomatic[ref, $QuiverGeometryStylesheetPath];
  availableNames = Join[NotebookStyleDataNames @ ref, $builtinTemplateBoxNames];
  notebookNames = NotebookTemplateBoxNames @ nb;
  Complement[notebookNames, availableNames]
];

(**************************************************************************************************)

PublicFunction[ApplyQuiverGeometryStylesheet]

ApplyQuiverGeometryStylesheet[] := (
  SetOptions[EvaluationNotebook[], StyleDefinitions -> $QuiverGeometryStylesheetPath];
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
    StyleDefinitions -> $QuiverGeometryStylesheetPath,
    DockedCells -> createQGNotebookDockedCells[]
  ];

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
    " QG fast "         :> Module[{res = Check[QuiverGeometryPackageLoader`Load[False], $Failed]}, If[res =!= $Failed, Beep[], Pause[0.1]; Beep[]; Pause[0.1]; Beep[]; SetSelectedNotebook @ First @ Notebooks["Messages"]]],
    None,
    " styles "          :> (getQG[]; RunAsyncWithMessagePopup @ UpdateQuiverGeometryStylesheet[])
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
