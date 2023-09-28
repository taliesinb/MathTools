PublicFunction[UpdateQuiverGeometryStylesheet]

UpdateQuiverGeometryStylesheet::open = "Could not open existing stylesheet at ``.";
UpdateQuiverGeometryStylesheet::replace = "Could not replace existing stylesheet at ``.";
UpdateQuiverGeometryStylesheet::save = "Could not save existing stylesheet at ``.";

UpdateQuiverGeometryStylesheet[] := Scope[
  template = Get @ LocalPath["StyleSheets", "QuiverGeometry.template.nb"];
  template = DeleteCases[template, ExpressionUUID -> _, {0, Infinity}];
  cells = Join[
    KeyValueMap[makeTemplateBoxStyleCell, $notebookDisplayFunction],
    generateNotebookColorPaletteStyles[$ColorPalette, Map[OklabLighter[#, 0.1]&] @ $LightColorPalette]
  ];
  template //= ReplaceAll[Cell[StyleData["Dummy"], ___] :> Splice[cells]];

  hash = Base36Hash[template];
  targetPath = LocalPath["StyleSheets", "QuiverGeometry-" <> hash <> ".nb"];
  If[!FileExistsQ[targetPath],
    Export[targetPath, template];
    Put[targetPath, $latestPathFile];
  ];

  If[!FileExistsQ[$QuiverGeometryStylesheetPath],
    NotebookSave[template, $QuiverGeometryStylesheetPath];
    Return[]];

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
  cells = KeyValueMap[makeTemplateBoxStyleCell, $notebookDisplayFunction];
  template //= ReplaceAll[Cell[StyleData["Dummy"], ___] :> Splice[cells]];
  template
];

PrivateFunction[makeTemplateBoxStyleCell]

makeTemplateBoxStyleCell[name_, fn_] := With[
  {base = Lookup[$notebookDisplayFunctionBases, name, "QuiverGeometryBase"]},
  Cell[
    StyleData[name, StyleDefinitions -> StyleData[base]],
    TemplateBoxOptions -> {DisplayFunction -> fn}
  ]
];

(**************************************************************************************************)

PublicVariable[$QuiverGeometryStylesheetPath]

$latestPathFile = LocalPath["StyleSheets", "latest.m"];
$QuiverGeometryStylesheetPath := $QuiverGeometryStylesheetPath = Check[Import @ $latestPathFile, None];
$QuiverGeometryStylesheetPath = LocalPath["StyleSheets", "QuiverGeometry.nb"];

(**************************************************************************************************)

PublicFunction[ApplyPrivateQuiverGeometryNotebookStyles]

ApplyPrivateQuiverGeometryNotebookStyles[] := (
  SetOptions[EvaluationNotebook[],
    StyleDefinitions -> GeneratePrivateQuiverGeometryStylesheet[],
    DockedCells -> None
  ];
);

(**************************************************************************************************)

generateNotebookColorPaletteStyles[palette_List, lightPalette_List] := Join[
  MapIndex1[Cell[StyleData["Color" <> IntegerString[#2]], FontColor -> #1]&, palette],
  MapIndex1[Cell[StyleData["Background" <> IntegerString[#2]], Background -> #1]&, lightPalette]
];

(**************************************************************************************************)

PublicFunction[UpdateLegacyNotebook]

createLegacyReplacementRules[] := Scope[
  keys = Select[Keys @ $notebookDisplayFunction, LowerCaseQ[StringTake[#, 1]]&];
  symbolRules = Map[
    UpperCaseFirst[#] -> #&,
    Select[Keys @ $notebookDisplayFunction, LowerCaseQ[StringTake[#, 1]]&]
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
PublicFunction[NotebookTemplateNames]
PublicFunction[FindMissingTemplateBoxDefinitions]

(* $defaultStyleSheetStyleNames := $defaultStyleSheetStyleNames =
  getStyleDataNames @ System`Convert`CommonDump`GetStyleSheet["Core.nb"];
 *)

NotebookTemplateNames[nb_:Automatic] :=
  DeepUniqueCases[ToNotebookExpression @ nb, TemplateBox[_, name_String] :> name];

NotebookStyleDataNames[nb_:Automatic] :=
  DeepUniqueCases[ToNotebookExpression @ nb, Cell[StyleData[style_String, ___], ___] :> style];

$builtinTemplateNames = {"RowWithSeparators", "RowWithSeparator", "Spacer1", "Ceiling", "Floor"};

FindMissingTemplateBoxDefinitions[] := FindMissingTemplateBoxDefinitions @ Automatic;
FindMissingTemplateBoxDefinitions[nb_, ref_:Automatic] := Scope[
  SetAutomatic[ref, $QuiverGeometryStylesheetPath];
  availableNames = Join[NotebookStyleDataNames @ ref, $builtinTemplateNames];
  notebookNames = NotebookTemplateNames @ nb;
  Complement[notebookNames, availableNames]
];

(**************************************************************************************************)

PublicFunction[QuiverGeometryStyleNames]

QuiverGeometryStyleNames[] := NotebookStyleDataNames[$QuiverGeometryStylesheetPath];

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
    StyleDefinitions -> $QuiverGeometryStylesheetPath
  ];

