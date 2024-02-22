PublicVariable[$DarkStylesheetPath, $LightStylesheetPath, $StylesheetPath]

$DarkStylesheetPath = LocalPath["StyleSheets", "QuiverGeometryDark.nb"];
$LightStylesheetPath = LocalPath["StyleSheets", "QuiverGeometryLight.nb"];
$StylesheetPath = $DarkStylesheetPath;

(**************************************************************************************************)

PublicFunction[UpdateQuiverGeometryStylesheet]

UpdateQuiverGeometryStylesheet::open = "Could not open existing stylesheet at ``.";
UpdateQuiverGeometryStylesheet::replace = "Could not replace existing stylesheet at ``.";
UpdateQuiverGeometryStylesheet::save = "Could not save existing stylesheet at ``.";

UpdateQuiverGeometryStylesheet[] := Scope @ CatchMessage[

  template  = loadStylesheet["QuiverGeometry.template.nb"];
  darkMode  = loadStylesheet["DarkMode.nb"];
  lightMode = loadStylesheet["LightMode.nb"];

  cells = Join[
    KVMap[makeTemplateBoxStyleCell, $notebookDisplayFunction],
    generateNotebookColorPaletteStyles[$ColorPalette, Map[OklabLighter[#, 0.1]&] @ $LightColorPalette]
  ];

  darkTemplate  = fillTemplate[template, cells, Part[darkMode,  1, 2;;]];
  lightTemplate = fillTemplate[template, cells, Part[lightMode, 1, 2;;]];

  writeStylesheetFile[$DarkStylesheetPath, darkTemplate];
  writeStylesheetFile[$LightStylesheetPath, lightTemplate];
];

fillTemplate[template_, cells1_, cells2_] :=
  RepAll[template, Cell[StyleData["Dummy"], ___] :> Splice[Join[cells1, cells2]]];

loadStylesheet[name_] :=
  deleteUUIDs @ Get @ LocalPath["StyleSheets", name];

deleteUUIDs[nb_] :=
  Decases[nb, ExpressionUUID -> _, {0, Inf}];

writeStylesheetFile[path_, contents_] := Scope[
  If[!FileExistsQ[path], NotebookSave[contents, path]; Return[]];
  nb = NotebookOpen[path, Visible -> True];
  If[H[nb] =!= NotebookObject,          ThrowMessage["open", path]];
  If[FailureQ @ NotebookPut[contents, nb], ThrowMessage["replace", path]];
  If[FailureQ @ NotebookSave[nb],          ThrowMessage["save", path]];
  NotebookClose[nb];
];

(**************************************************************************************************)

PrivateFunction[GeneratePrivateQuiverGeometryStylesheet]

GeneratePrivateQuiverGeometryStylesheet[] := Scope[
 template = Notebook[{
      Cell[StyleData[StyleDefinitions -> $StylesheetPath]],
      Cell[StyleData["Dummy"]]
    },
    FrontEndVersion -> "13.1 for Mac OS X x86 (64-bit) (June 16, 2022)",
    StyleDefinitions -> "PrivateStylesheetFormatting.nb"
  ];
  cells = KVMap[makeTemplateBoxStyleCell, $notebookDisplayFunction];
  template //= RepAll[Cell[StyleData["Dummy"], ___] :> Splice[cells]];
  template
];

PrivateFunction[makeTemplateBoxStyleCell]

makeTemplateBoxStyleCell[name_, fn_] := With[
  {base = Lookup[$notebookDisplayFunctionBases, name, "MathFont"]},
  Cell[
    StyleData[name, StyleDefinitions -> StyleData[base]],
    TemplateBoxOptions -> {DisplayFunction -> fn}
  ]
];

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
  MapIndex1[Cell[StyleData["Color" <> IntStr[#2]], FontColor -> #1]&, palette],
  MapIndex1[Cell[StyleData["Background" <> IntStr[#2]], Background -> #1]&, lightPalette]
];

(**************************************************************************************************)

PublicFunction[UpdateLegacyNotebook]

createLegacyReplacementRules[] := Scope[
  keys = Select[Keys @ $notebookDisplayFunction, LowerCaseQ[STake[#, 1]]&];
  symbolRules = Map[
    UpperCaseFirst[#] -> #&,
    Select[Keys @ $notebookDisplayFunction, LowerCaseQ[STake[#, 1]]&]
  ];
  colorRules = {
    "RedGreenForm" -> "TealForm", "RedBlueForm" -> "PinkForm", "GreenBlueForm" -> "OrangeForm"
  };
  colorRules = {
    colorRules,
    colorRules /. s_Str :> SJoin["Light", s],
    colorRules /. s_Str :> SJoin["Dark", s]
  };
  Map[toTemplateRule, Flatten @ {symbolRules, colorRules}]
];

toTemplateRule[l_Str -> r_Str] :=
  TemplateBox[z_, l] (* /; (Print["Replacing ", l -> r]; True) *) :> TemplateBox[z, r];

$legacyReplacementRules := $legacyReplacementRules = createLegacyReplacementRules[];

UpdateLegacyNotebook[] := UpdateLegacyNotebook @ EvaluationNotebook[];

UpdateLegacyNotebook[nb_NotebookObject] := Scope[
  content = NotebookGet[nb];
  result = UpdateLegacyNotebook[content];
  NotebookPut[result, nb]
];

UpdateLegacyNotebook[nb_Notebook] := Scope[
  nb = Decases[nb, StyleDefinitions -> _];
  AppTo[nb, StyleDefinitions -> $StylesheetPath];
  RepRep[nb, $legacyReplacementRules]
];

UpdateLegacyNotebook::fail = "Could not update ``."
UpdateLegacyNotebook[path_Str] /; SEndsQ[path, ".nb"] := Scope[
  Quiet @ CopyFile[path, path <> ".backup"];
  nb = NotebookOpen[path, Visible -> False];
  UpdateLegacyNotebook @ nb;
  ApplyQuiverGeometryNotebookStyles @ nb;
  SetOptions[nb, Visible -> True];
  NotebookSave @ nb;
  NotebookClose @ nb;
  path
];

UpdateLegacyNotebook[dir_Str] /; FileType[dir] === Directory :=
  Map[UpdateLegacyNotebook, FileNames["*.nb", dir, Inf]];

(**************************************************************************************************)

PublicFunction[NotebookStyleDataNames]
PublicFunction[NotebookTemplateNames]
PublicFunction[FindMissingTemplateBoxDefinitions]

NotebookTemplateNames[nb_:Auto] :=
  DeepUniqueCases[ToNotebookExpression @ nb, TemplateBox[_, name_Str] :> name];

NotebookStyleDataNames[nb_:Auto] :=
  DeepUniqueCases[ToNotebookExpression @ nb, Cell[StyleData[style_Str, ___], ___] :> style];

$builtinTemplateNames = {"RowWithSeparators", "RowWithSeparator", "Spacer1", "Ceiling", "Floor"};

FindMissingTemplateBoxDefinitions[] := FindMissingTemplateBoxDefinitions @ Auto;
FindMissingTemplateBoxDefinitions[nb_, ref_:Auto] := Scope[
  SetAutomatic[ref, $StylesheetPath];
  availableNames = Join[NotebookStyleDataNames @ ref, $builtinTemplateNames];
  notebookNames = NotebookTemplateNames @ nb;
  Comp[notebookNames, availableNames]
];

(**************************************************************************************************)

PublicFunction[QuiverGeometryStyleNames]

QuiverGeometryStyleNames[] := NotebookStyleDataNames[$StylesheetPath];

(**************************************************************************************************)

PublicFunction[ApplyQuiverGeometryStylesheet]

ApplyQuiverGeometryStylesheet[] := (
  SetOptions[EvaluationNotebook[], StyleDefinitions -> $StylesheetPath];
);

ApplyQuiverGeometryStylesheet["Dark"] := (
  SetOptions[EvaluationNotebook[], StyleDefinitions -> $DarkStylesheetPath]
)

ApplyQuiverGeometryStylesheet["Light"] := (
  SetOptions[EvaluationNotebook[], StyleDefinitions -> $LightStylesheetPath]
)

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
    StyleDefinitions -> $StylesheetPath
  ];

