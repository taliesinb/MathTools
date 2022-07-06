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
    StyleDefinitions -> "QuiverGeometry.nb",
    DockedCells -> createQGNotebookDockedCells[]
  ];

createQGNotebookDockedCells[] := With[
  {qgPath = QuiverGeometryPackageLoader`$LoaderFileName},
  {getQG = Function[If[DownValues[QuiverGeometryPackageLoader`Load] === {}, Get[qgPath], QuiverGeometryPackageLoader`Load[False]]]},
  Cell[BoxData @ ToBoxes @ Row[makeButton /@ {
    " Preview: ",
    " nb "        :> (getQG[]; QuiverGeometry`PreviewInIAWriter[]),
    " section "         :> (getQG[]; QuiverGeometry`PreviewInIAWriter[SelectedCellGroup["Section"]]),
    " subsection "      :> (getQG[]; QuiverGeometry`PreviewInIAWriter[SelectedCellGroup["Subsection"]]),
    " group "           :> (getQG[]; QuiverGeometry`PreviewInIAWriter[SelectedCellGroup["Group"]]),
    " cell "            :> (getQG[]; QuiverGeometry`PreviewInIAWriter[SelectedCellGroup["Cell"]]),
    None,
    " Build: ",
    " nb "            :> (getQG[]; QuiverGeometry`BuildQGSiteHugo[EvaluationNotebook[], Verbose -> True];),
    " dir "            :> (getQG[]; NotebookSave[]; QuiverGeometry`BuildQGSiteHugo[NotebookDirectory[]];),
    " site "             :> (getQG[]; NotebookSave[]; QuiverGeometry`BuildQGSiteHugo[]),
    None,
    " Reload: ",
    " QG "               :> (Get[qgPath]),
    " Styles "           :> (getQG[]; QuiverGeometry`ApplyQuiverGeometryNotebookStyles[])
  }, "   "], "DockedCell"]];

makeButton[None] := Spacer[10];

makeButton[txt_String] := Style[txt, "Text"];

makeButton[txt_ :> code_] := Button[
  Style[" " <> txt <> " ", White],
  code,
  Appearance -> FrontEndResource["FEExpressions", "OrangeButtonNinePatchAppearance"]
]

(**************************************************************************************************)

PublicFunction[ReplaceContentInclusions]

ReplaceContentInclusions[] := Scope[
  nb = EvaluationNotebook[];
  NotebookFind[nb, "@", All, AutoScroll -> False];
  cells = SelectedCells @ nb;
  SelectionMove[nb, Previous, Cell];
  $dir = NotebookDirectory[];
  Scan[replaceInclusionCell, cells];
];

replaceInclusionCell[co_CellObject] := Scope[
  cell = NotebookRead[co];
  If[!MatchQ[cell, Cell[s_String /; StringMatchQ[s, $idInsertionRegexp], "Text"]], Return[None]];
  str = First @ cell;
  res = First @ StringReplace[str, $idInsertionRegexp :> foo["$1","$2", "$3"]];
  {title, tag, class} = List @@ res;
  findNb = FileNames["*" <> title <> ".nb", $dir];
  If[findNb === {}, Print["NB NOT FOUND: ", title]; Return[]];
  nbPath = First @ findNb;
  nb = Get[nbPath];
  cases = Cases[nb, c:Cell[_, "Output", ___, CellTags -> ({___, tag, ___} | tag), ___] :> c, {0, Infinity}, Heads -> True];
  If[cases === {}, Print["CELL NOT FOUND: ", title, " # ", tag]];
  cell = First[cases];
  cell //= MapAt[applyClassTag[class], 1];
  NotebookWrite[co, cell];
];

applyClassTag[class_][BoxData[contents_]] := BoxData @ TagBox[contents, "ClassTaggedForm"[class]];
applyClassTag[class_][contents_] := TagBox[contents, "ClassTaggedForm"[class]];

$idInsertionRegexp = RegularExpression["""\[\[\[(.+)#(.+)@(.+)\]\]\]"""];

(**************************************************************************************************)

PublicFunction[ApplyQuiverGeometryNotebookStyles]

ApplyQuiverGeometryNotebookStyles[] :=
  ApplyQuiverGeometryNotebookStyles @ EvaluationNotebook[];

ApplyQuiverGeometryNotebookStyles[nb_NotebookObject] := (SetOptions[nb,
  StyleDefinitions -> "QuiverGeometry.nb",
  DockedCells -> createQGNotebookDockedCells[]
]; nb);

ApplyQuiverGeometryNotebookStyles[dir_String ? DirectoryQ] :=
  Map[ApplyQuiverGeometryNotebookStyles, Select[StringFreeQ["XXX"]] @ FileNames["*.nb", dir, Infinity]];

ApplyQuiverGeometryNotebookStyles[file_String ? FileQ] := Scope[
  nb = NotebookOpen[file, Visible -> False];
  ApplyQuiverGeometryNotebookStyles[nb];
  NotebookSave[nb];
  NotebookClose[nb];
  file
];

(**************************************************************************************************)

PublicFunction[SelectedCellGroup]

SelectedCellGroup[type_:"Cell"] := Scope[
  nb = EvaluationNotebook[];
  init = SelectedCells[nb];
  If[init =!= {}, SelectionMove[nb, All, Cell]];
  If[init === {}, SelectionMove[nb, Next, Cell]; init = SelectedCells[nb]];
  If[init === {}, SelectionMove[nb, Previous, Cell]; init = SelectedCells[nb]];
  res = None;
  If[type === "Cell",
    res = cells = SelectedCells[nb]; read = First[cells, None];
    If[MatchQ[CurrentValue[read, "CellStyleName"], "Code" | "Input" | "Output"],
      SelectionMove[nb, All, CellGroup];
      cells = SelectedCells @ nb; read = Last[cells, None];
      If[Head[read] =!= CellObject, Goto[Done]];
      If[CurrentValue[read, "CellStyleName"] === "Output", res = cells];
    ];
    Goto[Done];
  ];
  If[type === "Group",
    SelectionMove[nb, All, CellGroup];
    res = cells = SelectedCells @ nb; read = Last[cells, None];
    If[Head[read] =!= CellObject, Goto[Done]];
    If[CurrentValue[read, "CellStyleName"] === "Output",
        SelectionMove[nb, All, CellGroup];
        res = SelectedCells @ nb];
    Goto[Done]
  ];
  Do[
    SelectionMove[nb, All, CellGroup];
    cells = SelectedCells @ nb; read = First[cells, None];
    If[Head[read] =!= CellObject, Goto[Done]];
    If[CurrentValue[read, "CellStyleName"] === type, res = cells; Goto[Done]]
  ,
    {8}
  ];
  Label[Done];
  If[init =!= {}, SelectionMove[First @ init, Before, Cell]];
  If[res === None, None, CellGroup @ res]
];


(**************************************************************************************************)

PublicFunction[PreviousTextCell]

PreviousTextCell[] := Scope[
  cellExpr = NotebookRead @ PreviousCell[];
  If[Head[cellExpr] =!= Cell, ThrowFailure["exportmdnocell"]];
  cellType = Replace[cellExpr, {Cell[_, type_String, ___] :> type, _ :> $Failed}];
  If[!MatchQ[cellType, "Text"], ReturnFailed[]];
  Take[cellExpr, 2]
];

(**************************************************************************************************)

PublicFunction[CopyImageToClipboard]

CopyImageToClipboard[expr_] := (
  CopyToClipboard @ Rasterize[expr, ImageFormattingWidth -> Infinity, ImageResolution -> 144];
  expr
);

(**************************************************************************************************)

PublicFunction[CopyRetinaImageToClipboard]

CopyRetinaImageToClipboard[expr_, crop_:False] := (
  CopyToClipboard @ If[crop, ImageCrop, Identity] @ Rasterize[expr /. (RawImageSize -> _) -> Sequence[], ImageFormattingWidth -> Infinity, ImageResolution -> (144*2), Background -> Transparent];
  expr
);

(**************************************************************************************************)

PublicFunction[FastCopyRetinaImageToClipboard]

FastCopyRetinaImageToClipboard[expr_, crop_:True] := (
  CopyToClipboard @ If[crop, ImageCrop, Identity] @ Rasterize[expr /. (RawImageSize -> _) -> Sequence[], ImageFormattingWidth -> Infinity, ImageResolution -> (144*2)];
  expr
);

(**************************************************************************************************)

PublicFunction[InteractiveCopyRetinaImageToClipboard]

InteractiveCopyRetinaImageToClipboard[expr_] := EventHandler[
  MouseAppearance[
    expr,
    "LinkHand"
  ],
  {"MouseClicked" :> CopyRetinaImageToClipboard[expr]}
];

(**************************************************************************************************)

PublicFunction[CopyImageGalleryToClipboard]

$galleryCount = 0;
newImageGalleryTempDir[] := Scope[
  dir = FileNameJoin[{$TemporaryDirectory, "temp_image_gallery", IntegerString @ $galleryCount++}];
  If[FileExistsQ[dir], DeleteDirectory[dir, DeleteContents -> True]];
  EnsureDirectory @ dir
];

CopyImageGalleryToClipboard[args___, "Conform" -> sz_] :=
  CopyImageGalleryToClipboard[ConformImages[Flatten @ {args}, sz, "Fit"]];

CopyImageGalleryToClipboard[args___] := Scope[
  $galleryDir = newImageGalleryTempDir[];
  images = galleryRasterize /@ Flatten[{args}];
  If[!VectorQ[images, ImageQ], ReturnFailed[]];
  images = CenterPadImages @ images;
  MapIndexed[saveToGalleryFile, images];
  CopyToClipboard @ $galleryDir;
  Print[$galleryDir]; SystemOpen[$galleryDir];
  SpacedRow[args]
];

saveToGalleryFile[image_, {i_}] := Scope[
  path = FileNameJoin[{$galleryDir, IntegerString[i] <> ".png"}];
  Export[path, image, CompressionLevel -> 1];
  path
]

galleryRasterize = Case[
  label_ -> expr_ := % @ LargeLabeled[expr, label];
  expr_ := Rasterize[expr, ImageFormattingWidth -> Infinity, ImageResolution -> 144];
]

(**************************************************************************************************)

PublicFunction[RemainingNotebook]

RemainingNotebook[] := Scope[
  cells = {};
  cell = NextCell[];
  skip = True;
  While[MatchQ[cell, _CellObject],
    result = NotebookRead @ cell;
    If[FailureQ[result], Break[]];
    skip = skip && MatchQ[result, Cell[_, "Output" | "Print" | "Echo" | "Message", ___]];
    If[!skip, AppendTo[cells, result]];
    cell = NextCell @ cell;
  ];
  Notebook[cells]
]

(**************************************************************************************************)

PublicFunction[ReplaceInCurrentNotebook, CellTypes, ReplaceExistingNotebook]

Options[ReplaceInCurrentNotebook] = {
  CellTypes -> Automatic,
  ReplaceExistingNotebook -> True
}

ReplaceInCurrentNotebook[rule_, opts:OptionsPattern[]] := Scope[
  ReplaceBoxesInCurrentNotebook[
    toBoxRules @ rule,
    opts
  ]
];

toBoxRules = Case[
  e_List := Map[%, e];
  a_ -> b_ := Rule[MakeBoxes @ a, MakeBoxes @ b];
  a_ :> b_ := Rule[MakeBoxes @ a, MakeBoxes @ b];
];

(**************************************************************************************************)

PublicFunction[CellReplaceBoxes]

CellReplaceBoxes[nbData_, rule_, typePattern_:_] :=
  ReplaceAll[nbData,
    Cell[contents_, type:typePattern, opts___] :>
      RuleCondition @ Cell[contents /. rule, type, opts]
  ];

(**************************************************************************************************)

PublicFunction[ReplaceBoxesInCurrentNotebook]

Options[ReplaceBoxesInCurrentNotebook] = Options[ReplaceInCurrentNotebook];

ReplaceBoxesInCurrentNotebook[boxRules_, OptionsPattern[]] := Scope[
  UnpackOptions[cellTypes, replaceExistingNotebook];
  nb = EvaluationNotebook[];
  nbData = NotebookGet[nb];
  If[Head[nbData] =!= Notebook, ReturnFailed[]];
  SetAutomatic[cellTypes, $replacementCellTypes];
  SetAll[cellTypes, _];
  nbData2 = CellReplaceBoxes[nbData, mapVerbatim[boxRules], cellTypes];
  If[nbData === nbData2, Beep[],
    If[replaceExistingNotebook, NotebookPut[nbData2, nb], NotebookPut[nbData2]]];
];

mapVerbatim = Case[
  e_List := Map[%, e];
  lhs_ -> rhs_ := Verbatim[lhs] -> rhs;
  lhs_ :> rhs_ := Verbatim[lhs] :> RuleCondition[rhs];
];

$replacementCellTypes = "Output" | "Text" | "Section" | "Subsection" | "Subsubsection" | "Item" | "SubItem" | "Subsubitem";

(**************************************************************************************************)

CopyFileToClipboard[path_] := If[
  Run["osascript -e 'tell app \"Finder\" to set the clipboard to ( POSIX file \"" <> path <> "\" )'"] === 0,
  path,
  $Failed
];

