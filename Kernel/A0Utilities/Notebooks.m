
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

PublicFunction[ExtendCellSelection]

ExtendCellSelection[] := ExtendCellSelection @ EvaluationNotebook[];

ExtendCellSelection[nb_NotebookObject] := extendedCells[nb, True]

(**************************************************************************************************)

PublicFunction[SelectedCellGroup]

SelectedCellGroup[] := SelectedCellGroup @ EvaluationNotebook[];

SelectedCellGroup[nb_NotebookObject] :=
  CellGroup @ extendedCells[nb, False]

(**************************************************************************************************)

extendedCells[nb_, extend_:False] := Scope[
  
  cells = SelectedCells[nb];

  If[cells === {},
    SelectionMove[nb, Next, Cell];
    cells = SelectedCells[nb];
    If[cells === {}, SelectionMove[nb, Previous, Cell]; cells = SelectedCells[nb]];
    nextPrev = True;
  ,
    nextPrev = False;
  ];

  If[Length[cells] == 1,
    type = cellType @ First @ cells;
    If[MatchQ[type, "Code" | "Input" | "Output" | "Section" | "Subsection" | "Subsubsection" | "Chapter" | "Title"],
      SelectionMove[nb, All, CellGroup]];
    If[!nextPrev && extend && MatchQ[type, "Text" | "Item" | "SubItem"],
      cpos = Lookup[Developer`CellInformation[First @ cells], "CursorPosition"];
      If[cpos === "CellBracket",
        SelectionMove[nb, All, CellGroup],
        SelectionMove[nb, All, Cell]
      ];
    ];
    cells = SelectedCells[nb];
  ,
    If[extend, SelectionMove[nb, All, CellGroup]];
    cells = SelectedCells[nb];
  ];

  cells
];

cellType[cell_] := CurrentValue[cell, "CellStyleName"];

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

CopyFileToClipboard[path_] := If[
  Run["osascript -e 'tell app \"Finder\" to set the clipboard to ( POSIX file \"" <> path <> "\" )'"] === 0,
  path,
  $Failed
];

(**************************************************************************************************)

PublicFunction[ToNotebookExpression]

ToNotebookExpression = Case[
  str_String | Path[str_String]     := Import @ str;
  nb_NotebookObject                 := NotebookGet @ nb;
  nb_Notebook                       := nb;
  Automatic                         := % @ EvaluationNotebook[];
];

(**************************************************************************************************)

PublicFunction[DeleteUUIDs]

DeleteUUIDs[e_] := DeleteCases[e, ExpressionUUID -> _, {0, Infinity}, Heads -> True];

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
