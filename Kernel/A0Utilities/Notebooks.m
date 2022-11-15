PublicFunction[WithExternalMessageCapture]

$tmpMessageFilePath = FileNameJoin[{$TemporaryDirectory, "messages.txt"}];

SetHoldFirst[WithExternalMessageCapture];

WithExternalMessageCapture[body_] := Scope[
  stream = OpenWrite[$tmpMessageFilePath, PageWidth -> 120, FormatType -> InputForm];
  $hadMessage = False;
  Block[{$Messages = {stream}, $MessagePrePrint = applyIF}, Check[result = body, $hadMessage = True]];
  Close[$tmpMessageFilePath];
  If[$hadMessage, NotebookOpen[$tmpMessageFilePath]];
  result
];

(* this is because $Messages doesn't respect FormatType -> InputForm *)
applyIF[HoldForm[e_]] := HoldForm[InputForm[e]];
applyIF[e_] := e;

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

InteractiveCopyRetinaImageToClipboard[expr_] := ClickForm[expr, CopyRetinaImageToClipboard[expr]];

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
  RunAppleScript["tell app \"Finder\" to set the clipboard to ( POSIX file \"" <> path <> "\" )"] === 0,
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

(**************************************************************************************************)

PrivateFunction[CellCursorPosition]

CellCursorPosition[cell_CellObject] := Scope[
  info = Developer`CellInformation[cell];
  If[!RuleListQ[info], None, First[Lookup[info, "CursorPosition", None], None]]
];

(**************************************************************************************************)

PrivateFunction[ReplaceTaggingRule]

ReplaceTaggingRule[Cell[l__, TaggingRules -> rules_List, r___], rule_] :=
  Cell[l, TaggingRules -> ReplaceOptions[rules, rule], r];

ReplaceTaggingRule[Cell[args__], rule_] :=
  Cell[args, TaggingRules -> {rule}];
  
(**************************************************************************************************)

PrivateFunction[LookupTaggingRule]

LookupTaggingRule[Cell[__, TaggingRules -> rules_List, ___], key_] :=
  Lookup[rules, key, None];

LookupTaggingRule[_, _] := None;

(**************************************************************************************************)

PublicFunction[ToggleInlineCells]

$LastConvertedCellBuffer = {};

ToggleInlineCells[] := ToggleInlineCells @ SelectedCells[];

ToggleInlineCells[list:{___CellObject}] := Map[ToggleInlineCells, list];

ToggleInlineCells[cell_CellObject] := Scope[
  cellData = NotebookRead[cell];
  (* to make sure we can get manually retrieve cells if they get mangld *)
  $LastConvertedCellBuffer ^= Prepend[$LastConvertedCellBuffer, cellData];
  If[Length[$LastConvertedCellBuffer] > 4, $LastConvertedCellBuffer ^= Take[$LastConvertedCellBuffer, 4]];

  If[MatchQ[cellData, Cell @ BoxData @ $toggledInlineCellP],
    (* we're already inside an inline cell, just change it to text inline *)
    str = toggleToText @ cellData;
    NotebookWrite[cell, Cell[TextData[str], "Text"]];
    Return[];
  ];
  savePosition = CellCursorPosition[cell];
  restorePosition = None; nb = None;
  If[MemberQ[SelectedCells[], cell], nb = ParentNotebook[cell]];
  newData = Which[
    ContainsQ[cellData, $toggledInlineCellP],
      restorePosition = LookupTaggingRule[cellData, "CursorPosition"];
      toggleToText[cellData],
    !ContainsQ[cellData, _String ? containsWLQ],
      BadBeep[];
      Return[],
    True,
      newData = toggleToCode[cellData];
      ReplaceTaggingRule[newData, "CursorPosition" -> savePosition]
  ];
  NotebookWrite[cell, newData, All];
  If[nb =!= None && restorePosition =!= None,
    SelectionMove[nb, ##, AutoScroll -> False]& @@@ {
      {All, CellContents},
      {Before, CellContents},
      {Next, Character, restorePosition},
      {After, Character}
    }
  ];
];

When two paths are(( PCF[ CP[$gva,$gvb,$gvc], CP[$gvc,$gvd] ] = CP[$gva,$gvb,$gvc,$gvd] ))

$toggledInlineCellP = FormBox[TagBox[_, "ToggledInlineCell" -> _], _];

toggleToText = Case[
  c:Cell[_TextData, __] := MapAt[%, c, 1];
  t_TextData := Map[%, t];
  list_List := Map[%, list];
  s:StyleBox[___] := MapAt[%, s, 1];
  Cell[BoxData[FormBox[TagBox[_, "ToggledInlineCell" -> str_], TraditionalForm]]] := str;
  c_Cell := c;
  e_ := e;
];

$inlineWLExprRegex =  RegularExpression @ "(\\(\\([^\n]+\\)\\))|(\\$[[:alpha:]][[:alpha:][:digit:]$]*\\b)";
containsWLQ[e_] := StringContainsQ[e, $inlineWLExprRegex];

toggleToCode = Case[
  c:Cell[_String ? containsWLQ, __] := MapAt[% /* TextData, c, 1];
  c:Cell[_TextData, __] := MapAt[%, c, 1];
  t_TextData := Map[%, t];
  list_List := Map[%, list];
  s_StyleBox := MapAt[%, s, 1];
  c_Cell := c;
  str_String ? containsWLQ := splitToCode[str];
  e_ := e;
];

splitToCode[str_String] :=
  List @@ StringReplace[str, {
    var:("$" ~~ WordCharacter ~~ RepeatedNull[WordCharacter | DigitCharacter | "$"] ~~ WordBoundary) :> makeToggledInlineCell[var],
    span:("((" ~~ Shortest[Repeated[Except["\n"]]] ~~ "))") :> makeToggledInlineCell[span]
  }];

makeToggledInlineCell[str_] := Scope[
  expr = toInlineExpression[str];
  If[FailureQ[expr], BadBeep[]; Return @ str];
  boxes = ToBoxes[expr, StandardForm];
  boxes = TagBox[boxes, "ToggledInlineCell" -> str];
  Cell[BoxData @ FormBox[boxes, TraditionalForm]]
];