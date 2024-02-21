PublicFunction[NotebookObjectQ]

NotebookObjectQ = Case[
  _NotebookObject := True;
  _               := False;
];

(**************************************************************************************************)

PrivateFunction[ValidNotebookObjectQ]

ValidNotebookObjectQ[nb_NotebookObject] := ListQ @ Options[nb, "FrontEndVersion"];
ValidNotebookObjectQ[_] := False;

(**************************************************************************************************)

PrivateFunction[CallFrontEnd]

(* this allows MakeImage etc to work from the terminal *)
CallFrontEnd[e_] := If[$Notebooks, MathLink`CallFrontEnd[e], System`ConvertersDump`Utilities`GetFromFE[e]];

(**************************************************************************************************)

PublicFunction[WithExternalMessageCapture]

$tmpMessageFilePath := $tmpMessageFilePath = TemporaryPath["messages.txt"];

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
  If[!MatchQ[cell, Cell[s_Str /; StringMatchQ[s, $idInsertionRegexp], "Text"]], Return[None]];
  str = P1 @ cell;
  res = P1 @ StringReplace[str, $idInsertionRegexp :> foo["$1","$2", "$3"]];
  {title, tag, class} = List @@ res;
  findNb = FileNames["*" <> title <> ".nb", $dir];
  If[findNb === {}, Print["NB NOT FOUND: ", title]; Return[]];
  nbPath = P1 @ findNb;
  nb = Get[nbPath];
  cases = Cases[nb, c:Cell[_, "Output", ___, CellTags -> ({___, tag, ___} | tag), ___] :> c, {0, Infinity}, Heads -> True];
  If[cases === {}, Print["CELL NOT FOUND: ", title, " # ", tag]];
  cell = P1[cases];
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

  If[Len[cells] == 1,
    type = cellType @ P1 @ cells;
    If[MatchQ[type, "Code" | "Input" | "Output" | "Section" | "Subsection" | "Subsubsection" | "Chapter" | "Title"],
      SelectionMove[nb, All, CellGroup]];
    If[!nextPrev && extend && MatchQ[type, "Text" | "Item" | "SubItem"],
      cpos = Lookup[CellInformation[P1 @ cells], "CursorPosition"];
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

PublicFunction[CopyPreviousCellRaw]

(* this is useful for testing *)

CopyPreviousCellRaw[] := Scope[
  cellExpr = NotebookRead @ PreviousCell[];
  cellExpr = Take[cellExpr, 2];
  CopyToClipboard @ ToPrettifiedString @ cellExpr;
];


(**************************************************************************************************)

PublicFunction[PreviousTextCell]

PreviousTextCell[] := Scope[
  cellExpr = NotebookRead @ PreviousCell[];
  If[H[cellExpr] =!= Cell, ThrowFailure["exportmdnocell"]];
  cellType = Replace[cellExpr, {Cell[_, type_Str, ___] :> type, _ :> $Failed}];
  If[!MatchQ[cellType, "Text"], ReturnFailed[]];
  Take[cellExpr, 2]
];

(**************************************************************************************************)

PublicIOFunction[CopyImageToClipboard]

CopyImageToClipboard[expr_] := (
  CopyToClipboard @ Rasterize[expr, ImageFormattingWidth -> Infinity, ImageResolution -> 144];
  expr
);

(**************************************************************************************************)

PublicIOFunction[CopyRetinaImageToClipboard]

CopyRetinaImageToClipboard[expr_, crop_:False] := (
  CopyToClipboard @ If[crop, ImageCrop, Id] @ Rasterize[expr /. (RawImageSize -> _) -> Sequence[], ImageFormattingWidth -> Infinity, ImageResolution -> (144*2), Background -> Transparent];
  expr
);

(**************************************************************************************************)

PublicIOFunction[CopyBearImageToClipboard]

CopyBearImageToClipboard[expr_] := (
  CopyToClipboard @ ImageCropAndPad @ Rasterize[expr /. (RawImageSize -> _) -> Sequence[], ImageFormattingWidth -> Infinity, ImageResolution -> (144*2), Background -> White];
  expr
)

ImageCropAndPad[img_] := ImagePad[ImageCrop[img], 50, White];

(**************************************************************************************************)

PublicIOFunction[FastCopyRetinaImageToClipboard]

FastCopyRetinaImageToClipboard[expr_, crop_:True] := (
  CopyToClipboard @ If[crop, ImageCrop, Id] @ Rasterize[expr /. (RawImageSize -> _) -> Sequence[], ImageFormattingWidth -> Infinity, ImageResolution -> (144*2)];
  expr
);

(**************************************************************************************************)

PublicIOFunction[InteractiveCopyRetinaImageToClipboard]

InteractiveCopyRetinaImageToClipboard[expr_] := ClickForm[expr, CopyRetinaImageToClipboard[expr]];

(**************************************************************************************************)

PublicIOFunction[CopyImageGalleryToClipboard]

$galleryCount = 0;
newImageGalleryTempDir[] := Scope[
  dir = TemporaryPath["temp_image_gallery", IntegerString[$galleryCount++]];
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
  path = PathJoin[$galleryDir, IntegerString[i] <> ".png"];
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
  CellTypes -> "Input" | "Code",
  ReplaceExistingNotebook -> False
};

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
  l_LexicalRule := RawBoxes @ makeLexicalRule[l];
];

(**************************************************************************************************)

PublicHead[LexicalPattern, LexicalRule]

SetHoldAll[LexicalPattern, LexicalRule, lexicalPatternBoxes, makeLexicalRule];

DefineStandardTraditionalForm[LexicalPattern[e_] :> lexicalPatternBoxes @ e];

lexicalPatternBoxes[e_] := Block[{BoxForm`UseTextFormattingQ}, MakeBoxes @@ ReplaceAll[
  Hold @ e, {
    Verbatim[Pattern][p_, rhs_] :> RuleCondition @ RawBoxes @ Construct[Pattern, p, lexicalPatternBoxes @ rhs],
    s:(_Str | Verbatim[_] | Verbatim[__] | Verbatim[___]) :> RawBoxes[s]
  }
]];

$whitespacePatt = ___Str ? WhitespaceQ;

addWhitespacePatts = Case[
  RowBox[{head_, "[", args_, "]"}] := RowBox[{head, "[", $whitespacePatt, % @ args, $whitespacePatt, "]"}];
  RowBox[list_List] := RowBox[PrependAppend[$whitespacePatt, $whitespacePatt] @ Riffle[% /@ list, $whitespacePatt]];
  other_ := other;
]

LexicalRule /: Normal[l_LexicalRule] := makeLexicalRule[l];

makeLexicalRule[LexicalRule[lhs_, rhs_] | LexicalRule[lhs_ :> rhs_]] := With[
  {lhsBoxes = addWhitespacePatts @ lexicalPatternBoxes @ lhs, patSymRules = toPatRule /@ PatternSymbols[lhs]},
  {rhsBoxes = lexicalPatternBoxes @@ ReplaceAll[Hold[rhs], patSymRules]},
  Construct[RuleDelayed, lhsBoxes, rhsBoxes]
];

toPatRule[Hold[sym_]] := HoldPattern[sym] -> RawBoxes[sym];

(**************************************************************************************************)

PublicFunction[CellReplaceBoxes]

CellReplaceBoxes[nbData_, rule_, typePattern_:_] :=
  ReplaceAll[nbData,
    Cell[contents_, type:typePattern, opts___] :>
      RuleCondition @ Cell[contents /. rule, type, opts]
  ];

(**************************************************************************************************)

PublicFunction[CellFindBoxes]

CellFindBoxes[nbData_, rule_, typePattern_:_] :=
  Catenate @ DeepCases[nbData,
    Cell[contents_, type:typePattern, opts___] :>
      RuleCondition @ DeepCases[contents, rule]
  ];

(**************************************************************************************************)

PublicFunction[EvaluateInitializationCellsContaining]

General::notstrpatt = "`` is not a string pattern."
EvaluateInitializationCellsContaining[token_] := Scope[
  If[!StringPatternQ[token], ReturnFailed["notstrpatt", token]];
  cells = FindCellObjects[token, "Code"];
  Scan[cell |-> (
      SelectionMove[cell, All, CellContents];
      FrontEndExecute[FrontEndToken["EvaluateCells"]];
    ),
    cells
  ];
]

(**************************************************************************************************)

PublicFunction[NewNotebookFromCellsContaining]

NewNotebookFromCellsContaining[token_, type_:All] := Scope[
  nb = NotebookGet[EvaluationNotebook[]];
  cells = FindCellObjects[token, type];
  cellData = NotebookRead[cells];
  NotebookPut @ ReplacePart[nb, 1 -> cellData]
]

(**************************************************************************************************)

PublicFunction[FindCellObjects]

FindCellObjects[boxPattern_, cellType_:All] := Scope[
  cells = If[cellType === All, Cells[], Cells[CellStyle -> cellType]];
  cellData = Part[NotebookRead[cells], All, 1];
  test = If[StringPatternQ[boxPattern], cellContainsStringQ, cellContainsQ][boxPattern];
  indices = SelectIndices[cellData, test];
  Part[cells, indices]
];

cellContainsStringQ[patt_][str_Str | BoxData[str_Str] | TextData[str_Str]] := StringContainsQ[str, patt];
cellContainsStringQ[patt_][expr_] := ContainsQ[expr, patt];
cellContainsQ[patt_][expr_] := ContainsQ[expr, patt];

(**************************************************************************************************)

CopyFileToClipboard[path_] := If[
  RunAppleScript["tell app \"Finder\" to set the clipboard to ( POSIX file \"" <> path <> "\" )"] === 0,
  path,
  $Failed
];

(**************************************************************************************************)

PublicIOFunction[PasteFromClipboard]

PasteFromClipboard[] := ReadString["!pbpaste"];

(**************************************************************************************************)

PublicFunction[ToNotebookExpression]

ToNotebookExpression = Case[
  str_Str | Path[str_Str] := Import @ str;
  nb_NotebookObject       := NotebookGet @ nb;
  nb_Notebook             := nb;
  Automatic               := % @ EvaluationNotebook[];
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
  If[H[nbData] =!= Notebook, ReturnFailed[]];
  SetAutomatic[cellTypes, $replacementCellTypes];
  SetAll[cellTypes, _];
  nbData2 = CellReplaceBoxes[nbData, mapVerbatim[boxRules], cellTypes];
  If[nbData === nbData2, Beep[],
    If[replaceExistingNotebook, NotebookPut[nbData2, nb], NotebookPut[nbData2]]];
];

mapVerbatim = Case[
  e_List := Map[%, e];
  RawBoxes[r_RuleDelayed] := r;
  lhs_ -> rhs_ := Verbatim[lhs] -> rhs;
  lhs_ :> rhs_ := Verbatim[lhs] :> RuleCondition[rhs];
];

$replacementCellTypes = "Output" | "Text" | "Section" | "Subsection" | "Subsubsection" | "Item" | "SubItem" | "Subsubitem" | "PreformattedCode";

(**************************************************************************************************)

PublicFunction[FindBoxesInCurrentNotebook]

Options[FindBoxesInCurrentNotebook] = {CellTypes -> Automatic};

FindBoxesInCurrentNotebook[boxRules_, OptionsPattern[]] := Scope[
  UnpackOptions[cellTypes];
  nb = EvaluationNotebook[];
  nbData = NotebookGet[nb];
  If[H[nbData] =!= Notebook, ReturnFailed[]];
  SetAutomatic[cellTypes, $replacementCellTypes];
  SetAll[cellTypes, _];
  CellFindBoxes[nbData, boxRules, cellTypes]
];

(**************************************************************************************************)

PrivateFunction[CellCursorPosition]

CellCursorPosition[cell_CellObject] := Scope[
  info = CellInformation[cell];
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

PrivateFunction[PrintInputCell]

PrintInputCell[e_] := CellPrint @ ExpressionCell[e, "Input"];

(**************************************************************************************************)

PublicFunction[InsertCellTable]

$textPlaceholder = Cell["\[Placeholder]", "Text"];

InsertCellTable[rows_, cols_] := Scope[
  cell = Cell[
    BoxData @ GridBox[
      Repeat[$textPlaceholder, {rows, cols}],
      BaseStyle -> "Text",
      FrameStyle -> GrayLevel[0.9],
      GridBoxDividers -> {"Columns" -> {{True}}, "Rows" -> {{True}}},
      GridBoxSpacings -> {"Rows" -> {{1}}, "Columns" -> {{2}}},
      GridBoxAlignment -> {"Columns" -> {{Left}}}
    ],
    "Text"
  ];
  SelectionMove[EvaluationNotebook[], After, Cell];
  NotebookWrite[EvaluationNotebook[], cell, Placeholder (* doesn't work! *)];
];

(**************************************************************************************************)

PublicFunction[CellTableAddRow, CellTableAddColumn, CellTableDeleteRow, CellTableDeleteColumn, CellTableMoveRow, CellTableMoveColumn]

General::nottexttable = "Previous cell is not a textual table.";

CellTableAddRow[n_Int:-1] := CatchMessage @ modifyPreviousTextTable[InsertConstantRow[$textPlaceholder, n]];
CellTableAddColumn[n_Int:-1] := CatchMessage @ modifyPreviousTextTable[InsertConstantColumn[$textPlaceholder, n]];
CellTableDeleteRow[n_Int:-1] := CatchMessage @ modifyPreviousTextTable[Delete[n]];
CellTableDeleteColumn[n_Int:-1] := CatchMessage @ modifyPreviousTextTable[DeleteColumn[n]];
CellTableMoveRow[a_Int -> b_Int] := CatchMessage @ modifyPreviousTextTable[moveRow[a -> b]];
CellTableMoveColumn[a_Int -> b_Int] := CatchMessage @ modifyPreviousTextTable[moveCol[a -> b]];

modifyPreviousTextTable[fn_] := Scope[
  prevCell = PreviousCell[];
  cell = NotebookRead[prevCell];
  If[!MatchQ[cell, Cell[BoxData @ GridBox[_, BaseStyle -> "Text", ___], "Text", ___]],
    ThrowMessage["nottexttable"]];
  cell[[1, 1, 1]] //= fn;
  NotebookWrite[prevCell, cell];
]

moveRow[a_ -> -1][list_] := moveRow[a -> Len[list]][list];
moveRow[-1 -> b_][list_] := moveRow[Len[list] -> b][list];
moveRow[a_ -> b_][list_] := Insert[Delete[list, a], Part[list, a], b];

moveCol[rule_][list_] := Transpose @ moveRow[rule][Transpose @ list];

(**************************************************************************************************)

PublicFunction[ToggleInlineCells]

$LastConvertedCellBuffer = {};

ToggleInlineCells[] := ToggleInlineCells @ SelectedCells[];

ToggleInlineCells[list:{___CellObject}] := Map[ToggleInlineCells, list];

ToggleInlineCells[cell_CellObject] := Scope[
  cellData = NotebookRead[cell];
  (* to make sure we can get manually retrieve cells if they get mangld *)
  $LastConvertedCellBuffer ^= Prepend[$LastConvertedCellBuffer, cellData];
  If[Len[$LastConvertedCellBuffer] > 4, $LastConvertedCellBuffer ^= Take[$LastConvertedCellBuffer, 4]];

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
    !ContainsQ[cellData, _Str ? containsWLQ],
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
  c:Cell[_Str ? containsWLQ, __] := MapAt[% /* TextData, c, 1];
  c:Cell[_TextData, __] := MapAt[%, c, 1];
  t_TextData := Map[%, t];
  list_List := Map[%, list];
  s_StyleBox := MapAt[%, s, 1];
  c_Cell := c;
  str_Str ? containsWLQ := splitToCode[str];
  e_ := e;
];

splitToCode[str_Str] :=
  List @@ StringReplace[str, {
    var:("$" ~~ WordCharacter ~~ RepeatedNull[WordCharacter | DigitCharacter | "$"] ~~ WordBoundary) :> makeToggledInlineCell[var],
    span:("((" ~~ Shortest[Repeated[Except["\n"]]] ~~ "))") :> makeToggledInlineCell[span]
  }];

makeToggledInlineCell[str_] := Scope[
  expr = toInlineExpression[str, InputForm];
  If[FailureQ[expr], BadBeep[]; Return @ str];
  boxes = ToBoxes[expr, TraditionalForm];
  boxes = TagBox[boxes, "ToggledInlineCell" -> str];
  Cell[BoxData @ FormBox[boxes, TraditionalForm]]
];

(**************************************************************************************************)

PublicGraphicsDirective[CellWrapper]

(* TODO: This helps capture keypressed for EventHandler to work nicely! *)

DefineStandardTraditionalForm[CellWrapper[a_] :> With[{b = ToBoxes[a]}, Cell[BoxData @ b,ShowSelection->False]]];
