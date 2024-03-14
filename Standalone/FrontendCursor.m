Begin["`FrontendCursor`"];

(* $KernelName only gets set after a little delay so we remove this noisy stuff from LinkSnooper kernels when we can *)
System`SaveCursorPosition[] := If[FrontEnd`Private`$KernelName === "LinkSnooper",
  SetOptions[$FrontEndSession, FrontEndEventActions -> {}];
  Clear[System`SaveCursorPosition, System`RestoreCursorPosition];
,
  With[
  {ec = EvaluationCell[]},
  {ci = If[Head[ec] === CellObject, Developer`CellInformation[ec], {}]},
  $LastNotebookObject = EvaluationNotebook[];
  $LastCellObject = First[SelectedCells[$LastNotebookObject], None];
  $LastCellCursorPosition = If[MatchQ[ci, {__Rule}],
    First[Lookup[ci, "CursorPosition"], None],
    None
  ];
]];

System`RestoreCursorPosition[] := If[
  TrueQ[FrontEnd`Private`$KernelName =!= "LinkSnooper"] &&
  IntegerQ[$LastCellCursorPosition] && Head[$LastCellObject] === CellObject,
  SelectionMove[$LastCellObject, Before, CellContents];
  SelectionMove[$LastNotebookObject, Next, Character, $LastCellCursorPosition];
  SelectionMove[$LastNotebookObject, After, Character];
];

If[$FrontEnd =!= Null && FrontEnd`Private`$KernelName =!= "LinkSnooper",
SetOptions[$FrontEndSession, FrontEndEventActions -> {{"MenuCommand", "HandleShiftReturn"} :>
  (System`SaveCursorPosition[]; FrontEndTokenExecute["EvaluateCells"]; System`RestoreCursorPosition[])}];
];

End[];