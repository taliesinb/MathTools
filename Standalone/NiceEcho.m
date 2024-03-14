Begin["`NiceEcho`"];

SetAttributes[EchoH, HoldAllComplete];

EchoH[expr_] := Module[
  {res = $Aborted},
  Internal`WithLocalSettings[
    $echoLevel++
  ,
    res = expr
  ,
    $echoLevel--;
    With[{res2 = res}, echoArrow[expr, res2, True]]
  ]
];

(*************************************************************************************************)

(* if fn is held, EchoH0 should use EchoH *)
SetAttributes[EchoH0, HoldAllComplete];

EchoH0[fn_[args___]] := Module[
  {res = $Aborted, inner = $Aborted},
  Internal`WithLocalSettings[
    $echoLevel++
  ,
    inner = StandaloneSequence[args];
    res = Apply[fn, inner]
  ,
    $echoLevel--;
    With[{lhs2 = StandaloneHold[fn[$inner]] /. $inner -> inner, res2 = res}, echoArrow[lhs2, res2, True]]
  ]
];

(*************************************************************************************************)

(* if fn is held, EchoH0 should use EchoH *)
SetAttributes[EchoH1, HoldAllComplete];

EchoH1[fn_[args___]] := Module[
  {res = $Aborted, inner = $Aborted},
  Internal`WithLocalSettings[
    $echoLevel++
  ,
    inner = StandaloneSequence[args];
    res = Apply[fn, inner]
  ,
    $echoLevel--;
    With[{lhs2 = StandaloneHold[fn[$inner]] /. $inner -> inner, res2 = res}, echoArrow[lhs2, res2, True]]
  ]
];

(*************************************************************************************************)

Clear[echoArrow];

SetAttributes[echoArrow, HoldAllComplete];

$echoLevel = 0;

echoArrow[lhs2_, rhs2_, fill_] := With[
  {lhs = If[fill, fillImmediateVals, Identity] @ StandaloneHold @ lhs2,
   rhs = StandaloneHold @ rhs2},
  printRawEchoCell @ RowBox[{
    ToBoxes @ NicePaster[NicePane[lhs, {{200, 300}, UpTo @ 100}], lhs],
    "\[Function]",
    ToBoxes @ NicePaster[NicePane[rhs, {UpTo @ 500, UpTo @ 100}], rhs]
  }]
];

(* avoid filling just a single value or list of these *)
fillImmediateVals[e:StandaloneHold[_Symbol | {__Symbol}]] := e;
fillImmediateVals[e_] := ReplaceAll[e, s_Symbol ? System`Private`HasImmediateValueQ :> RuleCondition[s]];

(*************************************************************************************************)

printRawEchoCell[boxes_] := CellPrint @ Cell[
  BoxData @ boxes, "Echo",
  CellMargins -> {{80 + $echoLevel * 20, 0}, {0, 0}},
  GeneratedCell -> True, CellAutoOverwrite -> True,
  CellBracketOptions -> {"Color" -> Orange, "Thickness" -> 2}
];

(*************************************************************************************************)

EchoF[fn_] := EchoFL[getFLabel @ fn, fn];

SetAttributes[getFLabel, HoldAllComplete];

getFLabel[Function]       := "Fn";
getFLabel[e_Symbol]       := SymbolName @ e;
getFLabel[_Association]   := "\[LeftAssociation]\[Ellipsis]\[RightAssociation]";
getFLabel[h_[]]           := getFLabel[h] <> "[]";
getFLabel[h_[i_Integer]]  := getFLabel[h] <> "[" <> IntegerString[i] <> "]";
getFLabel[h_[___]]        := getFLabel[h] <> "[\[Ellipsis]]";
getFLabel[_]              := "\[FilledSquare]";

EchoFL[fnl_, fn_][args___] := Module[
  {res = $Aborted},
  Internal`WithLocalSettings[
    $echoLevel++
  ,
    res = fn[args]
  ,
    $echoLevel--;
    With[{res2 = res}, echoArrow[RawBoxes[StyleBox[fnl, Bold]][args], res2, True]]
  ]
];

(*************************************************************************************************)

EchoFH[fn_] := With[{label = getFLabel @ fn},
  Function[Null, EchoFLH[label, fn, SlotSequence[]], HoldAllComplete]
];

SetAttributes[EchoFLH, HoldAllComplete];

EchoFLH[fnl_, fn_, args___] := Module[
  {res = $Aborted},
  Internal`WithLocalSettings[
    $echoLevel++
  ,
    res = fn[args]
  ,
    $echoLevel--;
    With[{res2 = res}, echoArrow[RawBoxes[StyleBox[fnl, Bold]][args], res2, False]]
  ]
];

(*************************************************************************************************)

Clear[NicePaster];

NicePaster::usage =
"NicePaster[display, paste] displays as display but when clicked pastes the held expression paste.
";

SetAttributes[{NicePaster, printHeldExpr, nicePasterBoxes}, HoldRest];

MakeBoxes[NicePaster[display_, paste_], _] := nicePasterBoxes[display, paste];

nicePasterBoxes[display_, StandaloneHold[paste_]] := nicePasterBoxes[display, paste];

nicePasterBoxes[display_, paste_] := With[
  {boxes = MakeBoxes[display], comp = Compress @ Defer @ paste},
  TagBox[
    TagBox[boxes, EventHandlerTag[{{"MouseClicked", 1} :> pasteExpr[comp], Method -> "Preemptive", PassEventsDown -> Auto, PassEventsUp -> True}]],
    MouseAppearanceTag["LinkHand"]
  ]
];

pasteExpr[expr_String] := (
  (* the GeneratedCell -> False prevents the printed cell from mixing with echos and causing problems on re-evaluation *)
  CellPrint @ ExpressionCell[Uncompress @ expr, "Input", GeneratedCell -> False];
);

End[];