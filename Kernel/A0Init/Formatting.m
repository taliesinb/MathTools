PrivateFunction[declareFormatting]
PrivateVariable[$isTraditionalForm]

getPatternHead[sym_Symbol] := sym;
getPatternHead[expr_] := First @ PatternHead @ expr;

declareFormatting[rules__RuleDelayed] := Scan[declareFormatting, {rules}];
declareFormatting[lhs_ :> rhs_] :=
  With[{head = getPatternHead[lhs]}, {isProtected = ProtectedFunctionQ[head]},
    If[isProtected, Unprotect[head]];
    Format[$LHS:lhs, StandardForm] := Block[{$isTraditionalForm = False}, Interpretation[rhs, $LHS]];
    Format[$LHS:lhs, TraditionalForm] := Block[{$isTraditionalForm = True}, Interpretation[rhs, $LHS]];
    If[isProtected, Protect[head]];
  ];

declareFormatting[___] := Panic["BadFormatting"]

(**************************************************************************************************)

PrivateFunction[declareBoxFormatting]
PrivateVariable[$BoxFormattingHeadQ]

SetHoldAllComplete[getPatternHead];

$BoxFormattingHeadQ = UAssociation[];

declareBoxFormatting[rules__RuleDelayed] := Scan[declareBoxFormatting, {rules}];
declareBoxFormatting[lhs_ :> rhs_] :=
  With[{head = getPatternHead[lhs]}, {isProtected = ProtectedFunctionQ[head]},
    If[isProtected, Unprotect[head]]; $BoxFormattingHeadQ[head] = True;
    MakeBoxes[lhs, StandardForm] := Block[{$isTraditionalForm = False}, rhs];
    MakeBoxes[lhs, TraditionalForm] := Block[{$isTraditionalForm = True}, rhs];
    If[isProtected, Protect[head]];
  ];

declareBoxFormatting[___] := Panic["BadFormatting"]

(**************************************************************************************************)

PrivateFunction[summaryItem, padSummaryItem]

summaryItem[a_, b_] := BoxForm`SummaryItem[{a <> ": ", b}];
padSummaryItem[a_, b_] := BoxForm`SummaryItem[{Pane[a <> ": ", 60], b}];

(**************************************************************************************************)

Unprotect[Grid]
Grid[a_Association] := Grid[KeyValueMap[List, a], Alignment -> Left, Dividers -> All, ItemSize -> {{Automatic, 30}}];
Protect[Grid]

(**************************************************************************************************)

PrivateFunction[declareGraphicsFormatting]

declareGraphicsFormatting[lhs_ :> rhs_, type_:Graphics|Graphics3D] :=
  Typeset`MakeBoxes[expr:lhs, StandardForm | TraditionalForm, type] :=
    Construct[InterpretationBox, rhs, expr];

declareGraphicsFormatting[list_List, type_:Graphics|Graphics3D] :=
  Scan[declareGraphicsFormatting[#, type]&, list];

declareGraphicsFormatting[___] := Panic["BadGraphicsFormatting"];

(**************************************************************************************************)

PrivateFunction[make2DBoxes, make3DBoxes]

make2DBoxes[e_] := Typeset`MakeBoxes[e, StandardForm, Graphics];
make3DBoxes[e_] := Typeset`MakeBoxes[e, StandardForm, Graphics3D];
