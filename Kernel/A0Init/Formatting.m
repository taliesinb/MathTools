PrivateSpecialFunction[declareFormatting]

PrivateVariable[$isTraditionalForm]

getPatternHead[sym_Symbol] := sym;
getPatternHead[expr_] := P1 @ PatternHead @ expr;

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

PrivateSpecialFunction[DefineStandardTraditionalForm]

PublicFunction[HasBoxFormQ]

CacheSymbol[$StandardTraditionalFormCache]

$boxFormHeadAssoc = UAssoc[];

DefineStandardTraditionalForm[list_List] := Scan[DefineStandardTraditionalForm, list];

DefineStandardTraditionalForm[rule:(lhs_ :> rhs_)] := (
  AssociateTo[$boxFormHeadAssoc, PatternHead[lhs] -> True];
  CachedInto[$StandardTraditionalFormCache, rule,
    MakeBoxes[lhs /; Refresh[!BoxForm`UseTextFormattingQ, None], StandardForm] := rhs;
    MakeBoxes[l:lhs, TraditionalForm] := MakeBoxes @ l;
  ];
)

HasBoxFormQ[head_Symbol[___] | head_Symbol] := Lookup[$boxFormHeadAssoc, Hold[head], False];
HasBoxFormQ[_] := False;

_DefineStandardTraditionalForm := BadArguments[];

(**************************************************************************************************)

PublicDebuggingFunction[PrintFormatDefinitions]

PrintFormatDefinitions[sym_Symbol] :=
  PrintDefinitions @ Select[FormatValues[MakeBoxes], ContainsQ[sym]];

(**************************************************************************************************)

PrivateSpecialFunction[declareBoxFormatting]

PrivateVariable[$BoxFormattingHeadQ]

SetHoldAllComplete[getPatternHead];

$BoxFormattingHeadQ = UAssoc[];

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
Grid[a_Assoc] := Grid[KeyValueMap[List, a], Alignment -> Left, Dividers -> All, ItemSize -> {{Automatic, 30}}];
Protect[Grid]

(**************************************************************************************************)

PublicFunction[PrintGraphicsFormatDefinitions]

PrintGraphicsFormatDefinitions[sym_Symbol] := PrintDefinitions @
  Select[DownValues[Typeset`MakeBoxes], ContainsQ[sym]];

(**************************************************************************************************)

PrivateFunction[make2DBoxes, make3DBoxes]

make2DBoxes[e_] := Typeset`MakeBoxes[e, StandardForm, Graphics];
make3DBoxes[e_] := Typeset`MakeBoxes[e, StandardForm, Graphics3D];
