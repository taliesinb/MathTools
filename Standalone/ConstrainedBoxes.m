Begin["`ConstrainedBoxes`"];

Clear[ConstrainedMakeBoxes];

ConstrainedMakeBoxes::usage =
"ConstrainedMakeBoxes[expr, limit] is like MakeBoxes but attempts to introduces truncation when the number of approximate characters exceeds expr.
ConstrainedMakeBoxes[..., slimit] truncates strings that are longer than slimit.
ConstrainedMakeBoxes[..., ..., dlimit] truncates elements deeper than dlimit.
* symbols will not be printed with their full contexts.
* symbols outside system will be tinted red if they have definitions and orange if not.
"

(* todo: make the character limit apply to both width and height, and make it image size based instead! *)
ConstrainedMakeBoxes[e_, charLimit_, strTrunc_:Infinity, maxDepth_:5] := Block[
  {$cmMax = charLimit, $cmLeft = charLimit, $cmStrMax = strTrunc, $cmDepth = maxDepth},
  cmAny @ e
];

(*
TODO: optimization: if its a small expression free of weird heads, just call ToExpression and then check if the resulting length is
short??? *)

(*************************************************************************************************)

Clear[cmAny];

SetAttributes[{cmAny, cmFinal, holdBigQ}, HoldAllComplete];

With[{haq = Developer`HoldAtomQ, $la = "\[LeftAssociation]", $ra = "\[RightAssociation]"},

cmAny[None]                := chowing[4, "None"];
cmAny[True]                := chowing[4, "True"];
cmAny[False]               := chowing[5, "False"];
cmAny[Infinity]            := chowing[1, "\[Infinity]"];
cmAny[DirectedInfinity[1]] := chowing[1, "\[Infinity]"];

cmAny[e_Symbol ? haq]      := cmSymbol @ e;
cmAny[e_Integer ? haq]     := chowStr @ cmInt @ e;
cmAny[e_Real ? haq]        := cmReal @ e;
cmAny[e_String ? haq]      := cmString @ e;

cmAny[e_Rule]              := cmRule[e];
cmAny[e_RuleDelayed]       := cmRule[e];
cmAny[e_Pattern]           := cmPattern[e];
cmAny[e_Alternatives]      := cmAlternatives[e];

cmAny[e_Set]               := cmSet[e];
cmAny[e_SetDelayed]        := cmSet[e];
cmAny[e_CompoundExpression] := cmCE[e];

cmAny[Verbatim[_]]         := chowing[1, "_"];
cmAny[Verbatim[__]]        := chowing[2, "__"];
cmAny[Verbatim[___]]       := chowing[3, "___"];

cmAny[e_List]              := chowing[2, decStrLen @ rbox["{",  cmListSeq @ e, "}"]];
cmAny[e_Association ? haq] := chowing[2, decStrLen @ rbox[$la, cmAssocSeq @ e, $ra]]; (* TODO: doesn't limit keys *)

cmAny[e_]                  := cmFinal[e];

];

(*************************************************************************************************)

cmInt[0]             := "0";
cmInt[1]             := "1";
cmInt[i_ ? Negative] := "-" <> IntegerString[i];
cmInt[i_]            := IntegerString[i];

(*************************************************************************************************)

(* NOTE: the FE actually does scientific formatting itself for numbers with a final `. it's not customizable somehow. *)
(* use scientific form after 1000, which somehow you can't turn on in the FE by default *)
cmReal[0.] := chowStr @ "0.";
cmReal[r_] := If[0 < Abs[r] < 1000, chowing[5, Internal`MRealToString[r] <> "`"], chowing[10, scientificBoxes @ r]];

(* this fixes a default pathology with the FE in which it doesn't use scientific form sometimes like for 999999.1` but it does for 111111.1`. *)
scientificBoxes[r_] := ReplaceAll[
  ToBoxes[ScientificForm[r, DefaultPrintPrecision -> 3, ExponentStep -> 3]],
  RowBox[{s_String ? (StringEndsQ[".\""]), a1_, a2_}] :> RuleCondition @ RowBox[{StringDelete[s, "."], a1, a2}]
];

(*************************************************************************************************)

SetAttributes[cmSymbol, HoldAllComplete];

cmSymbol[s_] := cmSymbol2[s, SymbolName @ Unevaluated @ s, Context @ s];

SetAttributes[cmSymbol2, HoldFirst];

cmSymbol2[_, name_, "System`" | "Global`" | "GeneralUtilities`"] := chowStr @ name;

cmSymbol2[sym_, name_, context_] :=
  cmSymbolTB[context <> name, chowStr @ name, If[System`Private`HasAnyEvaluationsQ[sym], $ladenSymbolColor, $freeSymbolColor]];

$ladenSymbolColor = RGBColor[0.71, 0.02, 0.];
$freeSymbolColor = RGBColor[0.68, 0.35, 0.];

cmSymbolTB[fullSymName_, symName_, color_] := TemplateBox[
  {fullSymName, symName, color}, "ColoredSymbol",
  DisplayFunction :> Function[DynamicBox @ FEPrivate`If[FrontEnd`CurrentValue["CellStyleName"] === "Input", #1, StyleBox[#2, FontColor -> #3]]],
  CopyFunction    :> Automatic,
  InterpretationFunction :> (Function[#1]),
  Selectable -> False
];

mergeSymTemplates[RowBox[{TemplateBox[{fn_, n_, c_}, a___], s__String}]] :=
  TemplateBox[{fn <> s, n <> s, c}, a];

mergeSymTemplates[RowBox[{s__String, TemplateBox[{fn_, n_, c_}, a___]}]] :=
  TemplateBox[{s <> fn, s <> n, c}, a];

mergeSymTemplates[RowBox[{TemplateBox[{fn1_, n1_, c1_}, a1___], s_String, TemplateBox[{fn2_, n2_, c2_}, a2___]}]] :=
  TemplateBox[{fn1 <> s <> fn2, n1 <> s <> n2, c1}, a];

mergeSymTemplates[other_] := other;

(*************************************************************************************************)

SetAttributes[holdMap, HoldRest];

holdMap[f_, a_] := Map[f, Unevaluated @ a];
holdMap[f_, a_, n_] := First @ Map[f, Take[Hold @ a, All, n], {2}];

(*************************************************************************************************)

SetAttributes[{cmPattern, cmPatternRHS}, HoldRest];

With[{haq = Developer`HoldAtomQ},

cmPattern[Verbatim[Pattern][a_Symbol ? haq, b_]] :=
  mergeSymTemplates @ RowBox @ Prepend[cmSymbol @ a] @ cmPatternRHS[b];

cmPattern[p_] := cmFinal @ p;

(* TODO: chow *)
cmPatternRHS[Verbatim[_]]                      := {"_"};
cmPatternRHS[Verbatim[Blank][b_Symbol ? haq]]  := {"_", cmSymbol @ b};
cmPatternRHS[Verbatim[__]]                     := {"__"};
cmPatternRHS[Verbatim[___]]                    := {"___"};
cmPatternRHS[a_Alternatives]                   := {":", cmAlternatives @ a};
cmPatternRHS[other_]                           := {":", parenBox @ cmAny @ other, ")"};
];

(*************************************************************************************************)

SetAttributes[{cmAlternatives, cmAlternativesEntry}, HoldAllComplete];

cmAlternatives[Verbatim[Alternatives][a_, b__]] := riffMapList[cmAlternativesEntry, "|", {a, b}];
cmAlternatives[e_]                              := cmFinal @ e;

cmAlternativesEntry[e:(_Alternatives | _Rule | _RuleDelayed)] := parenBox @ cmAny @ e;
cmAlternativesEntry[e_] := cmAny @ e;

(*************************************************************************************************)

SetAttributes[{cmSet, cmSetEntry}, HoldAllComplete];

cmSet[Set[a_, b_]] := rbox[cmAny @ a, "=", cmSetEntry @ b];
cmSet[SetDelayed[a_, b_]] := rbox[cmAny @ a, ":=", cmSetEntry @ b];
cmSet[e_] := cmFinal @ e;

cmSetEntry[b:(_SetDelayed | _CompoundExpression)] := parenBox @ cmAny @ b;
cmSetEntry[b_] := cmAny @ b;

(*************************************************************************************************)

SetAttributes[{cmCE, cmCEEntry, riffMapList}, HoldAllComplete];

cmCE[CompoundExpression[a__]] := riffMapList[cmCEEntry, ";", {a}];
cmCE[CompoundExpression[a__, Null]] := RowBox @ Append[";"] @ First @ cmCE[CompoundExpression[a]];
cmCE[e_] := cmFinal @ e;

cmCEEntry[e_CompoundExpression] := parenBox @ cmAny @ e;
cmCEEntry[e_] := cmAny @ e;

riffMapList[fn_, str_, a_] := RowBox @ Riffle[holdMap[fn, a], str];

(*************************************************************************************************)

Clear[cmFinal, cmAnyMap];

SetAttributes[{cmFinal, cmFinal2, plainHeadQ, holdBigQ, holdByteCount, recHeadQ, recHeadQ2}, HoldAllComplete];

$boxRecurse1P = _Pane | _Tooltip | _NicePane | _NiceTooltip | _Annotation;
$boxTypesetP  = _Image | _Graph | _Grid | _NiceGrid | _Column | _NiceMulticolumn | _PlainGrid;

(* TODO: assume thing is square, and so typeset width and height will generate that (width * height) / font size chars *)
With[{haq = Developer`HoldAtomQ, recurse1 = $boxRecurse1P, typeset = $boxTypesetP},
cmFinal[StandaloneSequence[a___]] := cmListSeq[{a}]; (* TODO: decstringlen *)
cmFinal[StandaloneHold[a_]] := cmAny[a];

cmFinal[a:recurse1]         := ToBoxes @ MapAt[cmAny /* RawBoxes, Unevaluated @ a, 1];
cmFinal[Style[a_, rest___]] := StyleBox[cmAny @ a, rest];
cmFinal[Row[a_List]]        := RowBox @ Riffle[holdMap[cmAny, a], ""];
cmFinal[Row[a_List, b_]]    := Construct[cmFinal, Row @ Riffle[holdMap[StandaloneHold, a], StandaloneHold @ b]];
cmFinal[a:typeset]          := "???";
cmFinal[RawBoxes[b_]]       := b;
cmFinal[a_]                 := cmFinal2[a];
];

cmFinal2[(s_Symbol ? recHeadQ)[a___]] := chowing[2, decStrLen @ rbox[cmSymbol @ s, "[", cmListSeq @ {a}, "]"]];
cmFinal2[a_]                          := chowing[holdByteCount[a] / 4, cleanupManualBoxes @ MakeBoxes[a, StandardForm]];

cleanupManualBoxes[e_] := e //. FractionBox[a_, b_] :> RowBox[{a, "/", b}];
(* TODO: find way of disabling fraction box *)

recHeadQ2[s_] := EchoLabel[Hold[s]] @ recHeadQ @ s;

With[
{allowedRecHeads = Hold | HoldComplete | Splice | Sequence},
recHeadQ[allowedRecHeads] := True;
recHeadQ[_ ? System`Private`HasPrintCodeQ] := False;
recHeadQ[_ ? System`Private`HasUpEvaluationsQ] := MemberQ[$whitelistEntry, SymbolName @ Unevaluated @ s];
recHeadQ[_Symbol ? Developer`HoldAtomQ] := True;
];

$whitelistEntry = {"Scope", "ModuleScope", "Case"};

$bigSize = 2000;
holdByteCount[e_] := ByteCount[Unevaluated[e]];
holdBigQ[e_] := ByteCount[Unevaluated[e]] > $bigSize;

(*************************************************************************************************)

SetAttributes[{chowing, chowing2}, HoldRest];

chowing[i_, e_]   := If[($cmLeft -= i) >= 0, e, $cmEll];
chowing[i_, e_]   := If[($cmLeft -= i) >= 0, e, $cmEll];

chowing2[i_, e_, f_] := If[($cmLeft -= i) >= 0, e, f];

SetAttributes[cmAnyComma, HoldAllComplete];
cmAnyComma[e_] /; $cmLeft <= 0 := If[$trig, $trig = False; $cmEll, Nothing];
cmAnyComma[e_] := Splice[{cmAny[e], $cmLeft -= 2; ","}];

commaRowBox[a:{___, ","}]            := commaRowBox @ Most @ a;
commaRowBox[a:{___, $cmEll, $cmEll}] := commaRowBox @ Most @ a;
commaRowBox[a_List]                  := RowBox[a];

safeMost[{}] := {};
safeMost[list_] := Most @ list;

chowStr[e_String] := chowing[StringLength @ e, e];

leftFor[n_]       := Floor[$cmLeft / n];

(*************************************************************************************************)

rbox[b___]        := RowBox[{b}];

commasBox[b_]     := RowBox @ Riffle[b, ","];

parenBox[b_]      := chowing[2, rbox["(", b, ")"]];

(*************************************************************************************************)

Clear[cmString];

SetAttributes[decStrLen, HoldFirst];

decStrLen[body_] := Block[{$cmStrMax = Max[Floor[$cmStrMax / 3], 3]}, body];

cmString[s_] /; StringLength[s] > $cmStrMax := cmString[StringTake[s, $cmStrMax-1] <> $cmEll];

cmString[s_] := ToBoxes @ With[{left = $cmLeft}, chowing2[StringLength[s] + 2, s, StringTake[s, UpTo @ Max[left-3, 0]] <> $cmEll]];

$cmEll = "\[Ellipsis]";

(*************************************************************************************************)

SetAttributes[{cmRule, ruleBox}, HoldAllComplete];

cmRule[head_[a:(_Rule | _RuleDelayed), b_]] := ruleBox[head, parenBox @ cmRule @ a, cmAny @ b];
cmRule[head_[a_, b_]]                       := ruleBox[head, cmAny @ a, cmAny @ b];
cmRule[e_]                                  := cmFinal @ e;

ruleBox[Rule,        a_, b_] := chowing[4, rbox[a, "\[Rule]", b]];
ruleBox[RuleDelayed, a_, b_] := chowing[4, rbox[a, "\[RuleDelayed]", b]];

(*************************************************************************************************)

SetAttributes[{cmListSeq, cmListSeqEntries}, HoldAllComplete];

cmListSeq[{}]                  := Nothing;
cmListSeq[{a_}]                := cmAny @ a;
cmListSeq[{a_, b_}]            := chowing[2, rbox[cmAny @ a, ",", cmAny @ b]];
cmListSeq[l_List]           := With[
  {n = Length @ Unevaluated @ l},
  $trig = True; commaRowBox @ cmListSeqEntries[l, n, leftFor[3]]
];

cmListSeqEntries[l_, n_, m_] /; n <= m :=
  holdMap[cmAnyComma, l];

cmListSeqEntries[l_, n_, m_] :=
  Append[$cmEll] @ holdMap[cmAnyComma, l, m];

(*************************************************************************************************)

SetAttributes[{cmAssocSeq, cmAssocEntries, cmAssocEntry, cmKVRule}, HoldAllComplete];

cmAssocSeq[a_] := With[
  {n = Length @ Unevaluated @ a},
  Which[
    n == 0, Nothing,
    n == 1, First @ KeyValueMap[cmKVRule, Unevaluated @ a],
    True,   $trig = True; commaRowBox @ cmAssocEntries[a, n, leftFor[7]]
  ]
];

cmAssocEntries[a_, n_, m_] /; n < m :=
  KeyValueMap[cmKVRule, Unevaluated @ a];

cmAssocEntries[a_, n_, m_] := Block[
  {$i = 1, $m = m, $cmbag = Internal`Bag[]},
  Association`ScanWhile[Unevaluated @ a, cmAssocEntry];
  Internal`StuffBag[$cmbag, $cmEll];
  Internal`BagPart[$cmbag, All]
];

cmKVRule[k_, v_] := cmRule[k -> v];

cmAssocEntry[r_] := (Internal`StuffBag[$cmbag, cmAnyComma @ r]; ++$i <= $m);

(*************************************************************************************************)

Clear[InputFormLength];

Options[InputFormLength] = {
  PrintPrecision -> Infinity
};

InputFormLength[e_, opts___] := chars0[e, opts];
InputFormLength[NicePane[e_, ___], opts___] := chars0[e, opts];

InputFormLength::invalidOptions = "Non-rules provided as options to InputFormLength.";

$pp = Infinity;
chars0[e_, opts___Rule] := Block[
  {$pp = Lookup[{opts}, PrintPrecision, Infinity]},
  chars[e]
];

chars0[e_, ___] := chars0[Message[InputFormLength::invalidOptions]; e];

(*************************************************************************************************)

ClearAll[chars, ruleChars, runChars, commaChars, chars2, manualChars];
SetAttributes[{chars, ruleChars, runChars, commaChars, chars2, manualChars}, HoldAllComplete];

With[{haq = Developer`HoldAtomQ},
chars[0]                            := 1;
chars[True]                         := 4;
chars[None]                         := 4;
chars[False]                        := 5;
chars[a_String ? haq]               := StringLength[a] + 2;
chars[a_Symbol ? haq]               := StringLength @ SymbolName @ Unevaluated @ a;
chars[a_Integer ? haq]              := intChars @ a;
chars[a_Real ? haq]                 := realChars @ a;

chars[{}]                           := 2;
chars[a_List]                       := 2 + runChars[a];

chars[(Rule|RuleDelayed)[a_, b_]]   := ruleChars[a, b];

chars[a_Association ? haq]          := 4 + Total[KeyValueMap[ruleChars, Unevaluated @ a]];

chars[a_ ? System`Private`NoEntryQ] := Infinity;
chars[Verbatim[Condition][a_, b_]]  := chars[a] + chars[b] + 4;
chars[Verbatim[Pattern][a_, b_]]    := chars[a] + chars[b] + 3;
chars[Verbatim[_]]                  := 1;
chars[Verbatim[__]]                 := 2;
chars[Verbatim[___]]                := 3;

chars[MathTools`MsgPath[p_String ? haq]] := Min[StringLength[p], 40] + 2;
chars[RawBoxes[b_]]                 := 4; (* i'm using this in NiceEcho, don't want to drop these guys *)
chars[a_]                           := If[holdBigQ[a], Infinity, chars2 @ a];

];

(*************************************************************************************************)

With[
  {recurse1 = $boxRecurse1P, typeset = $boxTypesetP, haq = Developer`HoldAtomQ},
  chars2[a:recurse1]        := MapAt[chars, Unevaluated @ a, 1];
  chars2[typeset ? haq]     := Infinity;
  chars2[a_]                := manualChars @ a;
];

manualChars[a_]             := StringLength[ToString[Unevaluated @ a, InputForm]];

(*************************************************************************************************)

(* the extra 5 is for * 10^n *)
realChars[r_ ? Developer`MachineRealQ] := Min[StringLength @ ToString[r, InputForm], If[Abs[r] >= 1000, 5, 0] + 1 + $pp];
realChars[r_]                          :=     StringLength @ ToString[r, InputForm];

(*************************************************************************************************)

intChars[i_] := Which[
    -9 < i < 9,   1,
   -99 < i < 99,  2,
  -999 < i < 999, 3,
  True,           If[Negative[i], 1, 0] + Ceiling[Log10[Abs[i] + 1/10]]
];

ruleChars[a_, b_] := chars[a] + chars[b] + 4;

runChars[{}]           := 0;
runChars[{a_}]         := chars[a];
runChars[{a_, b_}]     := 2 + chars[a] + chars[b];
runChars[{a_, b_, c_}] := 4 + chars[a] + chars[b] + chars[c];
runChars[a_List]       := Plus[Total @ Map[chars, Unevaluated @ a], commaChars[a]];

commaChars[a_List]     := 2 * (Length[Unevaluated @ a] - 1);

End[];