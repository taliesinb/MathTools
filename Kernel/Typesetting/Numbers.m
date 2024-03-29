PublicFunction[CompactNumberBox]

Options[CompactNumberBox] = $compactNumberOptions;

CompactNumberBox[expr_, OptionsPattern[]] := Scope[
  UnpackOptions[negationStyle, inversionStyle];

  blockNumberFormatting[
    CompactNumberBox, {negationStyle, inversionStyle},
    iCompactNumberBox[expr]
  ]
];

iCompactNumberBox[expr_] := numBox[expr] // simplifyNumBoxes;

(**************************************************************************************************)

PublicFunction[CompactNumberForm]

Options[CompactNumberForm] = $compactNumberOptions;

MakeBoxes[CompactNumberForm[expr_, opts:OptionsPattern[]], form_] := Scope[

  {negationStyle, inversionStyle} = OptionValue[CompactNumberForm, {opts}, {NegationStyle, InversionStyle}];

  blockNumberFormatting[CompactNumberForm, {negationStyle, inversionStyle},
    held = Hold[expr] /. numExpr:(Times|Plus|Power|Sqrt)[___] :> RuleCondition[RawBoxes @ iCompactNumberBox @ numExpr];
    held = held /. {
      b_RawBoxes :> b,
      number_ ? System`Dump`HeldNumericQ :> RuleCondition[RawBoxes @ iCompactNumberBox @ number]
    };
  ];

  MakeBoxes @@ held
];

(**************************************************************************************************)

$vertLineBox = AdjustmentBox["\[VerticalLine]", BoxBaselineShift -> -0.2];
$horLineBox = "\[HorizontalLine]";

$invSep = "\"\[InvisibleSpace]\"";
$vthinSep = "\"\[VeryThinSpace]\"";
$thinSep = "\"\[ThinSpace]\"";

RowSeq[args___] := TemplateBox[{args}, "RowDefault"];
RowSep[{args___}, sep_] := RowSeqSep[args, sep];
RowSeqSep[args___, sep_] := TemplateBox[{sep, "", args}, "RowWithSeparators"];

(**************************************************************************************************)

tradBox[b_] := StyleBox[FormBox[b, TraditionalForm], ShowStringCharacters -> False];

(**************************************************************************************************)

PublicTypesettingForm[LeftBar, RightBar]

SetUsage @ "LeftBar[x$] typesets as a vertical line to the left of x$."
SetUsage @ "RightBar[x$] typesets as a vertical line to the right of x$."

MakeBoxes[LeftBar[x_], form_] := leftbarBox @ MakeBoxes[x, form]
MakeBoxes[RightBar[x_], form_] := rightbarBox @ MakeBoxes[x, form];

leftbarBox[box_] := RowBox[{$vertLineBox, box}];
rightbarBox[box_] := RowBox[{box, $vertLineBox}];

(**************************************************************************************************)

$minusNegation = True;
$overbarNegation = $underbarNegation = $leftbarNegation = $barNegation = $colorNegation = False;

$normalInversion = True;
$overbarInversion = $underbarInversion = $leftbarInversion = $barInversion = $colorInversion = False;

$grayDot = StyleBox["\[CenterDot]", Gray];
$zero = "0";
$one = "1";
$imag = "\[ImaginaryI]";
$minus = "\[Minus]";
$plus = "+";
$prodSpace = "\[VeryThinSpace]";

rowBox[e__] := RowBox[{e}];
rowBox[rb_RowBox] := rb;

parenBox[e_] := rowBox["(", e, ")"];

(* maybeParenBox decides if a box contains infix syntax that
requires wrapping in parens to be unambigious, and applies it if so. *)
maybeParenBox = Case[
  b:parenBox[__] := b;
  b_ /; FreeQ[b, "+" | "/"] := b;
  b_ := parenBox @ b;
];

maybeParenBox2 = Case[
  b:parenBox[__] := b;
  b_ /; FreeQ[b, "+" | "/" | $vertLineBox | $prodSpace] := b;
  b_ := parenBox @ b;
];


adjustBox[e_, n_] := AdjustmentBox[e, BoxBaselineShift -> n];

$horBarBox = "_";
$overbar = adjustBox[$horBarBox, -1/5];
$underbar = adjustBox[$horBarBox, 1/20];

overbarBox[b_] := OverscriptBox[b, $overbar];
overbarBox[overbarBox[b_]] := b;

underbarBox[b_] := UnderscriptBox[b, $underbar];
underbarBox[underbarBox[b_]] := b;

overleftbarBox[b_] := GridBox[{{b}},
  ColumnAlignments -> {Left}, BaselinePosition -> {{1, 1}, Baseline},
  GridBoxDividers -> {"Columns" -> {True, False}, "Rows" -> {True, False}},
  GridBoxSpacings -> {"Columns" -> {{0.3}}, "Rows" -> {{0.3}}},
  GridBoxItemSize -> {"Columns" -> {{All}}, "Rows" -> {{Automatic}}}
]

underleftbarBox[b_] := GridBox[{{b}},
  ColumnAlignments -> {Left}, BaselinePosition -> {{1, 1}, Baseline},
  GridBoxDividers -> {"Columns" -> {True, False}, "Rows" -> {False, True}},
  GridBoxSpacings -> {"Columns" -> {{0.2}}, "Rows" -> {{0.0}}},
  GridBoxItemSize -> {"Columns" -> {{All}}, "Rows" -> {{Automatic}}}
]

(*redBox[b_] := StyleBox[b, $Red];*)
redBox[redBox[b_]] := b;

(*blueBox[b_] := StyleBox[b, $Blue];*)
blueBox[blueBox[b_]] := b;

minusBox = Case[
  rowBox[$minus, b_] := b;
  b_ := rowBox[$minus, b];
];

(**************************************************************************************************)

(* negBox applies negative styling *)
negBox[e_] /; $colorNegation := redBox @ e;
negBox[e_] /; $overbarNegation := overbarBox @ e;
negBox[e_] /; $underbarNegation := underbarBox @ e;
negBox[e_] /; $leftbarNegation := leftbarBox @ e;
negBox[e_] := minusBox @ maybeParenBox @ e;

(* maybeNegBox applies negative styling if first arg is negative *)
maybeNegBox[value_, box_] :=
  If[TrueQ @ Negative[value], negBox @ box, box];

(* prodBox creates a box two multiple two boxes *)
prodBox[a_] := a;x
prodBox[a_, b_] := sortProdTerms[{maybeParenBox @ a, maybeParenBox @ b}];
prodBox[first_, rest__] := sortProdTerms @ Prepend[Map[maybeParenBox, {rest}], maybeParenBox @ first];

(* prodTerm = MatchValues[
  b:leftbarBox[_] := maybeParenBox @ b;
  b_ := Splice[{$prodSpace, maybeParenBox @ b}]
];
 *)

prodRow[{}] := {};
prodRow[{e_}] := e;
prodRow[e_List] := RowBox @ Riffle[e, $prodSpace];

sortProdTerms[e_List] /; $barInversion := Scope[
  {inverted, notInverted} = SelectDiscard[e, barInvQ];
  inverted = stripBarInv /@ inverted;
  inverted //= prodRow; notInverted //= prodRow;
  Which[
    inverted === {}, notInverted,
    notInverted === {}, barInvBox @ inverted,
    True, rowBox[notInverted, barInvBox @ inverted]
  ]
];

sortProdTerms[e_List] := prodRow[e];

(**************************************************************************************************)

(* sumBox creates a box to add two or more boxes.
1. it will remove an inline negation, using a minus seperator instead *)
sumBox[a_] := a;
sumBox[a_, b_] := RowBox[{a, sumTerm @ b}];
sumBox[first_, rest__] := RowBox @ Prepend[Map[sumTerm, {rest}], first];

sumTerm = MatchValues[
  rowBox[$minus, b_] := Splice[{$minus, b}];
  b_ := Splice[{$plus, b}]
];

(**************************************************************************************************)

(* ratBox creates a box for a rational number, handles pos or neg rationals *)
ratBox[r_] := maybeNegBox[r, posRatBox @ Abs @ r];

(* posRatBox assumes positive rationals, negation handled by ratBox *)
posRatBox = MatchValues[
  Rational[1, b_] := invBox[numBox @ b];
  Rational[a_, b_] := divBox[numBox @ a, numBox @ b];
];

(**************************************************************************************************)

barInvBox[b_] /; $leftbarInversion := leftbarBox @ b;
barInvBox[b_] /; $overbarInversion := overbarBox @ b;
barInvBox[b_] /; $underbarInversion := underbarBox @ b;
barInvBox[b_] /; $colorInversion := blueBox @ b;

supInvBox[a_, b_] :=
  SuperscriptBox[maybeParenBox2 @ a, barInvBox @ b]

barInvQ[leftbarBox[_]] /; $leftbarInversion = True;
barInvQ[overbarBox[_]] /; $overbarInversion = True;
barInvQ[underbarBox[_]] /; $underbarInversion = True;
barInvQ[blueBox[_]] /; $colorInversion = True;
barInvQ[_] := False;

stripBarInv[leftbarBox[b_]] /; $leftbarInversion := b;
stripBarInv[overbarBox[b_]] /; $overbarInversion := b;
stripBarInv[underbarBox[b_]] /; $underbarInversion := b;
stripBarInv[blueBox[b_]] /; $colorInversion := b;
stripBarInv[b_] := b;

(* invBox creates a box for 1/b *)
invBox[b_] /; !$normalInversion := barInvBox @ barParenBox @ b;
invBox[b_] := divBox[$one, maybeParenBox @ b];

(**************************************************************************************************)

barParenBox[b_] /; $leftbarInversion := maybeParenBox[b];
barParenBox[b_] := b;

(* divBox formats a fraction. handles:
1. a / b for numBox, which comes as Times[a, Power[b, -1]]
2. m / n for posRatBox, where m and n will be pos int strings
3. 1 / b for invBox.
*)
divBox[a_, b_] /; !$normalInversion = prodBox[maybeParenBox @ a, barInvBox @ barParenBox @ b];
divBox[a_, b_] := shortDiv @ rowBox[maybeParenBox @ a, "/", maybeParenBox @ b];
shortDiv[RowBox[s:{_Str, "/", _Str}]] := StyleBox[StringJoin[s], AutoSpacing -> False];
shortDiv[e_] := e;

(**************************************************************************************************)

(* sqrtBox, surdBox, radicalBox handle the corresponding expressions, dispatched from numBox *)
sqrtBox[e_] /; !$normalInversion := supInvBox[e, "2"];
sqrtBox[e_] := SqrtBox[e];

surdBox[a_, b_] /; !$normalInversion := supInvBox[a, b];
surdBox[a_, b_] := RadicalBox[a, b, SurdForm -> True];

radicalBox[a_, b_] /; !$normalInversion := supInvBox[a, b];
radicalBox[a_, b_] := RadicalBox[a, b, SurdForm -> False];

(**************************************************************************************************)

(* powerBox handles Power, dispatched from numBox *)
powerBox = MatchValues[
  Power[-1, Rational[a_, b_]] := SubsuperscriptBox["\[Xi]", numBox @ b, numBox @ a];
  Power[a_, Rational[1, 2]] := sqrtBox @ numBox @ a;
  Power[a_, Rational[1, b_]] := radicalBox[numBox @ a, numBox @ b];
  Power[a_, -1] := invBox @ numBox @ a;
  Power[a_, b_] := SuperscriptBox[numBox @ a, numBox @ b];
];

(**************************************************************************************************)

(* realbox formats a decimal string *)
realBox[r_] := TextString[NumberForm[r, 2]];

(* imagBox handles the imaginary part of a complex number, handles pos or neg *)
imagBox[im_] := maybeNegBox[im, posImagBox @ Abs @ im];

(**************************************************************************************************)

posImagBox = MatchValues[
  1|1. := $imag;
  0|0. := $zero;
  (* Rational[1, iim_] := rowBox[$imag, numBox @ iim]; *)
  im_ := prodBox[numBox @ im, $imag];
];

(**************************************************************************************************)

(* complexBox handles complex numbers, with special cases for various simple ones *)
complexBox = MatchValues[
  I := $imag;
  -I := negBox @ $imag;
  Complex[0|0., imag_] := imagBox @ imag;
  Complex[re_, 0|0.] := numBox @ re;
  Complex[re_, im_] := sumBox[numBox @ re, imagBox @ im];
];

(**************************************************************************************************)

intBox[n_] := maybeNegBox[n, posIntBox @ Abs @ n];
posIntBox[i_] := IntegerString @ Abs @ i;

(**************************************************************************************************)

PrivateFunction[numBox]

numBox = MatchValues[

  0|0. := $zero;
  1|1. := $one;
  n_Int := intBox[n];
  Times[-1, e_] := negBox @ numBox @ e;

  r_Rational := ratBox[r];

  Sqrt[b_] := sqrtBox @ numBox @ b;
  Surd[a_, b_] := surdBox[numBox @ a, numBox @ b];

  p_Power := powerBox[p];
  r_Real := realBox[r];
  p_Plus := Apply[sumBox, Map[numBox, List @@ p]];

  (* purpose here is to simplify reduced sqrts *)
  Times[r_Rational, Sqrt[b_]] := maybeNegBox[r, sqrtBox @ numBox[r^2 * b]];
  Times[i_Int, Sqrt[b_]] := maybeNegBox[i, sqrtBox @ numBox[i^2 * b]];

  (* purpose here is to put the complex number later *)
  Times[Complex[0, r_Rational], b_] := prodBox[numBox[r * b], $imag];
  Times[complex_Complex, other_] := prodBox[numBox @ other, numBox @ complex];

  c_Complex := complexBox[c];

  Times[a_, b_, c__] := prodBox @@ Map[numBox, {a, b, c}];
  Times[a:Except[Power[_, -1] | Rational[1, _]], Power[b_, -1]] := divBox[numBox @ a, numBox @ b];
  Times[a_, b_] := prodBox[numBox @ a, numBox @ b];
  Times[a_] := numBox[a];

  r_UnitRoot := ToBoxes[r];

  ModForm[0, _] := $zero;
  ModForm[a_, b_] := modBox[% @ a, b];

(*   sym_Symbol := ToBoxes[sym];
  sym_Symbol[args___] := RowBox[{ToBoxes[sym], "[", RowBox @ Riffle[Map[numBox, {args}], ","], "]"}];

 *)
  e_ := ToBoxes[e];
];

(**************************************************************************************************)

$sqrtSupBox = StyleBox[$minus, FontWeight -> "Bold"];

PrivateFunction[simplifyNumBoxes]

simplifyNumBoxes[boxes_] := ReplaceRepeated[boxes, {
  RowBox[s:{__Str}] :> RuleCondition @ StringJoin[s],
  overbarBox[leftbarBox[e_]] :> overleftbarBox[e], leftbarBox[overbarBox[e_]] :> overleftbarBox[e],
  underbarBox[leftbarBox[e_]] :> underleftbarBox[e], leftbarBox[underbarBox[e_]] :> underleftbarBox[e],
  overbarBox[b_SuperscriptBox] :> underbarBox[b],
  zRowBox[{a_, RowBox[{b_AdjustmentBox, c_}]}] :> RowBox[{a, b, c}],
  zRowBox[{RowBox[{a_AdjustmentBox, b_}], c_}] :> RowBox[{a, b, c}],
  If[$underbarInversion, SuperscriptBox[a_, underbarBox["2"]] :> SuperscriptBox[a, $sqrtSupBox], Nothing],
  If[$underbarInversion && $colorNegation, SuperscriptBox[a_, redBox[underbarBox["2"]]] :> SuperscriptBox[a, redBox[$sqrtSupBox]], Nothing],
  SuperscriptBox[a_, UnderscriptBox[b_, AdjustmentBox[c_, _]]] :> SuperscriptBox[a, UnderscriptBox[b, c]],
  UnderscriptBox[OverscriptBox[b_, o_], u_] :> UnderoverscriptBox[b, u, o],
  OverscriptBox[UnderscriptBox[b_, u_], o_] :> UnderoverscriptBox[b, u, o]
}] // ReplaceRepeated[{
  b_redBox :> RuleCondition[evalColorBox[b]],
  b_blueBox :> RuleCondition[evalColorBox[b]]
}];

evalColorBox = MatchValues[
  redBox[parenBox[b__]] := rowBox[red @ "(", b, red @ ")"];
  redBox[rowBox[first_, rest__]] := rowBox[evalColorBox[redBox[first]], rest];
  redBox[SuperscriptBox[a_, b_]] := SuperscriptBox[evalColorBox[redBox[a]], b];
  redBox[SubscriptBox[a_, b_]] := SubscriptBox[evalColorBox[redBox[a]], b];
  redBox[OverscriptBox[a_, b_]] := OverscriptBox[evalColorBox[redBox[a]], b];
  redBox[UnderscriptBox[a_, b_]] := UnderscriptBox[evalColorBox[redBox[a]], b];
  redBox[redBox[b_]] := b;
  redBox[b_] := red[b];
  e_ := e;
];

red[e_] := StyleBox[e, $Red];

(**************************************************************************************************)

(* returns overbar, underbar, leftbar, underOrOver, color, normal *)
processStyleSpec = MatchValues[
  OverBar   :=  {True,  False, False, True,  False, False};
  UnderBar   := {False, True,  False, True,  False, False};
  LeftBar   :=  {False, False, True,  False, False, False};
  "Color" :=    {False, False, False, False, True,  False};
  None :=       {False, False, False, False, False, True};
];

General::confstylespec =
  "The specifications `` for negation and `` for inversion conflict."

(**************************************************************************************************)

PrivateFunction[blockNumberFormatting]

SetAttributes[blockNumberFormatting, HoldRest];

blockNumberFormatting[head_, {negationStyle_, inversionStyle_}, body_] := Scope[

  {$overbarNegation, $underbarNegation, $leftbarNegation, $barNegatin, $colorNegation, $minusNegation} =
    $nspec = processStyleSpec[negationStyle];

  {$overbarInversion, $underbarInversion, $leftbarInversion, $barInversion, $colorInversion, $normalInversion} =
    $ispec = processStyleSpec[inversionStyle];

  If[$nspec === $ispec,
    Message[MessageName[head, "confstylespec"], negationStyle, inversionStyle];
    ReturnFailed[];
  ];

  body
];
