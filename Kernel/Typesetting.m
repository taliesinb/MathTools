(**************************************************************************************************)

PackageExport["$MathFont"]

$MathFont = "STIX Two Math";

$vertLineBox = AdjustmentBox["\[VerticalLine]", BoxBaselineShift -> -0.2];
$horLineBox = "\[HorizontalLine]";

$invSep = "\"\[InvisibleSpace]\"";
$vthinSep = "\"\[VeryThinSpace]\"";
$thinSep = "\"\[ThinSpace]\"";

RowSeq[args___] := TemplateBox[{args}, "RowDefault"];
RowSep[{args___}, sep_] := RowSeqSep[args, sep];
RowSeqSep[args___, sep_] := TemplateBox[{sep, "", args}, "RowWithSeparators"];
GrayBox[e_] := StyleBox[e, $Gray];

(**************************************************************************************************)

tradBox[b_] := StyleBox[FormBox[b, TraditionalForm], ShowStringCharacters -> False];

(**************************************************************************************************)

PackageExport["LeftBar"]

SetUsage @ "
LeftBar[x$] typesets as a vertical line to the left of x$.
"

MakeBoxes[LeftBar[x_], form_] := leftbarBox @ MakeBoxes[x, form]

leftbarBox[box_] := RowBox[{$vertLineBox, box}];

(**************************************************************************************************)

PackageExport["RightBar"]

SetUsage @ "
RightBar[x$] typesets as a vertical line to the right of x$.
"

MakeBoxes[RightBar[x_], form_] := rightbarBox @ MakeBoxes[x, form];

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
maybeParenBox = MatchValues[
  b:parenBox[__] := b;
  b_ /; FreeQ[b, "+" | "/"] := b;
  b_ := parenBox @ b;
];

maybeParenBox2 = MatchValues[
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

minusBox = MatchValues[
  rowBox[$minus, b_] := b;
  b_ := rowBox[$minus, b];
];


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


(* sumBox creates a box to add two or more boxes.
1. it will remove an inline negation, using a minus seperator instead *)
sumBox[a_] := a;
sumBox[a_, b_] := RowBox[{a, sumTerm @ b}];
sumBox[first_, rest__] := RowBox @ Prepend[Map[sumTerm, {rest}], first];

sumTerm = MatchValues[
  rowBox[$minus, b_] := Splice[{$minus, b}];
  b_ := Splice[{$plus, b}]
];

(* ratBox creates a box for a rational number, handles pos or neg rationals *)
ratBox[r_] := maybeNegBox[r, posRatBox @ Abs @ r];

(* posRatBox assumes positive rationals, negation handled by ratBox *)
posRatBox = MatchValues[
  Rational[1, b_] := invBox[numBox @ b];
  Rational[a_, b_] := divBox[numBox @ a, numBox @ b];
];

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

barParenBox[b_] /; $leftbarInversion := maybeParenBox[b];
barParenBox[b_] := b;

(* divBox formats a fraction. handles:
1. a / b for numBox, which comes as Times[a, Power[b, -1]]
2. m / n for posRatBox, where m and n will be pos int strings
3. 1 / b for invBox.
*)
divBox[a_, b_] /; !$normalInversion = prodBox[maybeParenBox @ a, barInvBox @ barParenBox @ b];
divBox[a_, b_] := shortDiv @ rowBox[maybeParenBox @ a, "/", maybeParenBox @ b];
shortDiv[RowBox[s:{_String, "/", _String}]] := StyleBox[StringJoin[s], AutoSpacing -> False];
shortDiv[e_] := e;

(* sqrtBox, surdBox, radicalBox handle the corresponding expressions, dispatched from numBox *)
sqrtBox[e_] /; !$normalInversion := supInvBox[e, "2"];
sqrtBox[e_] := SqrtBox[e];

surdBox[a_, b_] /; !$normalInversion := supInvBox[a, b];
surdBox[a_, b_] := RadicalBox[a, b, SurdForm -> True];

radicalBox[a_, b_] /; !$normalInversion := supInvBox[a, b];
radicalBox[a_, b_] := RadicalBox[a, b, SurdForm -> False];

(* powerBox handles Power, dispatched from numBox *)
powerBox = MatchValues[
  Power[-1, Rational[a_, b_]] := SubsuperscriptBox["\[Xi]", numBox @ b, numBox @ a];
  Power[a_, Rational[1, 2]] := sqrtBox @ numBox @ a;
  Power[a_, Rational[1, b_]] := radicalBox[numBox @ a, numBox @ b];
  Power[a_, -1] := invBox @ numBox @ a;
  Power[a_, b_] := SuperscriptBox[numBox @ a, numBox @ b];
];

(* realbox formats a decimal string *)
realBox[r_] := TextString[NumberForm[r, 2]];

(* imagBox handles the imaginary part of a complex number, handles pos or neg *)
imagBox[im_] := maybeNegBox[im, posImagBox @ Abs @ im];

posImagBox = MatchValues[
  1|1. := $imag;
  0|0. := $zero;
  (* Rational[1, iim_] := rowBox[$imag, numBox @ iim]; *)
  im_ := prodBox[numBox @ im, $imag];
];

(* complexBox handles complex numbers, with special cases for various simple ones *)
complexBox = MatchValues[
  I := $imag;
  -I := negBox @ $imag;
  Complex[0|0., imag_] := imagBox @ imag;
  Complex[re_, 0|0.] := numBox @ re;
  Complex[re_, im_] := sumBox[numBox @ re, imagBox @ im];
];

intBox[n_] := maybeNegBox[n, posIntBox @ Abs @ n];
posIntBox[i_] := IntegerString @ Abs @ i;

numBox = MatchValues[

  0|0. := $zero;
  1|1. := $one;
  n_Integer := intBox[n];
  Times[-1, e_] := negBox @ numBox @ e;

  r_Rational := ratBox[r];

  Sqrt[b_] := sqrtBox @ numBox @ b;
  Surd[a_, b_] := surdBox[numBox @ a, numBox @ b];

  p_Power := powerBox[p];
  r_Real := realBox[r];
  p_Plus := Apply[sumBox, Map[numBox, List @@ p]];

  (* purpose here is to simplify reduced sqrts *)
  Times[r_Rational, Sqrt[b_]] := maybeNegBox[r, sqrtBox @ numBox[r^2 * b]];
  Times[i_Integer, Sqrt[b_]] := maybeNegBox[i, sqrtBox @ numBox[i^2 * b]];

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
  sym_Symbol := ToBoxes[sym];
  sym_Symbol[args___] := RowBox[{ToBoxes[sym], "[", RowBox @ Riffle[Map[numBox, {args}], ","], "]"}];

  e_ := ToBoxes[e];
];


PackageExport["NegationStyle"]

SetUsage @ "
NegationStyle is an option to CompactNumberForm and other functions.
"

PackageExport["InversionStyle"]

SetUsage @ "
InversionStyle is an option to CompactNumberForm and other functions.
"

$compactNumberOptions = {
  NegationStyle -> "Color",
  InversionStyle -> UnderBar
};

$sqrtSupBox = StyleBox[$minus, FontWeight -> "Bold"];

simplifyNumBoxes[boxes_] := ReplaceRepeated[boxes, {
  RowBox[s:{__String}] :> RuleCondition @ StringJoin[s],
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

(**************************************************************************************************)

PackageScope["ModForm"]

ModForm[x_, Infinity|0] := x;

declareBoxFormatting[
  ModForm[a_, b_] :> modBox[MakeBoxes @ a, b],
  ModForm[a_, b_List] :> RowBox[{MakeBoxes @ a, " % ", StyleBox[MakeBoxes @ b, $Blue]}]
];

modBox[a_, b_] := SubscriptBox[a, StyleBox[numBox @ b, $Blue]];

(**************************************************************************************************)

PackageExport["CompactNumberBox"]

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

PackageExport["CompactNumberForm"]

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

PackageExport["CompactMatrixBox"]

$compactMatrixOptions = JoinOptions[
  $compactNumberOptions,
  ItemSize -> Automatic, FrameStyle -> GrayLevel[0.85], "Factor" -> True, "HideZeros" -> True
];

Options[CompactMatrixBox] = $compactMatrixOptions;

expandItemSize[Automatic, matrix_] := {0.65, 0.2};

expandItemSize[num_ ? NumericQ, _] := {N @ num, 0.3};
expandItemSize[num:{_ ? NumericQ, _ ? NumericQ}, _] := {num, num};
expandItemSize[_, _] := {0.6, 0.3};

CompactMatrixBox[matrix_, OptionsPattern[]] := Scope[
  UnpackOptions[negationStyle, inversionStyle, itemSize, frameStyle, factor, hideZeros];

  blockNumberFormatting[CompactMatrixBox, {negationStyle, inversionStyle},
    $zero = If[hideZeros, $grayDot, "0"];
    iCompactMatrixBox[matrix, itemSize, frameStyle, factor]
  ]
];

iCompactMatrixBox[matrix_, itemSize_, frameStyle_, shouldFactor_] := Scope[
  {matrix, factor} = If[shouldFactor && Dimensions[matrix] =!= {1, 1} &&
    Min[Abs[ExpandUnitRoots[matrix] /. ModForm[m_, _] :> m]] > 0,
    MatrixSimplify[matrix], {matrix, 1}];
  entries = Map[numBox, matrix, {2}] // simplifyNumBoxes;
  matrixBoxes = matrixGridBoxes[entries, expandItemSize[itemSize, matrix], frameStyle];
  If[factor === 1, matrixBoxes,
    RowBox[{matrixBoxes, numBox[factor] // simplifyNumBoxes}]
  ]
];

matrixGridBoxes[entries_, {w_, h_}, frameStyle_] := GridBox[entries,
  GridBoxFrame -> {"ColumnsIndexed" -> {{{1, -1}, {1, -1}} -> True}},
  GridBoxAlignment -> {"Columns" -> {{Center}}},
  GridBoxItemSize -> {"Columns" -> {{All}}, "Rows" -> {{All}}},
  GridBoxSpacings -> {"Columns" -> {{w}}, "Rows" -> {0.7, {h}, 0.1}},
  BaseStyle -> {FontFamily -> "Source Code Pro", FontSize -> 12, TextAlignment -> Left},
  FrameStyle -> frameStyle
];

(**************************************************************************************************)

PackageExport["CompactMatrixForm"]

SetUsage @ "
CompactMatrixForm
";

Options[CompactMatrixForm] = Options[makeCompactMatrixFormBoxes] = $compactMatrixOptions;

declareBoxFormatting[
  CompactMatrixForm[e_, opts___Rule] :> makeCompactMatrixFormBoxes[e, opts]
];

makeCompactMatrixFormBoxes[e_, opts:OptionsPattern[]] := Scope[

  {negationStyle, inversionStyle, itemSize, frameStyle, factor, hideZeros} =
    OptionValue[CompactMatrixForm, {opts}, {NegationStyle, InversionStyle, ItemSize, FrameStyle, "Factor", "HideZeros"}];

  blockNumberFormatting[CompactMatrixForm, {negationStyle, inversionStyle},
    $zero = If[hideZeros, $grayDot, "0"];
    If[MatrixQ[Unevaluated[e]],
      held = List @ RawBoxes @ iCompactMatrixBox[e, itemSize, frameStyle, factor];
    ,
      held = Hold[e] /. m_List /; MatrixQ[Unevaluated[m]] :>
        RuleCondition[RawBoxes @ iCompactMatrixBox[m, itemSize, frameStyle, factor]];
    ]
  ];

  MakeBoxes @@ held
];

(**************************************************************************************************)

PackageScope["renderRepresentationMatrix"]

renderRepresentationMatrix[matrix_, isTraditional_:False, opts___] :=
  RawBoxes @ CompactMatrixBox[matrix, opts, NegationStyle -> "Color", InversionStyle -> None];


(**************************************************************************************************)

PackageExport["LabeledMatrixForm"]

declareFormatting[
  LabeledMatrixForm[expr_] :> formatLabeledMatrices[expr]
];

formatLabeledMatrices[expr_] := ReplaceAll[expr,
  matrix_ /; MatrixQ[Unevaluated @ matrix] /; Length[Unevaluated @ matrix] =!= 1 :> RuleCondition @ formatLabeledMatrix @ matrix
]

formatLabeledMatrix[matrix_] := Scope[
  tooltips = MapIndexed[Tooltip, matrix, {2}];
  MatrixForm[tooltips, TableHeadings -> Automatic]
];

(**************************************************************************************************)

PackageExport["MaxWidth"]

SetUsage @ "
MaxWidth is an option to SpacedRow.
"

(**************************************************************************************************)

PackageExport["ItemFunction"]

SetUsage @ "
ItemFunction is an option to SpacedRow.
"

(**************************************************************************************************)

PackageExport["LabelFunction"]

SetUsage @ "
LabelFunction is an option to SpacedRow.
"

(**************************************************************************************************)

PackageExport["LabelSpacing"]

SetUsage @ "
LabelSpacing is an option to SpacedRow.
"

(**************************************************************************************************)

PackageExport["SpacedColumn"]

SpacedColumn[args___] := SpacedRow[args, "Transposed" -> True];

(**************************************************************************************************)

PackageExport["SpacedRow"]


$srSpacings = 20;
$srMaxItems = Infinity;
$srMaxWidth = Infinity;
$srLabelStyle = $LabelStyle;
$srBaseStyle = {};
$srItemStyle = {};
$srItemFunction = Identity;
$srLabelFunction = Identity;
$srLabelSpacing = 0.1;
$srTransposed = False;

(* this is because i don't trust OptionsPattern to not capture rules used as label specs.
i might be wrong though *)

SpacedRow[elems__, MaxWidth -> n_] := Block[{$srMaxWidth = n}, SpacedRow[elems]];
SpacedRow[elems__, MaxItems -> n_] := Block[{$srMaxItems = n}, SpacedRow[elems]];
SpacedRow[elems__, Spacings -> n_] := Block[{$srSpacings = n}, SpacedRow[elems]];
SpacedRow[elems__, LabelStyle -> style_] := Block[{$srLabelStyle = style}, SpacedRow[elems]];
SpacedRow[elems__, BaseStyle -> s_] := Block[{$srBaseStyle = s}, SpacedRow[elems]];
SpacedRow[elems__, ItemStyle -> s_] := Block[{$srItemStyle = s}, SpacedRow[elems]];
SpacedRow[elems__, ItemFunction -> f_] := Block[{$srItemFunction = f}, SpacedRow[elems]];
SpacedRow[elems__, LabelFunction -> f_] := Block[{$srLabelFunction = f}, SpacedRow[elems]];
SpacedRow[elems__, LabelSpacing -> s_] := Block[{$srLabelSpacing = s}, SpacedRow[elems]];
SpacedRow[elems__, "Transposed" -> t_] := Block[{$srTransposed = t}, SpacedRow[elems]];

SpacedRow[labels_List -> items_List] /; Length[labels] == Length[items] :=
  SpacedRow[RuleThread[labels, items]];

SpacedRow[elems__] := Scope[
  items = DeleteCases[Null] @ Flatten @ {elems};
  items = canonicalizeItem /@ Take[items, UpTo @ $srMaxItems];
  hasLabels = MemberQ[items, _Labeled];
  tooLong = Length[items] > $srMaxWidth;
  If[tooLong || hasLabels,
    If[tooLong,
      items = Flatten @ Riffle[Partition[items, UpTo[$srMaxWidth]], {$nextRow}]
    ];
    If[hasLabels,
      items //= Map[toGridRowPair /* If[$srTransposed, Reverse, Identity]];
      vspacings = {$srLabelSpacing, .5};
      entries = unfoldRow /@ SequenceSplit[items, {$nextRow}];
      itemStyle = {{$srItemStyle, $srLabelStyle}};
    ,
      vspacings = {0.5};
      entries = SequenceSplit[items, {$nextRow}];
      itemStyle = {{$srItemStyle}};
    ];
    hspacings = $srSpacings/10;
    styles = {{}, itemStyle};
    If[$srTransposed,
      entries //= Transpose;
      styles //= Reverse;
      {hspacings, vspacings} = {vspacings * 1.5, hspacings * 0.5};
    ];
    Grid[
      entries,
      Alignment -> {Center, Center},
      Spacings -> {hspacings, {0, vspacings}},
      ItemStyle -> styles,
      BaseStyle -> $srBaseStyle
    ]
  ,
    If[$srTransposed,
      Column[items, Spacings -> $srSpacings/20, BaseStyle -> ToList[$srItemStyle, $srBaseStyle], Alignment -> Center],
      Row[items, Spacer[$srSpacings], BaseStyle -> ToList[$srItemStyle, $srBaseStyle]]
    ]
  ]
];

canonicalizeItem = Case[
  l_ -> i_        := % @ Labeled[i, l];
  Labeled[i_, l_] := Labeled[$srItemFunction @ i, $srLabelFunction @ l];
  other_          := $srItemFunction @ other;
];

toGridRowPair = Case[
  Labeled[item_, label_] := {item, label};
  $nextRow := $nextRow;
  item_ := {item, ""};
];

unfoldRow[pairs_] :=
  Splice @ Transpose @ pairs;

(**************************************************************************************************)

PackageExport["$LargeEllipsis"]

$LargeEllipsis = Style["\[Ellipsis]", $LabelStyle, Gray, 18]

(**************************************************************************************************)

PackageExport["$LargeVerticalEllipsis"]

$LargeVerticalEllipsis = Style["\[VerticalEllipsis]", $LabelStyle, Gray, 18]

(**************************************************************************************************)

PackageExport["MakeArrow"]

MakeArrow[w_:50, h_:15, thickness_:1, style_:Black] =
  makeNotationArrow[w, h, thickness, style];

(**************************************************************************************************)

PackageExport["SpacedArrow"]

SpacedArrow[l__, "ArrowColor" -> color_, r___] := Block[{$arrowColor = color},
  SpacedArrow[l, r]
];

SpacedArrow[l__, "ArrowThickness" -> thick_, r___] := Block[{$arrowThickness = thick},
  SpacedArrow[l, r]
];

SpacedArrow[a_, b_, rest___] :=
  SpacedRow[a, $smallNotationArrow, b, rest];

makeNotationArrow[w_, h_, thickness_, style___, opts___Rule] := Scope[
  h2 = h/2;
  line = Line[{{-w, 0}, Offset[{-thickness, 0}, {0,0}]}];
  head = Line[{{-h2, -h2}, {0, 0}, {-h2, h2}}];
  Graphics[{
    CapForm["Round"], JoinForm["Round"], AbsoluteThickness[thickness], $DarkGray,
    style, line, head},
    opts,
    ImageSize -> {w + 2, h + 2}, PlotRangePadding -> 0, ImagePadding -> {{1, 1}, {1, 1}},
    BaselinePosition -> Center
  ]
];

$arrowThickness = 1.1;
$arrowColor = $LightGray;
$smallNotationArrow := MakeArrow[30,10, $arrowThickness, $arrowColor];

(**************************************************************************************************)

PackageExport["Gallery"]

Options[Gallery] = {
  ImageSize -> 1000,
  Spacings -> Automatic
};

Gallery[elems_, OptionsPattern[]] := Scope[
  UnpackOptions[imageSize, spacings];
  {w, h} = ToNumericImageSize[imageSize, 1];
  elems = Flatten @ List @ elems;
  n = Length[elems];
  elems = Map[graphToGraphics, elems];
  size = estimateItemSize @ First @ elems;
  If[n > 16,
    m = Floor[N[w / size]],
    m = SelectFirst[{10, 9, 8, 7, 6, 5, 4, 3, 2, 2}, Divisible[n, #] && (size * #) < w&, Floor[N[w / size]]];
  ];
  Grid[
    Partition[attachEventHandlers @ elems, UpTo[m]],
    Alignment -> {Center, Top},
    Spacings -> spacings
  ]
];

attachEventHandlers[elems_] := MapIndexed[
  EventHandler[#, {"MouseClicked" :> Print[First @ #2]}]&,
  elems
];

graphToGraphics[Labeled[g_, x_]] := Labeled[graphToGraphics @ g, x];
graphToGraphics[g_Graph] := ExtendedGraphPlot @ g;
graphToGraphics[e_] := e;

lookupImageWidth[g_] := First @ LookupImageSize @ g;

estimateItemSize = MatchValues[
  g_Graphics | g_Graphics3D := lookupImageWidth[g];
  Labeled[g_, _] := %[g];
  Legended[g_, _] := %[g] + 50;
  other_ := First[Rasterize[other, "RasterSize"]] * 2;
];

(**************************************************************************************************)

PackageExport["ChartColorForm"]

ChartColorForm[expr_, colors_] := Scope[
  colors = Which[
    GraphQ[colors], LookupCardinalColors @ colors,
    AssociationQ[colors], colors,
    Automatic, Automatic,
    True, Return @ expr
  ];
  ReplaceAll[
    expr,
    ChartSymbol[sym_String] :> formatChartSymbol[sym, colors]
  ]
];

ChartColorForm[graph_][expr_] := ChartColorForm[expr, graph];

PackageScope["formatChartSymbol"]

formatChartSymbol[sym_String, colors_] := Scope[
  RawBoxes @ StyleBox[
    SubscriptBox["C", StyleBox[sym, Italic]]
  ,
    cards = ToPathWord @ StringTrim[sym, {"+" | "-"}];
    c = Lookup[
      If[colors === Automatic, ChooseCardinalColors @ cards, colors],
      cards,
      $Failed
    ];
    If[ContainsQ[c, $Failed], Sequence @@ {}, HumanBlend @ c],
    BaseStyle -> Append[$LabelStyle, AutoSpacing -> False]
  ]
];

(**************************************************************************************************)

PackageExport["EllipsisForm"]

EllipsisForm[list_, n_] := If[Length[list] > n, Append[Take[list, n], $LargeEllipsis], list];
EllipsisForm[n_][list_] := EllipsisForm[list, n];