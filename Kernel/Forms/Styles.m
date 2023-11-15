PublicFunction[LookupStyleColor]

LookupStyleColor[styleName_] :=
  FirstCase[CurrentValue[{StyleDefinitions, styleName}], rgb_RGBColor :> rgb, None, {0, Infinity}];

(**************************************************************************************************)

PublicFunction[StripColorForms]

StripColorForms[expr_] := expr //. $colorFormP[z_] :> z;

(**************************************************************************************************)

PublicTypesettingForm[StyledOperatorForm]

declareBoxFormatting[
  StyledOperatorForm[f_][e_] :>
    TemplateBox[{MakeMathBoxes @ f @ "|", MakeMathBoxes @ e}, "StyledOperatorForm"]
];

$TemplateKatexFunction["StyledOperatorForm"] = styledOperatorKatex;

styledOperatorKatex[dummy_, inner_] := Scope[
  katexStr = boxesToKatexString @ inner;
  styleOp = P1 @ StringCases[boxesToKatexString @ dummy, "\\" ~~ LetterCharacter.., 1];
  StringReplace[katexStr, "\\" ~~ w:LetterCharacter.. :>
    If[StringEndsQ[w, "Symbol"],
      StringJoin[styleOp, "\\", w],
      StringJoin["\\styled", UpperCaseFirst @ w, "{", styleOp, "}"]
    ],
    1
  ]
]
