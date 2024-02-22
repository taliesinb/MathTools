PublicFunction[LookupStyleColor]

LookupStyleColor[styleName_] :=
  FirstCase[CurrentValue[{StyleDefinitions, styleName}], rgb_RGBColor :> rgb, None, {0, Inf}];

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
  styleOp = F @ SCases[boxesToKatexString @ dummy, "\\" ~~ LetterCharacter.., 1];
  SRep[katexStr, "\\" ~~ w:LetterCharacter.. :>
    If[SEndsQ[w, "Symbol"],
      SJoin[styleOp, "\\", w],
      SJoin["\\styled", UpperCaseFirst @ w, "{", styleOp, "}"]
    ],
    1
  ]
]
