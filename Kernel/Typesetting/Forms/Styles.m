PublicFunction[LookupStyleColor]

LookupStyleColor[styleName_] :=
  FirstCase[CurrentValue[{StyleDefinitions, styleName}], rgb_RGBColor :> rgb, None, {0, Infinity}];

(**************************************************************************************************)

PublicFunction[StripColorForms]

StripColorForms[expr_] := expr //. $colorFormP[z_] :> z;

(**************************************************************************************************)

PublicForm[PlainTextForm]

declareBoxFormatting[
  PlainTextForm[p_] :>
    TemplateBox[List @ MakeBoxes @ p, "PlainTextForm"]
];

$TemplateKatexFunction["PlainTextForm"] = "textrm";

(**************************************************************************************************)

PublicForm[BoldForm, LightRedForm, LightGreenForm, LightBlueForm, LightRedGreenForm, LightGreenBlueForm, LightRedBlueForm, LightPurpleForm, RedForm, GreenForm, BlueForm, DarkRedForm, DarkGreenForm, DarkBlueForm, RedBlueForm, GreenBlueForm, RedGreenForm, DarkRedBlueForm, DarkGreenBlueForm, DarkRedGreenForm, PurpleForm, DarkPurpleForm, DarkGrayForm, MediumGrayForm, LightGrayForm]

declareUnaryWrapperForm[BoldForm, "boldForm"]
declareUnaryWrapperForm[LightRedForm, "lrform"];
declareUnaryWrapperForm[LightGreenForm, "lgform"];
declareUnaryWrapperForm[LightBlueForm, "lbform"];
declareUnaryWrapperForm[LightRedGreenForm, "lrgform"];
declareUnaryWrapperForm[LightGreenBlueForm, "lgbform"];
declareUnaryWrapperForm[LightRedBlueForm, "lrbform"];
declareUnaryWrapperForm[LightPurpleForm, "lpform"];
declareUnaryWrapperForm[RedForm, "rform"];
declareUnaryWrapperForm[GreenForm, "gform"];
declareUnaryWrapperForm[BlueForm, "bform"];
declareUnaryWrapperForm[DarkRedForm, "drform"];
declareUnaryWrapperForm[DarkGreenForm, "dgform"];
declareUnaryWrapperForm[DarkBlueForm, "dbform"];
declareUnaryWrapperForm[DarkRedGreenForm, "drgform"];
declareUnaryWrapperForm[DarkGreenBlueForm, "dgbform"];
declareUnaryWrapperForm[DarkRedBlueForm, "drbform"];
declareUnaryWrapperForm[RedBlueForm, "rbform"];
declareUnaryWrapperForm[PurpleForm, "pform"];
declareUnaryWrapperForm[DarkPurpleForm, "dpform"];
declareUnaryWrapperForm[RedGreenForm, "rgform"];
declareUnaryWrapperForm[GreenBlueForm, "gbform"];
declareUnaryWrapperForm[DarkGrayForm, "waform"];
declareUnaryWrapperForm[MediumGrayForm, "wbform"];
declareUnaryWrapperForm[LightGrayForm, "wcform"];

(**************************************************************************************************)

PublicForm[StyledOperatorForm]

declareBoxFormatting[
  StyledOperatorForm[f_][e_] :>
    TemplateBox[{makeQGBoxes @ f @ "|", makeQGBoxes @ e}, "StyledOperatorForm"]
];

$TemplateKatexFunction["StyledOperatorForm"] = styledOperatorKatex;

styledOperatorKatex[dummy_, inner_] := Scope[
  katexStr = boxesToKatexString @ inner;
  styleOp = First @ StringCases[boxesToKatexString @ dummy, "\\" ~~ LetterCharacter.., 1];
  StringReplace[katexStr, "\\" ~~ w:LetterCharacter.. :>
    If[StringEndsQ[w, "Symbol"],
      StringJoin[styleOp, "\\", w],
      StringJoin["\\styled", UpperCaseFirst @ w, "{", styleOp, "}"]
    ],
    1
  ]
]
