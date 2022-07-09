PrivateFunction[TBox, SBox, RBox, GBox]

TBox[form_][args___] := TemplateBox[{args}, form];

SBox[form_] := TemplateBox[{}, form];

RBox[args___] := RowBox[{args}];

GBox[entries_, alignments_, rowSpacings_, colSpacings_] :=
  GridBox[
    entries,
    GridBoxAlignment -> {"Columns" -> alignments},
    GridBoxSpacings -> {"Rows" -> prepend0 @ rowSpacings, "Columns" -> prepend0 @ colSpacings}
  ];

prepend0[list_List] := Prepend[list, 0];
prepend0[e_] := e;

(**************************************************************************************************)

PublicFunction[RedForm, GreenForm, BlueForm, OrangeForm, PinkForm, TealForm, GrayForm, PurpleForm]
PublicFunction[LightRedForm, LightGreenForm, LightBlueForm, LightOrangeForm, LightPinkForm, LightTealForm, LightGrayForm, LightPurpleForm]
PublicFunction[DarkRedForm, DarkGreenForm, DarkBlueForm, DarkOrangeForm, DarkPinkForm, DarkTealForm, DarkGrayForm, DarkPurpleForm]
PublicFunction[BoldForm, ItalicForm, UnderlinedForm, StruckthroughForm, PlainTextForm, MathTextForm]

PrivateFunction[RedBox, GreenBox, BlueBox, OrangeBox, PinkBox, TealBox, GrayBox, PurpleBox]
PrivateFunction[LightRedBox, LightGreenBox, LightBlueBox, LightOrangeBox, LightPinkBox, LightTealBox, LightGrayBox, LightPurpleBox]
PrivateFunction[DarkRedBox, DarkGreenBox, DarkBlueBox, DarkOrangeBox, DarkPinkBox, DarkTealBox, DarkGrayBox, DarkPurpleBox]
PrivateFunction[BoldBox, ItalicBox, UnderlinedBox, StruckthroughBox, PlainTextBox, MathTextBox]

defineStyleFormAndBox[form_, box_, style_] := With[
  {tName = SymbolName[form]},
  {kName = toKName[tName, 0]},
  MakeBoxes[form[e_], StandardForm]     := box[MakeBoxes[e, StandardForm]];
  MakeBoxes[form[e_], TraditionalForm]  := box[MakeBoxes[e, TraditionalForm]];
  box[e_]                               := TemplateBox[{e}, tName];
  (* TODO: replace this with single call to registerSingleArgTemplateBox *)
  $templateBoxDisplayFunction[tName]          = toFrontendStyleFunction[style];
  $templateToKatexFunction[tName]       = Function[kName[#]];
  $katexDisplayFunction[kName]          = toKatexStyleFunction[style];
];

toFrontendStyleFunction = Case[
  style_                := StyleBox[#1, style]&;
  KatexForm[style_, _]  := % @ style;
];

toKatexStyleFunction = Case[
  color_ ? ColorQ       := % @ StringJoin["textcolor{", ColorHexString @ color, "}"];
  str_String            := Function[str[#]];
  KatexForm[_, style_]  := % @ style;
];

MapApply[
  defineStyleFormAndBox,
  {
    {RedForm,            RedBox,             $Red},
    {GreenForm,          GreenBox,           $Green},
    {BlueForm,           BlueBox,            $Blue},
    {OrangeForm,         OrangeBox,          $Orange},
    {PinkForm,           PinkBox,            $Pink},
    {TealForm,           TealBox,            $Teal},
    {GrayForm,           GrayBox,            $Gray},
    {PurpleForm,         PurpleBox,          $Purple},
    {LightRedForm,       LightRedBox,        $LightRed},
    {LightGreenForm,     LightGreenBox,      $LightGreen},
    {LightBlueForm,      LightBlueBox,       $LightBlue},
    {LightOrangeForm,    LightOrangeBox,     $LightOrange},
    {LightPinkForm,      LightPinkBox,       $LightPink},
    {LightTealForm,      LightTealBox,       $LightTeal},
    {LightGrayForm,      LightGrayBox,       $LightGray},
    {LightPurpleForm,    LightPurpleBox,     $LightPurple},
    {DarkRedForm,        DarkRedBox,         $DarkRed},
    {DarkGreenForm,      DarkGreenBox,       $DarkGreen},
    {DarkBlueForm,       DarkBlueBox,        $DarkBlue},
    {DarkOrangeForm,     DarkOrangeBox,      $DarkOrange},
    {DarkPinkForm,       DarkPinkBox,        $DarkPink},
    {DarkTealForm,       DarkTealBox,        $DarkTeal},
    {DarkGrayForm,       DarkGrayBox,        $DarkGray},
    {DarkPurpleForm,     DarkPurpleBox,      $DarkPurple},
    {BoldForm,           BoldBox,            KatexForm[Bold, "mathbf"]},
    {ItalicForm,         ItalicBox,          KatexForm[Italic, "mathit"]},
    {UnderlinedForm,     UnderlinedBox,      KatexForm[Underlined, "underline"]},
    {StruckthroughForm,  StruckthroughBox,   KatexForm[Struckthrough, "struckthrough"]},
    {PlainTextForm,      PlainTextBox,       KatexForm["MathText", "textrm"]},
    {MathTextForm,       MathTextBox,        KatexForm["MathTextFont", "textrm"]}
  }
];

(**************************************************************************************************)

PrivateFunction[FunctionBox]

FunctionBox[e_String] := KatexBox[e, StringJoin["\\operatorname{", e, "}"]];

