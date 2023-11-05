PublicSymbol[NotApplicableSymbol, UnknownSymbol, EmptySetSymbol, TickSymbol, UnitInterval, BlankSymbol, PlaceholderSquareSymbol]

PublicSymbol[BarSymbol, FilledCircleSymbol, FilledSquareSymbol, FilledRectangleSymbol, CircleSymbol, SquareSymbol, RectangleSymbol]

PublicSymbol[RightArrowSymbol, LeftArrowSymbol, UpArrowSymbol, DownArrowSymbol, UpDownArrowSymbol, LeftRightArrowSymbol]

DefineSymbolForm @ {
  NotApplicableSymbol        -> GrayBox["---"],
  UnknownSymbol              -> GrayBox["?"],
  EmptySetSymbol             -> GrayBox["\[EmptySet]"],
  TickSymbol                 -> BoldBox["\[VeryThinSpace]\[Checkmark]\[VeryThinSpace]"],
  UnitInterval               -> "\[DoubleStruckCapitalI]",
  BlankSymbol                -> GrayBox[KBox["\[VeryThinSpace]_\[VeryThinSpace]", "_"]],
  PlaceholderSquareSymbol    -> "\[EmptySquare]",
  BarSymbol                  -> BoldBox["\[VeryThinSpace]|\[VeryThinSpace]"],
  FilledCircleSymbol         -> "\[FilledCircle]",
  FilledSquareSymbol         -> "\[FilledSquare]",
  FilledRectangleSymbol      -> "\[FilledRectangle]",
  CircleSymbol               -> "\[EmptyCircle]",
  SquareSymbol               -> "\[EmptySquare]",
  RectangleSymbol            -> "\[EmptyRectangle]",
  RightArrowSymbol           -> "\[RightArrow]",
  LeftArrowSymbol            -> "\[LeftArrow]",
  UpArrowSymbol              -> "\[UpArrow]",
  DownArrowSymbol            -> "\[DownArrow]",
  UpDownArrowSymbol          -> "\[UpDownArrow]",
  LeftRightArrowSymbol       -> "\[LeftRightArrow]"
};

(**************************************************************************************************)

PublicSymbol[BoldRightArrowSymbol, BoldLeftArrowSymbol, BoldUpArrowSymbol, BoldDownArrowSymbol]

DefineIconSymbolForm @ {
  BoldRightArrowSymbol     -> "➡︎",
  BoldLeftArrowSymbol      -> "⬅︎",
  BoldUpArrowSymbol        -> "⬆︎",
  BoldDownArrowSymbol      -> "⬇︎"
}

(**************************************************************************************************)

PublicSymbol[CommaSymbol, GrayCommaSymbol, LightGrayCommaSymbol, ArrowSymbol, GrayArrowSymbol, LightGrayArrowSymbol, ColonSymbol, GrayColonSymbol, LightGrayColonSymbol]

DefineSymbolForm @ {
  CommaSymbol                -> ",",
  GrayCommaSymbol            -> GrayBox @ ",",
  LightGrayCommaSymbol       -> LightGrayBox @ ",",
  ArrowSymbol                -> "\[RightArrow]",
  GrayArrowSymbol            -> GrayBox @ "\[RightArrow]",
  LightGrayArrowSymbol       -> LightGrayBox @ "\[RightArrow]",
  ColonSymbol                -> ":",
  GrayColonSymbol            -> GrayBox @ ":",
  LightGrayColonSymbol       -> LightGrayBox @ ":"
}

(**************************************************************************************************)

PublicSymbol[EllipsisSymbol, VerticalEllipsisSymbol, PlainEllipsisSymbol, CenterEllipsisSymbol]

DefineSymbolForm @ {
  PlainEllipsisSymbol        -> "\[ThinSpace]...\[ThinSpace]",
  CenterEllipsisSymbol       -> KBox["\[ThinSpace]\[CenterEllipsis]\[ThinSpace]", "\\,\\cdot\\!\\cdot\\!\\cdot\\,"],
  EllipsisSymbol             -> GrayBox[KBox["\[ThinSpace]...\[ThinSpace]", "\\,...\\,"]],
  VerticalEllipsisSymbol     -> KBox["\[ThinSpace]\[VerticalEllipsis]\[ThinSpace]", "\\vdots"]
};

(**************************************************************************************************)

PublicSymbol[RedToken, OrangeToken, GreenToken, TealToken, BlueToken, PinkToken, PurpleToken, GrayToken, WhiteToken]

DefineSymbolForm[#1 -> #2["\[FilledCircle]"]]& @@@ ExpressionTable[
  RedToken     RedBox
  OrangeToken  OrangeBox
  GreenToken   GreenBox
  TealToken    TealBox
  BlueToken    BlueBox
  PinkToken    PinkBox
  PurpleToken  PurpleBox
  GrayToken    GrayBox
];

DefineSymbolForm[WhiteToken -> "\[EmptyCircle]"];

(**************************************************************************************************)

PublicSymbol[ExtendedDoubleRightArrow, ExtendedDoubleLeftArrow]

DefineStandardTraditionalForm[{
  ExtendedDoubleRightArrow[n_] :> GridBox[{{ItemBox["\[DoubleRightArrow]", ItemSize -> n]}}],
  ExtendedDoubleLeftArrow[n_] :> GridBox[{{ItemBox["\[DoubleLeftArrow]", ItemSize -> n]}}]
}]

