PublicSymbol[NotApplicableSymbol, UnknownSymbol, EmptySetSymbol, TickSymbol, UnitInterval, BlankSymbol, PlaceholderSquareSymbol]

PublicSymbol[BarTokenSymbol, FilledTokenSymbol, FilledSquareTokenSymbol, FilledRectangleTokenSymbol, EmptyTokenSymbol, EmptySquareTokenSymbol, EmptyRectangleTokenSymbol]

DefineSymbolForm @ {
  NotApplicableSymbol        -> GrayBox["---"],
  UnknownSymbol              -> GrayBox["?"],
  EmptySetSymbol             -> GrayBox["\[EmptySet]"],
  TickSymbol                 -> BoldBox["\[VeryThinSpace]\[Checkmark]\[VeryThinSpace]"],
  UnitInterval               -> "\[DoubleStruckCapitalI]",
  BlankSymbol                -> GrayBox[KBox["\[VeryThinSpace]_\[VeryThinSpace]", "_"]],
  PlaceholderSquareSymbol    -> "\[EmptySquare]",
  BarTokenSymbol             -> BoldBox["\[VeryThinSpace]|\[VeryThinSpace]"],
  FilledTokenSymbol          -> "\[FilledCircle]",
  FilledSquareTokenSymbol    -> "\[FilledSquare]",
  FilledRectangleTokenSymbol -> "\[FilledRectangle]",
  EmptyTokenSymbol           -> "\[EmptyCircle]",
  EmptySquareTokenSymbol     -> "\[EmptySquare]",
  EmptyRectangleTokenSymbol  -> "\[EmptyRectangle]"
};

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

PublicSymbol[RedToken, OrangeToken, GreenToken, TealToken, BlueToken, PinkToken, PurpleToken, GrayToken]

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