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

PublicSymbol[EllipsisSymbol, VerticalEllipsisSymbol, PlainEllipsisSymbol, CenterEllipsisSymbol]

DefineSymbolForm @ {
  PlainEllipsisSymbol        -> "\[ThinSpace]...\[ThinSpace]",
  CenterEllipsisSymbol       -> KBox["\[ThinSpace]\[CenterEllipsis]\[ThinSpace]", "\\,\\cdot\\!\\cdot\\!\\cdot\\,"],
  EllipsisSymbol             -> GrayBox[KBox["\[ThinSpace]...\[ThinSpace]", "\\,\\gFo{...}\\,"]],
  VerticalEllipsisSymbol     -> KBox["\[ThinSpace]\[VerticalEllipsis]\[ThinSpace]", "\\vdots"]
};