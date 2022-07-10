PublicSymbol[NotApplicableSymbol, UnknownSymbol, EmptySetSymbol, TickSymbol, UnitInterval, BlankSymbol, PlaceholderSquareSymbol]

PublicSymbol[BarTokenSymbol, FilledTokenSymbol, FilledSquareTokenSymbol, FilledRectangleTokenSymbo, EmptyTokenSymbol, EmptySquareTokenSymbol, EmptyRectangleTokenSymbol]

PublicSymbol[EllipsisSymbol, VerticalEllipsisSymbol]

(**************************************************************************************************)

Map[DeclareConstantSymbolTemplateBox, {
  NotApplicableSymbol        -> GrayBox @ KatexBox["NA", "\\text{---}"],
  UnknownSymbol              -> GrayBox["?"],
  EmptySetSymbol             -> GrayBox["\[EmptySet]"],
  TickSymbol                 -> BoldBox["\[VeryThinSpace]\[Checkmark]\[VeryThinSpace]"],
  UnitInterval               -> "\[DoubleStruckCapitalI]",
  BlankSymbol                -> GrayBox @ KatexBox["\[VeryThinSpace]_\[VeryThinSpace]", "_"],
  PlaceholderSquareSymbol    -> "\[EmptySquare]",
  BarTokenSymbol             -> BoldBox["\[VeryThinSpace]|\[VeryThinSpace]"],
  FilledTokenSymbol          -> "\[FilledCircle]",
  FilledSquareTokenSymbol    -> "\[FilledSquare]",
  FilledRectangleTokenSymbol -> "\[FilledRectangle]",
  EmptyTokenSymbol           -> "\[EmptyCircle]",
  EmptySquareTokenSymbol     -> "\[EmptySquare]",
  EmptyRectangleTokenSymbol  -> "\[EmptyRectangle]",
  EllipsisSymbol             -> KatexBox["\[ThinSpace]...\[ThinSpace]", "\\,\\gFo{...}\\,"],
  VerticalEllipsisSymbol     -> KatexBox["\[ThinSpace]\[VerticalEllipsis]\[ThinSpace]", "\\vdots"]
}]

