PublicSymbol[NotApplicableSymbol, UnknownSymbol, EmptySetSymbol, TickSymbol, UnitInterval, BlankSymbol, PlaceholderSquareSymbol]

PublicSymbol[BarTokenSymbol, FilledTokenSymbol, FilledSquareTokenSymbol, FilledRectangleTokenSymbo, EmptyTokenSymbol, EmptySquareTokenSymbol, EmptyRectangleTokenSymbol]

PublicSymbol[EllipsisSymbol, VerticalEllipsisSymbol, PlainEllipsisSymbol, CenterEllipsisSymbol]

(**************************************************************************************************)

Map[DeclareConstantSymbolTemplateBox, {
  NotApplicableSymbol        -> GrayBox @ KatexSwitch["NA", "\\text{---}"],
  UnknownSymbol              -> GrayBox["?"],
  EmptySetSymbol             -> GrayBox["\[EmptySet]"],
  TickSymbol                 -> BoldBox["\[VeryThinSpace]\[Checkmark]\[VeryThinSpace]"],
  UnitInterval               -> "\[DoubleStruckCapitalI]",
  BlankSymbol                -> GrayBox @ KatexSwitch["\[VeryThinSpace]_\[VeryThinSpace]", "_"],
  PlaceholderSquareSymbol    -> "\[EmptySquare]",
  BarTokenSymbol             -> BoldBox["\[VeryThinSpace]|\[VeryThinSpace]"],
  FilledTokenSymbol          -> "\[FilledCircle]",
  FilledSquareTokenSymbol    -> "\[FilledSquare]",
  FilledRectangleTokenSymbol -> "\[FilledRectangle]",
  EmptyTokenSymbol           -> "\[EmptyCircle]",
  EmptySquareTokenSymbol     -> "\[EmptySquare]",
  EmptyRectangleTokenSymbol  -> "\[EmptyRectangle]",
  PlainEllipsisSymbol        -> KatexSwitch["\[ThinSpace]...\[ThinSpace]", "\\,...\\,"],
  CenterEllipsisSymbol       -> KatexSwitch["\[ThinSpace]\[CenterEllipsis]\[ThinSpace]", "\\,\\cdot\\!\\cdot\\!\\cdot\\,"],
  EllipsisSymbol             -> KatexSwitch[GrayBox["\[ThinSpace]...\[ThinSpace]"], "\\,\\gFo{...}\\,"],
  VerticalEllipsisSymbol     -> KatexSwitch["\[ThinSpace]\[VerticalEllipsis]\[ThinSpace]", "\\vdots"]
}]

