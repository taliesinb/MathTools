PrivateVariable[$LabelStyle]

$LabelStyle = {
  FontFamily -> "Avenir", FontSize -> 12
};

(**************************************************************************************************)

PublicVariable[$MathFont]

$MathFont = "KaTeX_Main";

(**************************************************************************************************)

PrivateVariable[$CardinalFont]

$CardinalFont = "KaTeX_Typewriter"

(**************************************************************************************************)

PublicVariable[$CardinalLabelStyle]

$CardinalLabelStyle = {
  FontFamily -> $CardinalFont, FontSize -> 14
};

(**************************************************************************************************)

PublicVariable[$MathLabelStyle]

$MathLabelStyle = {
  FontFamily -> $MathFont, FontSize -> 14,
  SingleLetterItalics -> True, ShowStringCharacters -> False,
  AutoItalicWords -> $RomanGreekAlphabet
};
