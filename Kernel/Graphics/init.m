PublicVariable[$ExtendedColors, $ExtendedColorsGrouped]

$ExtendedColorsGrouped = <|
  "Basic" -> <|
    "VeryLight" -> <|"Red" -> "#f09b91", "Orange" -> "#fffb7f", "Green" -> "#a5f56a", "Cyan" -> "#98f8ea", "Blue" -> "#60aff0", "Purple" -> "#f094c4"|>,
    "Light"     -> <|"Red" -> "#ed6d56", "Orange" -> "#f6e259", "Green" -> "#81d454", "Cyan" -> "#6be1ce", "Blue" -> "#45a0f7", "Purple" -> "#dd69a5"|>,
    "Medium"    -> <|"Red" -> "#da3b26", "Orange" -> "#eebb40", "Green" -> "#54ae32", "Cyan" -> "#4aa59d", "Blue" -> "#3175b6", "Purple" -> "#ba3b78"|>,
    "Dark"      -> <|"Red" -> "#a62a17", "Orange" -> "#f19837", "Green" -> "#316f1d", "Cyan" -> "#357a76", "Blue" -> "#1d4d7d", "Purple" -> "#8d275e"|>,
    ""          -> <|"White" -> "#ffffff", "LightGray" -> "#d5d5d5", "Gray" -> "#929292", "DarkGray" -> "#646464", "Black" -> "#000000"|>
  |>,
  "Cool" -> <|
    "VeryLight" -> <|"Green" -> "#dce9d5", "Teal" -> "#d3e0e2", "Blue" -> "#ccdaf5", "BabyBlue" -> "#d2e2f1", "UltraViolet" -> "#d8d2e7", "Violet" -> "#e6d2db"|>,
    "Light"     -> <|"Green" -> "#bdd6ac", "Teal" -> "#a9c3c8", "Blue" -> "#a9c2f0", "BabyBlue" -> "#a6c4e5", "UltraViolet" -> "#b2a8d3", "Violet" -> "#cea8bc"|>,
    "Medium"    -> <|"Green" -> "#9dc284", "Teal" -> "#80a4ae", "Blue" -> "#779ee5", "BabyBlue" -> "#7ba7d7", "UltraViolet" -> "#8b7ebe", "Violet" -> "#b87f9e"|>,
    "Dark"      -> <|"Green" -> "#78a65a", "Teal" -> "#53808c", "Blue" -> "#4978d1", "BabyBlue" -> "#4f84c1", "UltraViolet" -> "#6351a2", "Violet" -> "#9b5377"|>,
    "VeryDark"  -> <|"Green" -> "#48742c", "Teal" -> "#254e5a", "Blue" -> "#2456c5", "BabyBlue" -> "#23538f", "UltraViolet" -> "#312070"|>
  |>,
  "Subdued" -> <|
    "VeryLight" -> <|"Brown" -> "#dfbab1", "Red" -> "#eecdcd", "Orange" -> "#f8e6d0", "Yellow" -> "#fdf2d0", "Green" -> "#dce9d5", "Cyan" -> "#d3e0e2"|>,
    "Light"     -> <|"Brown" -> "#d18270", "Red" -> "#df9d9b", "Orange" -> "#f2cca2", "Yellow" -> "#fbe5a3", "Green" -> "#bdd6ac", "Cyan" -> "#a9c3c8"|>,
    "Medium"    -> <|"Brown" -> "#bd4b31", "Red" -> "#d16d6a", "Orange" -> "#ecb476", "Yellow" -> "#f9d978", "Green" -> "#9dc284", "Cyan" -> "#80a4ae"|>,
    "Dark"      -> <|"Brown" -> "#992a15", "Red" -> "#bb261a", "Orange" -> "#da944b", "Yellow" -> "#eac351", "Green" -> "#78a65a", "Cyan" -> "#53808c"|>,
    "VeryDark"  -> <|"Brown" -> "#7b2817", "Red" -> "#8c1a11", "Orange" -> "#a96324", "Yellow" -> "#b89130", "Green" -> "#48742c"|>
  |>
|>;

$ExtendedColorsGrouped = Map[RGBColor, $ExtendedColorsGrouped, {3}];

toGlobalColorName[color_, {Key @ palette_, Key @ variant_, Key @ suffix_}] :=
  $ExtendedColors[SJoin[palette, variant, suffix]] = color;

$ExtendedColors = <||>;
ScanIndexed[toGlobalColorName, $ExtendedColorsGrouped, {3}];

(**************************************************************************************************)

PublicVariable[$ExtendedColorNames]

$ExtendedColorNames = Keys @ $ExtendedColorsGrouped;

(**************************************************************************************************)

PublicSymbol[LeftRight, TopBottom, BeforeAfter]
