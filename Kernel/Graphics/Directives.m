SystemSymbol[MediumSmall, MediumLarge, Huge]

SetUsage @ "MediumSmall represents a size betwen %Small and %Medium."
SetUsage @ "MediumLarge represents a size betwen %Medium and %Large."
SetUsage @ "Huge represents a size greater than %Large."

(* if you add a size, update $SizePattern and $SymbolicSizePattern *)

(**************************************************************************************************)

SystemSymbol[TopLeft, TopRight, BottomLeft, BottomRight]

(* if you add a compound side, update $SidePattern *)

(**************************************************************************************************)

PrivateVariable[$colorNormalizationRules]

$colorNormalizationRules = {
  Red -> $Red, Orange -> $Orange, Yellow -> $Yellow, Green -> $Green, Blue -> $Blue, Purple -> $Purple, Pink -> $Pink, Cyan -> $Teal,
  LightRed -> $LightRed, LightYellow -> $LightYellow, LightGreen -> $LightGreen, LightBlue -> $LightBlue, LightPink -> $LightPink, LightPurple -> $LightPurple, LightCyan -> $LightTeal
};

(**************************************************************************************************)

SystemSymbol[VeryTransparent, HalfTransparent, PartlyTransparent, Opaque]

VeryTransparent::usage = HalfTransparent::usage = PartlyTransparent::usage = Opaque::usage = "";

PrivateFunction[toNumericOpacity]

toNumericOpacity = Case[
  r_ ? NumericQ := Clip[N @ r, {0, 1}];
  sym_Symbol    := Lookup[$opacityNormalizationRules, sym, 1];
  _             := 1;
];

PrivateVariable[$opacityNormalizationRules]

(* if you add an opacity, update $OpacityPattern *)

$opacityNormalizationRules = {
  VeryTransparent -> Opacity[0.2],
  HalfTransparent -> Opacity[0.5],
  PartlyTransparent -> Opacity[0.8],
  Opaque -> Opacity[1.0]
};

(**************************************************************************************************)

SystemSymbol[VeryThick, MediumThick, SlightlyThick, SlightlyThin, MediumThin, VeryThin]

(* how AbsoluteThickness works:
  Thin/AT[Tiny] = AT[0.25],
  Small = AT[0.5],
  Medium = AT[1],
  Large / Thick is AT[2] *)

PrivateVariable[$thicknessNormalizationRules]

$thicknessNormalizationRules = {
  VeryThin -> AbsoluteThickness[0.1],
  MediumThin -> AbsoluteThickness[0.5],
  SlightlyThin -> AbsoluteThickness[0.8],
  SlightlyThick -> AbsoluteThickness[1.2],
  MediumThick -> AbsoluteThickness[1.5],
  VeryThick -> AbsoluteThickness[3]
};

(**************************************************************************************************)

PrivateFunction[NormalizeThickness]

NormalizeThickness = Case[
  Automatic             := AbsoluteThickness[1.2];
  t:Thickness[s_Symbol] := t;
  s_Symbol              := Lookup[$thicknessNormalizationRules, s, $Failed];
  n_ ? NumericQ         := AbsoluteThickness @ N @ n;
  _                     := $Failed;
];

(**************************************************************************************************)

PrivateFunction[toDirective]

toDirective = Case[
  Automatic           := Automatic;
  {d_Directive}       := % @ d;
  e_List              := normalizeStyles[
    Directive @@ DeleteNone @ Flatten @ ReplaceAll[e, Directive[d_] :> d]
  ];
  Directive[e_List]   := %[e];
  e_                  := normalizeStyles @ e
];

$styleNormalizationRules = Dispatch @ Flatten @ {
  $colorNormalizationRules,
  $opacityNormalizationRules,
  $thicknessNormalizationRules
};

PrivateFunction[normalizeStyles]

normalizeStyles[e_] := ReplaceAll[e, $styleNormalizationRules];
