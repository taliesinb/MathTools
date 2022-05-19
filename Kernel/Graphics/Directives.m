PackageExport["TopLeft"]
PackageExport["TopRight"]
PackageExport["BottomLeft"]
PackageExport["BottomRight"]

(* if you add a compound side, update $SidePattern *)

(**************************************************************************************************)

PackageScope["$colorNormalizationRules"]

$colorNormalizationRules = {
  Red -> $Red, Orange -> $Orange, Yellow -> $Yellow, Green -> $Green, Blue -> $Blue, Purple -> $Purple, Pink -> $Pink, Cyan -> $Teal,
  LightRed -> $LightRed, LightYellow -> $LightYellow, LightGreen -> $LightGreen, LightBlue -> $LightBlue, LightPink -> $LightPink, LightPurple -> $LightPurple, LightCyan -> $LightTeal
};

(**************************************************************************************************)

PackageScope["$opacityNormalizationRules"]

(* if you add an opacity, update $OpacityPattern *)

PackageExport["VeryTransparent"]
PackageExport["HalfTransparent"]
PackageExport["PartlyTransparent"]
PackageExport["Opaque"]

$opacityNormalizationRules = {
  VeryTransparent -> Opacity[0.2],
  HalfTransparent -> Opacity[0.5],
  PartlyTransparent -> Opacity[0.8],
  Opaque -> Opacity[1.0]
};

PackageScope["toNumericOpacity"]

toNumericOpacity = Case[
  r_ ? NumericQ := Clip[N @ r, {0, 1}];
  sym_Symbol    := Lookup[$opacityNormalizationRules, sym, 1];
  _             := 1;
];

(**************************************************************************************************)

PackageExport["VeryThick"]
PackageExport["MediumThick"]
PackageExport["SlightlyThick"]
PackageExport["SlightlyThin"]
PackageExport["MediumThin"]
PackageExport["VeryThin"]

(* how AbsoluteThickness works:
  Thin/AT[Tiny] = AT[0.25],
  Small = AT[0.5],
  Medium = AT[1],
  Large / Thick is AT[2] *)

PackageScope["$thicknessNormalizationRules"]

$thicknessNormalizationRules = {
  VeryThin -> AbsoluteThickness[0.1],
  MediumThin -> AbsoluteThickness[0.5],
  SlightlyThin -> AbsoluteThickness[0.8],
  SlightlyThick -> AbsoluteThickness[1.2],
  MediumThick -> AbsoluteThickness[1.5],
  VeryThick -> AbsoluteThickness[3]
};

(**************************************************************************************************)

PackageScope["NormalizeThickness"]

NormalizeThickness = Case[
  Automatic             := AbsoluteThickness[1.2];
  t:Thickness[s_Symbol] := t;
  s_Symbol              := Lookup[$thicknessNormalizationRules, s, $Failed];
  n_ ? NumericQ         := AbsoluteThickness @ N @ n;
  _                     := $Failed;
];

(**************************************************************************************************)

PackageScope["toMultiDirective"]

iToMultiDirective = Case[
  {}                            := Automatic;
  {spec_}                       := toDirective @ spec;
  spec_List | spec_Association  := Map[toDirective, spec];
  spec_                         := toDirective @ spec
];

toMultiDirective[spec_] := Scope[
  res = ToColorPalette @ spec;
  If[FailureQ[res], iToMultiDirective[spec], res]
];

(**************************************************************************************************)

PackageScope["toDirective"]

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

PackageScope["normalizeStyles"]

normalizeStyles[e_] := ReplaceAll[e, $styleNormalizationRules];
