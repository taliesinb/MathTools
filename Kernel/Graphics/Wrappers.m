PublicTypesettingForm[LegendForm]

SetUsage @ "
LegendForm[expr$] renders as a form of expr$ that is suitable for use in %Legended.
"

LegendForm[Placed[a_, pos_]] :=
  Placed[LegendForm @ a, pos];

LegendForm[lf_LegendForm] := lf;

declareFormatting[
  LegendForm[list_List] :> Row[Map[LegendForm, list], Spacer[5]],
  LegendForm[assoc_Association] :> Grid[
    Transpose @ KeyValueMap[{LegendForm[#2], LabelForm[#1, Bold]}&, assoc],
    Spacings -> {{0.2, {1.2}}, {{0.5}}}
  ],
  LegendForm[e_] :> e
];

(* fix a bug in mathematica *)
DownValues[WrappersDump`makeLabeledCore] = ReplaceAll[DownValues[WrappersDump`makeLabeledCore], "SkipImageSizeLevel" -> "ZSkipImageSizeLevel"];

(**************************************************************************************************)

PrivateFunction[ApplyLegend]

ApplyLegend[expr_, None | Automatic | {}] :=
  expr;

ApplyLegend[expr_, item_] :=
  updateLegendMargins @ Legended[expr, If[ListQ[item], Map[LegendForm], LegendForm] @ item];

ApplyLegend[Labeled[graphics_, args__], newLegend_] :=
  Labeled[ApplyLegend[graphics, newLegend], args];

ApplyLegend[Legended[expr_, oldLegend_], newLegend_] :=
 updateLegendMargins @ Legended[expr, ToList[oldLegend, LegendForm /@ newLegend]];

ApplyLegend[legend_][graphics_] := ApplyLegend[graphics, legend];

updateLegendMargins[other_] := other;
updateLegendMargins[Legended[g_Graphics | g_Graphics3D, legendSpec_]] := Scope[
  l = 0; r = 0; b = 0; t = 0;
  Cases[legendSpec, Placed[_, s_Symbol ? updateMargin], {0, 2}];
  If[Min[l,r,b,t] == 0, r = $legendMargin];
  Legended[
    ReplaceOptions[g, ImageMargins -> {{l, r}, {b, t}}],
    legendSpec
  ]
];

$legendMargin = 15;
updateMargin = <|
  Left :> (l = $legendMargin), Right :> (r = $legendMargin),
  Bottom :> (b = $legendMargin), Top :> (r = $legendMargin)
|>;

(**************************************************************************************************)

PrivateFunction[ApplyEpilog]

ApplyEpilog[graphics_, None | {}] := graphics;

ApplyEpilog[Labeled[graphics_, args__], epilog_] :=
  Labeled[ApplyEpilog[graphics, epilog], args];

ApplyEpilog[graphics_Graphics, epilog_] :=
  UpdateOptions[graphics, Epilog, Function[If[#1 === {}, epilog, {epilog, #1}]]];

ApplyEpilog[Graphics3D[primitives_, opts___], epilog_] :=
  Graphics3D[{primitives, epilog}, opts];

ApplyEpilog[epilog_][graphics_] := ApplyEpilog[graphics, epilog];

(**************************************************************************************************)

PrivateFunction[ApplyProlog]

ApplyProlog[graphics_, None | {}] := graphics;

ApplyProlog[Labeled[graphics_, args__], prolog_] :=
  Labeled[ApplyProlog[graphics, prolog], args];

ApplyProlog[graphics_Graphics, prolog_] :=
  UpdateOptions[graphics, Prolog, Function[If[#1 === {}, prolog, {#1, prolog}]]];

ApplyProlog[Graphics3D[primitives_, opts___], prolog_] :=
  Graphics3D[{prolog, primitives}, opts];

ApplyProlog[prolog_][graphics_] := ApplyProlog[graphics, prolog];
