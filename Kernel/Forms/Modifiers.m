PrivateFunction[applyCurrentFormModifiers]

(* this currently doesn't work on the outside of StringDiagram since it evaluates directly to a graphics which is THEN
typeset. so StringDiagram takes a set of modifiers directly. *)

applyCurrentFormModifiers[expr_] := If[Len[$formModifierFnStack] > 0,
  RawBoxes @ Apply[Composition, $formModifierFnStack] @ ToBoxes @ expr,
  expr
]

(**************************************************************************************************)

$formModifierFnStack = {};

DefineFormModifier[head_, boxfn_] := (
  DefineStandardTraditionalForm[head[expr_] :> applyFormModifier[MakeBoxes[expr], boxfn]];
);

applyFormModifier[boxes_, fn_] := InheritedBlock[
  {$formModifierFnStack},
  AppTo[$formModifierFnStack, fn];
  fn @ boxes
];

(**************************************************************************************************)

PublicTypesettingForm[UsingImplicitAppliedForm]

DefineFormModifier[UsingImplicitAppliedForm, toImplicitAppliedForm];

toImplicitAppliedForm[boxes_] := RepRep[boxes,
  TemplateBox[list_, "appliedForm"] :> TemplateBox[list, "ImplicitAppliedForm"]
];

(**************************************************************************************************)

PublicTypesettingForm[UsingExplicitAppliedForm]

DefineFormModifier[UsingExplicitAppliedForm, toExplicitAppliedForm];

toExplicitAppliedForm[boxes_] := RepRep[boxes,
  TemplateBox[list_, "ImplicitAppliedForm"] :> TemplateBox[list, "appliedForm"]
];

(**************************************************************************************************)

PublicTypesettingForm[UsingTightForm]

DefineFormModifier[UsingTightForm, toTightForm];

toTightForm[boxes_] := TBox[RepAll[EvaluateTemplateBoxFull @ boxes, ", " -> ","], "MathForm"];

(**************************************************************************************************)

PublicTypesettingForm[UsingModernForm]

$taggedFormHeadQ[UsingModernForm] = True;

DefineStandardTraditionalForm[
  UsingModernForm[a_] :> usingModernBoxes[a]
];

(* this is kinda hacky: we don't really know which strings are content and which are options etc, but the
Rule exception tries to deal with that *)
usingModernBoxes[sub_] := ToBoxes @ RepAll[sub, {r_Rule :> r, s_String :> RuleEval @ toModernStr @ s}];

toModernStr[str_] := ModernForm @ ScriptToRoman @ str;

(**************************************************************************************************)

PublicTypesettingForm[RemoveDecoratorsForm]

DefineStandardTraditionalForm[
  RemoveDecoratorsForm[a_] :> ToBoxes @ RepAll[a, {Subscript[e_, _] :> e, PrimedForm[e_] :> e}]
];
