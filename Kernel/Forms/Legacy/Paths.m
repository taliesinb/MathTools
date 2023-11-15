PublicTypesettingForm[PathGroupoidSymbol]

PathGroupoidSymbol[] := PathGroupoidSymbol["Q"];

DefineTaggedForm[PathGroupoidSymbol]

(**************************************************************************************************)

PublicTypesettingForm[PathQuiverSymbol]

DefineUnaryForm[PathQuiverSymbol, "?"];

(**************************************************************************************************)

PublicTypesettingForm[ForwardPathQuiverSymbol]

ForwardPathQuiverSymbol[] := ForwardPathQuiverSymbol["Q", "v"];
ForwardPathQuiverSymbol[q_] := ForwardPathQuiverSymbol[q, "v"];

DefineBinaryForm[ForwardPathQuiverSymbol, "?"];

(**************************************************************************************************)

PublicTypesettingForm[BackwardPathQuiverSymbol]

BackwardPathQuiverSymbol[] := BackwardPathQuiverSymbol["Q", "v"];
BackwardPathQuiverSymbol[q_] := BackwardPathQuiverSymbol[q, "v"];

DefineBinaryForm[BackwardPathQuiverSymbol, "?"];

(**************************************************************************************************)

PublicTypesettingForm[ParenPathWordForm, ParenEmptyPathWordForm]

DefineTernaryForm[ParenPathWordForm, "?"];
DefineUnaryForm[ParenEmptyPathWordForm, "?"]

(**************************************************************************************************)

PublicTypesettingForm[EmptyPathWordForm]

DefineUnaryForm[EmptyPathWordForm, "?"]

(**************************************************************************************************)

PublicTypesettingForm[PathWordForm]

PathWordForm[a_, b_Str, c_] := PathWordForm[a, ToPathWord @ b, c];

DefineTernaryForm[PathWordForm, "?"]

(**************************************************************************************************)

PublicTypesettingForm[PathHomomorphismSymbol]

PathHomomorphismSymbol[] := PathHomomorphismSymbol["\[Rho]"]

DefineTaggedForm[PathHomomorphismSymbol];

(**************************************************************************************************)

PublicTypesettingForm[PathMapSymbol]

PathMapSymbol[] := PathMapSymbol["\[Mu]"];

DefineTaggedForm[PathMapSymbol];

(**************************************************************************************************)

PublicTypesettingForm[PathWordRewritingForm]

declareBoxFormatting[
  PathWordRewritingForm[args__] :>
    TemplateBox[
      MapUnevaluated[pathWordRewritingRuleBox, {args}],
      "GroupWordRewritingForm"
  ]
];

SetHoldAllComplete[pathWordRewritingRuleBox];
pathWordRewritingRuleBox = Case[
  a_ -> b_ := TemplateBox[{wordBoxes @ a, wordBoxes @ b}, "RewritingRuleForm"];
  other_   := MakeMathBoxes @ other;
];

(**************************************************************************************************)

PublicSymbol[NullPath, NullElement]

DefineSymbolForm[{NullPath -> "?", NullElement -> "?"}];

(**************************************************************************************************)

PublicTypesettingForm[PathSymbol]

PathSymbol[] := PathSymbol["P"];

DefineTaggedForm[PathSymbol];
