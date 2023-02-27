PublicForm[ElementOfForm, NotElementOfForm]

DefineLiteralInfixBinaryForm[ElementOfForm, "\[Element]"];
DefineLiteralInfixBinaryForm[NotElementOfForm, "\[NotElement]"];

DefineStandardTraditionalForm[{
  ElementOfForm[a_, as__, b_] :> MakeBoxes @ ElementOfForm[CommaRowForm[a, as], b],
  NotElementOfForm[a_, as__, b_] :> MakeBoxes @ ElementOfForm[CommaRowForm[a, as], b]
}];

(**************************************************************************************************)

PublicForm[PathRelationForm]

declareBoxFormatting[
  PathRelationForm[args__] :>
    TemplateBox[MapUnevaluated[wordBoxes, {args}], "PathRelationForm"],
  PathRelationForm[] :>
    SBox["PathRelationSymbol"]
]

$TemplateKatexFunction["PathRelationForm"] = katexAliasRiffled["pathIso"]
$TemplateKatexFunction["PathRelationSymbol"] = katexAlias["pathIso"];

(**************************************************************************************************)

PublicForm[TailEqualForm, HeadEqualForm, ApproxEqualForm, IsomorphicForm, HomeomorphicForm, CongruentForm, IdenticallyEqualForm, HomotopicForm, DefEqualForm, SyntaxEqualForm, DotEqualForm, ColonEqualForm]

declareInfixSymbol[{ApproxEqualForm, IsomorphicForm, HomeomorphicForm, HomotopicForm, DefEqualForm, SyntaxEqualForm,
  DotEqualForm, ColonEqualForm, TailEqualForm, HeadEqualForm, CongruentForm, IdenticallyEqualForm}];

(**************************************************************************************************)

PublicForm[EqualForm, NotEqualForm]

declareInfixSymbol[EqualForm] // usingCustomKatex[" = "];
declareInfixSymbol[NotEqualForm] // usingCustomKatex[" \\neq "];

(**************************************************************************************************)

PublicForm[LessForm, LessEqualForm, GreaterForm, GreaterEqualForm]

declareInfixSymbol[LessForm] // usingCustomKatex[" < "];
declareInfixSymbol[LessEqualForm] // usingCustomKatex[" \\le "];
declareInfixSymbol[GreaterForm] // usingCustomKatex[" > "];
declareInfixSymbol[GreaterEqualForm] // usingCustomKatex[" \\ge "];

(**************************************************************************************************)

PublicForm[SubsetForm, SubsetEqualForm, SupersetForm, SupersetEqualForm]

declareInfixSymbol[SubsetForm] // usingCustomKatex[" \\subset "];
declareInfixSymbol[SubsetEqualForm] // usingCustomKatex[" \\subseteq "];
declareInfixSymbol[SupersetForm] // usingCustomKatex[" \\supset "];
declareInfixSymbol[SupersetEqualForm] // usingCustomKatex[" \\supseteq "];

(**************************************************************************************************)

PublicForm[SubmultisetForm, SubmultisetEqualForm, SupermultisetForm, SupermultisetEqualForm]

declareInfixSymbol[SubmultisetForm] // usingCustomKatex[" \\submset "];
declareInfixSymbol[SubmultisetEqualForm] // usingCustomKatex[" \\submseteq "];
declareInfixSymbol[SupermultisetForm] // usingCustomKatex[" \\supmset "];
declareInfixSymbol[SupermultisetEqualForm] // usingCustomKatex[" \\supmseteq "];

(**************************************************************************************************)

PublicForm[BijectiveForm]

declareInfixSymbol[BijectiveForm];

(**************************************************************************************************)

PublicForm[BinaryRelationForm]

declareBoxFormatting[
  BinaryRelationForm[relation_String][args__] :>
    makeTypedTemplateBox[relation -> None, args, "BinaryRelationForm"]
]

$TemplateKatexFunction["BinaryRelationForm"] = Function[riffled[#1][##2]];

(**************************************************************************************************)

PublicForm[AppliedRelationForm]

declareBoxFormatting[
  AppliedRelationForm[a_, b_, c_] :> makeTemplateBox[a, b, c, "AppliedRelationForm"]
];

$TemplateKatexFunction["AppliedRelationForm"] = applyRiffled["appliedRelation", " "];
