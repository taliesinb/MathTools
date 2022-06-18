PublicForm[GradientForm]

declareBoxFormatting[
  GradientForm[q_] :> makeHintedTemplateBox[q -> VertexFieldSymbol, "GradientForm"],
  GradientForm[] :> SBox["GradientSymbol"]
];

$TemplateKatexFunction["GradientForm"] = "gradOf";
$TemplateKatexFunction["GradientSymbol"] = katexAlias["grad"];

(**************************************************************************************************)

PublicForm[DivergenceForm]

declareBoxFormatting[
  DivergenceForm[q_] :> makeHintedTemplateBox[q -> EdgeFieldSymbol, "DivergenceForm"],
  DivergenceForm[] :> SBox["DivergenceSymbol"]
];

$TemplateKatexFunction["DivergenceForm"] = "divOf";
$TemplateKatexFunction["DivergenceSymbol"] = katexAlias["div"];

(**************************************************************************************************)

PublicForm[LaplacianForm]

declareBoxFormatting[
  LaplacianForm[q_] :> makeHintedTemplateBox[q -> EdgeFieldSymbol, "LaplacianForm"],
  LaplacianForm[] :> SBox["LaplacianSymbol"]
];

$TemplateKatexFunction["LaplacianForm"] = "laplacianOf";
$TemplateKatexFunction["LaplacianSymbol"] = katexAlias["laplacian"];

(**************************************************************************************************)

PublicForm[PartialDifferentialOfForm]

declareBoxFormatting[
  PartialDifferentialOfForm[x_] :>
    makeTemplateBox[x, "PartialDifferentialOfForm"],
  PartialDifferentialOfForm[] :>
    SBox["PartialDifferentialSymbol"]
];

$TemplateKatexFunction["PartialDifferentialOfForm"] = "partialdof";
$TemplateKatexFunction["PartialDifferentialSymbol"] = katexAlias["partial"];

(**************************************************************************************************)

PublicForm[LieBracketForm]

declareBinaryForm[LieBracketForm];
