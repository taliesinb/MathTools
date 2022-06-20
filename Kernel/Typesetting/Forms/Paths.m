PrivateFunction[graphOrQuiverBoxes]

graphOrQuiverBoxes = Case[
  g_GraphSymbol | g_QuiverSymbol := MakeBoxes @ g;
  c:(colorsP[_])                 := MakeBoxes @ c;
  s:symsP                        := MakeBoxes @ QuiverSymbol @ s;
  other_                         := makeQGBoxes @ other;
,
  {colorsP -> $colorFormP, symsP -> $rawSymbolP}
]

(**************************************************************************************************)

PublicForm[PathGroupoidSymbol]

PathGroupoidSymbol[] := PathGroupoidSymbol["Q"];

declareBoxFormatting[
  PathGroupoidSymbol[q_] :>
    TemplateBox[List @ graphOrQuiverBoxes @ q, "PathGroupoidSymbolForm"]
]

$TemplateKatexFunction["PathGroupoidSymbolForm"] = "pathGroupoid";

(**************************************************************************************************)

PublicForm[PathQuiverSymbol]

SetUsage @ "
PathQuiverSymbol[q$] represents the path quiver on quiver q$.
"

declareSymbolForm[PathQuiverSymbol, QuiverSymbol];

(**************************************************************************************************)

PublicForm[ForwardPathQuiverSymbol]

SetUsage @ "
ForwardPathQuiverSymbol[q$] represents the path quiver on quiver q$.
"

ForwardPathQuiverSymbol[] := ForwardPathQuiverSymbol["Q", "v"];
ForwardPathQuiverSymbol[q_] := ForwardPathQuiverSymbol[q, "v"];

declareBoxFormatting[
  HoldPattern[ForwardPathQuiverSymbol[q_, v_]] :>
    makeHintedTemplateBox[q -> QuiverSymbol, v -> VertexSymbol, "ForwardPathQuiverSymbolForm"]
];

$TemplateKatexFunction["ForwardPathQuiverSymbolForm"] = "forwardPathQuiver";


(**************************************************************************************************)

PublicForm[BackwardPathQuiverSymbol]

SetUsage @ "
BackwardPathQuiverSymbol[q$] represents the path quiver on quiver q$.
"

BackwardPathQuiverSymbol[] := BackwardPathQuiverSymbol["Q", "v"];
BackwardPathQuiverSymbol[q_] := BackwardPathQuiverSymbol[q, "v"];

declareBoxFormatting[
  HoldPattern[BackwardPathQuiverSymbol[q_, v_]] :>
    makeTypedTemplateBox[q -> QuiverSymbol, v -> VertexSymbol, "BackwardPathQuiverSymbolForm"]
];

$TemplateKatexFunction["BackwardPathQuiverSymbolForm"] = "backwardPathQuiver";

(**************************************************************************************************)

PublicForm[ParenPathWordForm, ParenEmptyPathWordForm]

declareBoxFormatting[
  ParenPathWordForm[a_, b_, c_] :> ReplacePart[MakeBoxes @ PathWordForm[a, b, c], 2 -> "ParenPathWordForm"],
  ParenEmptyPathWordForm[v_] :> ReplacePart[MakeBoxes @ EmptyPathWordForm[v], 2 -> "ParenPathWordForm"]
];

$TemplateKatexFunction["ParenPathWordForm"] = "parenPathWord";

(**************************************************************************************************)

PublicForm[EmptyPathWordForm]

declareBoxFormatting[
  EmptyPathWordForm[v_] :>
    MakeBoxes @ PathWordForm[v, {}, v]
];

(**************************************************************************************************)

PublicForm[PathWordForm]

PathWordForm[a_, b_String, c_] := PathWordForm[a, ToPathWord @ b, c];

declareBoxFormatting[
  PathWordForm[t_, w_, h_] :>
    makeTypedTemplateBox[t -> generalizedVertexSymbol, w -> WordForm, h -> generalizedVertexSymbol, "PathWordForm"]
];

PrivateFunction[generalizedVertexSymbol]

SetHoldAllComplete[generalizedVertexSymbol, generalizedVertexBoxes];

declareBoxFormatting[
  generalizedVertexSymbol[e_] :> recurseWrapperBoxes[e, generalizedVertexBoxes]
];

generalizedVertexBoxes = Case[
  s_Symbol | s_String := MakeBoxes @ VertexSymbol @ s;
  e_ := MakeBoxes[e];
];

$TemplateKatexFunction["PathWordForm"] = "pathWord";

(**************************************************************************************************)

PublicForm[PathHomomorphismSymbol]

PathHomomorphismSymbol[] := PathHomomorphismSymbol["\[Rho]"]

declareSymbolForm[PathHomomorphismSymbol];

(**************************************************************************************************)

PublicForm[PathMapSymbol]

PathMapSymbol[] := PathMapSymbol["\[Mu]"];

declareBoxFormatting[
  PathMapSymbol[mu_] :>
    makeTemplateBox[mu, "PathMapSymbolForm"]
]

$TemplateKatexFunction["PathMapSymbolForm"] = "pathMap";

(**************************************************************************************************)

PublicForm[PathWordRewritingForm]

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
  other_   := makeQGBoxes @ other;
];

(**************************************************************************************************)

PublicSymbol[NullPath, NullElement]

declareConstantSymbol[{NullPath, NullElement}];

(**************************************************************************************************)

PublicForm[PathSymbol]

PathSymbol[] := PathSymbol["P"];

declareSymbolForm[PathSymbol];

