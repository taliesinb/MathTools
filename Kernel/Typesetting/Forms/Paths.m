PrivateTypesettingBoxFunction[graphOrQuiverBoxes]

graphOrQuiverBoxes = Case[
  g_GraphSymbol | g_QuiverSymbol := MakeBoxes @ g;
  c:(colorsP[_])                 := MakeBoxes @ c;
  s:symsP                        := MakeBoxes @ QuiverSymbol @ s;
  other_                         := MakeQGBoxes @ other;
,
  {colorsP -> $colorFormP, symsP -> $rawSymbolP}
]

(**************************************************************************************************)

PublicTypesettingForm[PathGroupoidSymbol]

PathGroupoidSymbol[] := PathGroupoidSymbol["Q"];

declareBoxFormatting[
  PathGroupoidSymbol[q_] :>
    TemplateBox[List @ graphOrQuiverBoxes @ q, "PathGroupoidSymbolForm"]
]

$TemplateKatexFunction["PathGroupoidSymbolForm"] = "pathGroupoid";

(**************************************************************************************************)

PublicTypesettingForm[PathQuiverSymbol]

SetUsage @ "
PathQuiverSymbol[q$] represents the path quiver on quiver q$.
"

declareSymbolForm[PathQuiverSymbol, QuiverSymbol];

(**************************************************************************************************)

PublicTypesettingForm[ForwardPathQuiverSymbol]

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

PublicTypesettingForm[BackwardPathQuiverSymbol]

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

PublicTypesettingForm[ParenPathWordForm, ParenEmptyPathWordForm]

declareBoxFormatting[
  ParenPathWordForm[a_, b_, c_] :> ReplacePart[MakeBoxes @ PathWordForm[a, b, c], 2 -> "ParenPathWordForm"],
  ParenEmptyPathWordForm[v_] :> ReplacePart[MakeBoxes @ EmptyPathWordForm[v], 2 -> "ParenPathWordForm"]
];

$TemplateKatexFunction["ParenPathWordForm"] = "parenPathWord";

(**************************************************************************************************)

PublicTypesettingForm[EmptyPathWordForm]

declareBoxFormatting[
  EmptyPathWordForm[v_] :>
    MakeBoxes @ PathWordForm[v, {}, v]
];

(**************************************************************************************************)

PublicTypesettingForm[PathWordForm]

PathWordForm[a_, b_Str, c_] := PathWordForm[a, ToPathWord @ b, c];

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
  s_Symbol | s_Str := MakeBoxes @ VertexSymbol @ s;
  e_ := MakeBoxes[e];
];

$TemplateKatexFunction["PathWordForm"] = "pathWord";

(**************************************************************************************************)

PublicTypesettingForm[PathHomomorphismSymbol]

PathHomomorphismSymbol[] := PathHomomorphismSymbol["\[Rho]"]

declareSymbolForm[PathHomomorphismSymbol];

(**************************************************************************************************)

PublicTypesettingForm[PathMapSymbol]

PathMapSymbol[] := PathMapSymbol["\[Mu]"];

declareBoxFormatting[
  PathMapSymbol[mu_] :>
    makeTemplateBox[mu, "PathMapSymbolForm"]
]

$TemplateKatexFunction["PathMapSymbolForm"] = "pathMap";

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
  other_   := MakeQGBoxes @ other;
];

(**************************************************************************************************)

PublicSymbol[NullPath, NullElement]

declareConstantSymbol[{NullPath, NullElement}];

(**************************************************************************************************)

PublicTypesettingForm[PathSymbol]

PathSymbol[] := PathSymbol["P"];

declareSymbolForm[PathSymbol];
