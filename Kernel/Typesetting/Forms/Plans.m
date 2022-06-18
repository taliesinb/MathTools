PublicForm[RouteSymbol, MultirouteSymbol, PlanSymbol, MultiwordSymbol]

declareSymbolFormExplicit[RouteSymbol];
declareSymbolFormExplicit[MultirouteSymbol];
declareSymbolFormExplicit[PlanSymbol];
declareSymbolFormExplicit[MultiwordSymbol];

(**************************************************************************************************)

PublicForm[RouteForm, MultirouteForm]

RouteForm[a_, b_String, c_] := RouteForm[a, ToPathWord @ b, c];

declareBoxFormatting[
  RouteForm[t_, w_, h_] :>
    makeTypedTemplateBox[t -> generalizedVertexSymbol, w -> WordForm, h -> generalizedVertexSymbol, "RouteForm"],
  MultirouteForm[t_, w_, h_] :>
    makeTypedTemplateBox[t -> generalizedVertexSymbol, w, h -> generalizedVertexSymbol, "MultirouteForm"]
];

$TemplateKatexFunction["RouteForm"] = "route";
$TemplateKatexFunction["MultirouteForm"] = "multiroute";

(**************************************************************************************************)

PublicForm[PlanRingSymbol]

declareSymbolForm[PlanRingSymbol, QuiverSymbol]

declareBoxFormatting[
  PlanRingSymbol[] :> SBox["PlanRingSymbol"]
];

$TemplateKatexFunction["PlanRingSymbol"] = katexAlias["planRingSymbol"];
