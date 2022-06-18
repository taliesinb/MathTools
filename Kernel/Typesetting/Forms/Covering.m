PublicForm[CoversForm, CoveredByForm, StrictlyCoversForm, StrictlyCoveredByForm]

declareInfixSymbol[{CoversForm, CoveredByForm, StrictlyCoversForm, StrictlyCoveredByForm}, GraphSymbol];

(**************************************************************************************************)

PublicForm[GraphIndexedCoveringForm, QuiverIndexedCoveringForm]

declareBoxFormatting[
  GraphIndexedCoveringForm[pi_, g_, h_] :> makeHintedTemplateBox[pi -> GraphHomomorphismSymbol, g -> GraphSymbol, h -> GraphSymbol, "GraphIndexedCoveringForm"],
  QuiverIndexedCoveringForm[pi_, g_, h_] :> makeHintedTemplateBox[pi -> GraphHomomorphismSymbol, g -> QuiverSymbol, h -> QuiverSymbol, "QuiverIndexedCoveringForm"]
];

$TemplateKatexFunction["IndexedCoveringForm"] = "graphCovering";
$TemplateKatexFunction["GraphIndexedCoveringForm"] = "graphCovering";
$TemplateKatexFunction["QuiverIndexedCoveringForm"] = "quiverCovering";

(**************************************************************************************************)

PublicForm[ContractionLatticeSymbol]

SetUsage @ "
ContractionLatticeSymbol[q$] represents the lattice of contractions of a quiver q$.
"

declareSymbolForm[ContractionLatticeSymbol, QuiverSymbol];

(**************************************************************************************************)

PublicForm[ContractionProductForm, ContractionSumForm]

declareInfixSymbol[{ContractionProductForm, ContractionSumForm}, None, True];

(**************************************************************************************************)

PublicForm[ContractionSetForm]

ContractionSetForm[{RepeatedNull[{_}]}] := "";

ContractionSetForm[e_List] :=
  ContractionSumForm @@ (ContractionProductForm @@@ DeleteCases[e, {_}])

(**************************************************************************************************)

PublicForm[OrderedContractionSetForm]

OrderedContractionSetForm[index_][set_] :=
  ContractionSetForm @ SortContractionSet[DeleteCases[set, {_}], index]

(**************************************************************************************************)

PublicForm[IsContractedForm, IsNotContractedForm]

declareBoxFormatting[
  IsContractedForm[a_, b_] :>
    makeHintedTemplateBox[a -> VertexSymbol, b -> VertexSymbol, "IsContractedForm"],
  IsContractedForm[a_, b_, q_] :>
    makeHintedTemplateBox[a -> VertexSymbol, b -> VertexSymbol, q -> QuiverSymbol, "IsContractedInForm"],
  IsNotContractedForm[a_, b_] :>
    makeHintedTemplateBox[a -> VertexSymbol, b -> VertexSymbol, "IsNotContractedForm"],
  IsNotContractedForm[a_, b_, q_] :>
    makeHintedTemplateBox[a -> VertexSymbol, b -> VertexSymbol, q -> QuiverSymbol, "IsNotContractedInForm"]
];

$TemplateKatexFunction["IsContractedForm"] = "isContracted"
$TemplateKatexFunction["IsContractedInForm"] = "isContractedIn"
$TemplateKatexFunction["IsNotContractedForm"] = "isNotContracted"
$TemplateKatexFunction["IsNotContractedInForm"] = "isNotContractedIn"

(**************************************************************************************************)

PublicForm[ContractedRelationForm]

declareUnaryForm[ContractedRelationForm];
