declareTypeForm::badsym = "Name of symbol `` should end in Form."
declareTypeForm[head_Symbol, katex_:Auto] := With[
  {name = SymbolName @ head},
  If[!SEndsQ[name, "Form"], ReturnFailed["badsym", head]];
  declareBoxFormatting[head[s_] :> TemplateBox[List @ MakeMathBoxes @ s, name]];
  $unaryWrapperFormName[head] = name;
  $TemplateKatexFunction[name] = If[katex === Auto, LowerCaseFirst @ SDrop[name, -4], katex];
];

(**************************************************************************************************)

PublicTypesettingForm[PathTypeForm, VertexTypeForm, EdgeTypeForm]

declareTypeForm[PathTypeForm]
declareTypeForm[VertexTypeForm]
declareTypeForm[EdgeTypeForm]

(**************************************************************************************************)

PublicTypesettingForm[SetTypeForm, SignedSetTypeForm, OrderedSetTypeForm, ListTypeForm, CyclicListTypeForm, TupleTypeForm, MultisetTypeForm, SignedMultisetTypeForm]

(* TODO: These aren't used *)
declareTypeForm[SetTypeForm]
declareTypeForm[SignedSetTypeForm]
declareTypeForm[OrderedSetTypeForm]
declareTypeForm[ListTypeForm]
declareTypeForm[CyclicListTypeForm]
declareTypeForm[TupleTypeForm]
declareTypeForm[MultisetTypeForm]
declareTypeForm[SignedMultisetTypeForm]