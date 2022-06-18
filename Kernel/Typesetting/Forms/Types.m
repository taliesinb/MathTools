declareTypeForm::badsym = "Name of symbol `` should end in Form."
declareTypeForm[head_Symbol, katex_:Automatic] := With[
  {name = SymbolName @ head},
  If[!StringEndsQ[name, "Form"], ReturnFailed["badsym", head]];
  declareBoxFormatting[head[s_] :> TemplateBox[List @ makeQGBoxes @ s, name]];
  $unaryWrapperFormName[head] = name;
  $TemplateKatexFunction[name] = If[katex === Automatic, LowerCaseFirst @ StringDrop[name, -4], katex];
];

(**************************************************************************************************)

PublicForm[PathTypeForm, VertexTypeForm, EdgeTypeForm]

declareTypeForm[PathTypeForm]
declareTypeForm[VertexTypeForm]
declareTypeForm[EdgeTypeForm]

(**************************************************************************************************)

PublicForm[SetTypeForm, SignedSetTypeForm, OrderedSetTypeForm, ListTypeForm, CyclicListTypeForm, TupleTypeForm, MultisetTypeForm, SignedMultisetTypeForm]

(* TODO: These aren't used *)
declareTypeForm[SetTypeForm]
declareTypeForm[SignedSetTypeForm]
declareTypeForm[OrderedSetTypeForm]
declareTypeForm[ListTypeForm]
declareTypeForm[CyclicListTypeForm]
declareTypeForm[TupleTypeForm]
declareTypeForm[MultisetTypeForm]
declareTypeForm[SignedMultisetTypeForm]