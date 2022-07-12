PublicForm[CoversForm, CoveredByForm, StrictlyCoversForm, StrictlyCoveredByForm]

DeclareBinaryRelationTemplateBox[CoversForm, "\[SquareSupersetEqual]"]
DeclareBinaryRelationTemplateBox[CoveredByForm, "\[SquareSubsetEqual]"]
DeclareBinaryRelationTemplateBox[StrictlyCoversForm, "\[SquareSuperset]"]
DeclareBinaryRelationTemplateBox[StrictlyCoveredByForm, "\[SquareSubset]"]

(**************************************************************************************************)

PublicForm[IndexedCoveringForm, GraphIndexedCoveringForm, QuiverIndexedCoveringForm]

DeclareIndexedBinaryRelationTemplateBox[IndexedCoveringForm, "\[SquareSupersetEqual]"]
DeclareIndexedBinaryRelationTemplateBox[GraphIndexedCoveringForm, "\[SquareSupersetEqual]"]
DeclareIndexedBinaryRelationTemplateBox[QuiverIndexedCoveringForm, "\[SquareSupersetEqual]"]

(**************************************************************************************************)

PublicForm[ContractionLatticeSymbol]

DeclareDerivedSymbolTemplateBox[ContractionLatticeSymbol]

(**************************************************************************************************)

PublicForm[ContractionProductForm, ContractionSumForm]

DeclareNAryOperatorTemplateBox[ContractionProductForm, "\[CenterDot]"]
DeclareNAryOperatorTemplateBox[ContractionSumForm, "\[SquareUnion]"]

(**************************************************************************************************)

PublicForm[ContractionSetForm, OrderedContractionSetForm]

ContractionSetForm[{RepeatedNull[{_}]}] := "";

ContractionSetForm[e_List] :=
  ContractionSumForm @@ (ContractionProductForm @@@ DeleteCases[e, {_}])

OrderedContractionSetForm[index_][set_] :=
  ContractionSetForm @ SortContractionSet[DeleteCases[set, {_}], index]

(**************************************************************************************************)

PublicForm[IsContractedForm, IsNotContractedForm]

DeclareIndexedBinaryRelationTemplateBox[IsContractedForm, "\[Tilde]"]
DeclareIndexedBinaryRelationTemplateBox[IsNotContractedForm, "\[NotTilde]"]

(**************************************************************************************************)

PublicForm[ContractedRelationForm]

DeclareNAryRelationTemplateBox[ContractedRelationForm, "\[Tilde]"]
