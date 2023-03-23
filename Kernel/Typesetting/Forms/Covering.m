PublicForm[CoversForm, CoveredByForm, StrictlyCoversForm, StrictlyCoveredByForm]

DefineInfixForm[CoversForm,            WideOpBox @ "\[SquareSupersetEqual]"]
DefineInfixForm[CoveredByForm,         WideOpBox @ "\[SquareSubsetEqual]"]
DefineInfixForm[StrictlyCoversForm,    WideOpBox @ "\[SquareSuperset]"]
DefineInfixForm[StrictlyCoveredByForm, WideOpBox @ "\[SquareSubset]"]

(**************************************************************************************************)

PublicForm[IndexedCoveringForm, GraphIndexedCoveringForm, QuiverIndexedCoveringForm]

DefineIndexedInfixBinaryForm[IndexedCoveringForm,       "\[SquareSupersetEqual]"]
DefineIndexedInfixBinaryForm[GraphIndexedCoveringForm,  "\[SquareSupersetEqual]"]
DefineIndexedInfixBinaryForm[QuiverIndexedCoveringForm, "\[SquareSupersetEqual]"]

(**************************************************************************************************)

PublicForm[ContractionLatticeSymbol]

DefineTaggedForm[ContractionLatticeSymbol];

(**************************************************************************************************)

PublicForm[ContractionProductForm, ContractionSumForm]

DefineInfixForm[ContractionProductForm, WideOpBox @ "\[CenterDot]"]
DefineInfixForm[ContractionSumForm,     WideOpBox @ "\[SquareUnion]"]

(**************************************************************************************************)

PublicForm[ContractionSetForm, OrderedContractionSetForm]

ContractionSetForm[{RepeatedNull[{_}]}] := "";

ContractionSetForm[e_List] :=
  ContractionSumForm @@ (ContractionProductForm @@@ DeleteCases[e, {_}])

OrderedContractionSetForm[index_][set_] :=
  ContractionSetForm @ SortContractionSet[DeleteCases[set, {_}], index]

(**************************************************************************************************)

PublicForm[IsContractedForm, IsNotContractedForm]

DefineIndexedInfixBinaryForm[IsContractedForm,    "\[Tilde]"]
DefineIndexedInfixBinaryForm[IsNotContractedForm, "\[NotTilde]"]

(**************************************************************************************************)

PublicForm[ContractedRelationForm]

DefineInfixForm[ContractedRelationForm, WideOpBox @ "\[Tilde]"]
