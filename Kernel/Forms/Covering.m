PublicTypesettingForm[CoversForm, CoveredByForm, StrictlyCoversForm, StrictlyCoveredByForm]

DefineInfixForm[CoversForm,            WideOpBox @ "\[SquareSupersetEqual]"]
DefineInfixForm[CoveredByForm,         WideOpBox @ "\[SquareSubsetEqual]"]
DefineInfixForm[StrictlyCoversForm,    WideOpBox @ "\[SquareSuperset]"]
DefineInfixForm[StrictlyCoveredByForm, WideOpBox @ "\[SquareSubset]"]

(**************************************************************************************************)

PublicTypesettingForm[IndexedCoveringForm, GraphIndexedCoveringForm, QuiverIndexedCoveringForm]

DefineIndexedInfixBinaryForm[IndexedCoveringForm,       "\[SquareSupersetEqual]"]
DefineIndexedInfixBinaryForm[GraphIndexedCoveringForm,  "\[SquareSupersetEqual]"]
DefineIndexedInfixBinaryForm[QuiverIndexedCoveringForm, "\[SquareSupersetEqual]"]

(**************************************************************************************************)

PublicTypesettingForm[ContractionLatticeSymbol]

DefineTaggedForm[ContractionLatticeSymbol];

(**************************************************************************************************)

PublicTypesettingForm[ContractionProductForm, ContractionSumForm]

DefineInfixForm[ContractionProductForm, WideOpBox @ "\[CenterDot]"]
DefineInfixForm[ContractionSumForm,     WideOpBox @ "\[SquareUnion]"]

(**************************************************************************************************)

PublicTypesettingForm[ContractionSetForm, OrderedContractionSetForm]

ContractionSetForm[{RepeatedNull[{_}]}] := "";

ContractionSetForm[e_List] :=
  ContractionSumForm @@ (ContractionProductForm @@@ Decases[e, {_}])

OrderedContractionSetForm[index_][set_] :=
  ContractionSetForm @ SortContractionSet[Decases[set, {_}], index]

(**************************************************************************************************)

PublicTypesettingForm[IsContractedForm, IsNotContractedForm]

DefineIndexedInfixBinaryForm[IsContractedForm,    "\[Tilde]"]
DefineIndexedInfixBinaryForm[IsNotContractedForm, "\[NotTilde]"]

(**************************************************************************************************)

PublicTypesettingForm[ContractedRelationForm]

DefineInfixForm[ContractedRelationForm, WideOpBox @ "\[Tilde]"]
