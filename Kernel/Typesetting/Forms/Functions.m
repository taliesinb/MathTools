PublicTypesettingForm[FunctionSignatureForm]

FunctionSignatureForm[f_, a_List, b_] :=
  FunctionSignatureForm[f, TupleForm @@ a, b];

FunctionSignatureForm[f_, a_, b_List] :=
  FunctionSignatureForm[f, a, TupleForm @@ b];

DefineTernaryForm[FunctionSignatureForm, RBox[$1, OpBox @ ":", $2, OpBox @ "\[Rule]", $3], KatexMacroName -> "fs"];

(**************************************************************************************************)

PublicTypesettingForm[PartialFunctionSignatureForm]

PartialFunctionSignatureForm[f_, a_List, b_] :=
  PartialFunctionSignatureForm[f, TupleForm @@ a, b];

PartialFunctionSignatureForm[f_, a_, b_List] :=
  PartialFunctionSignatureForm[f, a, TupleForm @@ b];

DefineTernaryForm[PartialFunctionSignatureForm, RBox[$1, OpBox @ ":", $2, OpBox @ "\[RightVector]", $3]];

(**************************************************************************************************)

PublicTypesettingForm[FunctionGraphForm]

DefineUnaryForm[FunctionGraphForm, SubscriptBox["G", $1]];

(**************************************************************************************************)

PublicTypesettingForm[FunctionSpaceForm, FiniteTotalFunctionSpaceForm]

DefineBinaryForm[FunctionSpaceForm, SubscriptBox[$2, $1]];
DefineBinaryForm[FiniteTotalFunctionSpaceForm, SuperscriptBox[$2, RBox["\[Subset]", $1]]]

(**************************************************************************************************)

PublicTypesettingForm[FunctionTypeForm]

DefineBinaryForm[FunctionTypeForm, RBox[$1, " \[Rule] ", $2]]

(**************************************************************************************************)

PublicTypesettingForm[FunctionCompositionForm, RightFunctionCompositionForm]

DefineInfixForm[FunctionCompositionForm,      OpBox @ "\[SmallCircle]"];
DefineInfixForm[RightFunctionCompositionForm, OpBox @ "\[FilledSmallCircle]"];

DefineStandardTraditionalForm[{
  f_FunctionCompositionForm[args___] :> MakeQGBoxes[AppliedForm[ParenthesesForm[f], args]],
  f_RightFunctionCompositionForm[args___] :> MakeQGBoxes[AppliedForm[ParenthesesForm[f], args]]
}]

(**************************************************************************************************)

(* replaces OperatorAppliedForm *)
PublicTypesettingForm[OperatorCompositionForm]

DefineInfixForm[OperatorCompositionForm, KBox["\[InvisibleSpace]", "\\,"]];

(**************************************************************************************************)

PublicTypesettingForm[FunctionSymbolForm]

DefineTaggedForm[FunctionSymbolForm]

DefineStandardTraditionalForm[fn_FunctionSymbolForm[args___] :> MakeBoxes[AppliedForm[fn, args]]];

(**************************************************************************************************)

PublicTypesettingForm[NamedFunctionForm]

DefineUnaryForm[NamedFunctionForm, FunctionBox @ $1]

DefineStandardTraditionalForm[nfs_NamedFunctionForm[args___] :> MakeBoxes[AppliedForm[nfs, args]]];

(**************************************************************************************************)

PublicSymbol[AndFunction, OrFunction, NotFunction, XorFunction, NandFunction, ParityFunction]

DefineNamedFunctionSymbolForm[{
  AndFunction,
  OrFunction,
  NotFunction,
  XorFunction,
  NandFunction,
  ParityFunction
}]

(**************************************************************************************************)

PublicSymbol[AutomorphismsFunction, GraphAutomorphismsFunction, EndomorphismsFunction, GraphEndomorphismsFunction]

DefineNamedFunctionSymbolForm[{
  AutomorphismsFunction -> "Aut",
  GraphAutomorphismsFunction -> "Aut",
  EndomorphismsFunction -> "End",
  GraphEndomorphismsFunction -> "End"
}];

(**************************************************************************************************)

PublicSymbol[VertexListFunction, MinimalContractionsFunction, MinimalContractionSetsFunction, EdgeListFunction, PathListFunction, CardinalListFunction, SignedCardinalListFunction, SignedLengthFunction, LengthFunction, LCMFunction, HeadVertexFunction, TailVertexFunction, WordFunction]

DefineNamedFunctionSymbolForm[{
  VertexListFunction -> "vertices",
  MinimalContractionsFunction -> "MCSets",
  MinimalContractionSetsFunction -> "MC",
  EdgeListFunction -> "edges",
  PathListFunction -> "paths",
  CardinalListFunction -> "cards",
  SignedCardinalListFunction -> "cards*",
  SignedLengthFunction -> "len*",
  LengthFunction -> "len",
  LCMFunction -> "lcm",
  HeadVertexFunction -> "head",
  TailVertexFunction -> "tail",
  WordFunction
}]

(**************************************************************************************************)

PublicSymbol[SplitFunction, BasisFunction, SupportFunction, GradeFunction, CoefficientFunction]

DefineNamedFunctionSymbolForm[{
  SplitFunction,
  BasisFunction,
  SupportFunction -> "supp",
  GradeFunction,
  ModFunction,
  CoefficientFunction -> "coeff",
  TorusFunction,
  MobiusFunction
}]

(**************************************************************************************************)

PublicSymbol[LimitFunction, ColimitFunction]

DefineNamedFunctionSymbolForm[{
  LimitFunction -> "lim",
  ColimitFunction -> "colim"
}]

(**************************************************************************************************)

PublicSymbol[TorusFunction, MobiusFunction]

DefineNamedFunctionSymbolForm[{
  TorusFunction,
  MobiusFunction
}]

(**************************************************************************************************)

PublicSymbol[MaxFunction, MinFunction, SinFunction, CosFunction, TanFunction, ArcTanFunction]

DefineNamedFunctionSymbolForm[{
  MaxFunction,
  MinFunction,
  SinFunction,
  CosFunction,
  TanFunction,
  ArcTanFunction -> "atan"
}]

(**************************************************************************************************)

PublicSymbol[ModFunction, ClipFunction, SignFunction, StepFunction, DomainFunction, CodomainFunction, ProjectionFunction, LiftFunction, IdentityFunction, TotalFunction]

DefineNamedFunctionSymbolForm[{
  ModFunction,
  ClipFunction,
  SignFunction -> "sgn",
  StepFunction,
  DomainFunction -> "dom",
  CodomainFunction -> "cod",
  ProjectionFunction -> "proj",
  LiftFunction,
  IdentityFunction -> "id",
  TotalFunction -> "tot"
}]

(**************************************************************************************************)

PublicSymbol[StateMeetFunction, StateJoinFunction, StateExtentFunction, StateIntentFunction, StateComposeFunction, StateDecomposeFunction]

DefineNamedFunctionSymbolForm[{
  StateMeetFunction -> "disj",
  StateJoinFunction -> "conj",
  StateExtentFunction -> "extent",
  StateIntentFunction -> "intent",
  StateComposeFunction -> "glue",
  StateDecomposeFunction -> "melt"
}]

(**************************************************************************************************)

PublicSymbol[SrcFunction, SourceFunction, TargetFunction, TgtFunction]

DefineNamedFunctionSymbolForm[{
  SrcFunction -> "src", SourceFunction,
  TgtFunction -> "tgt", TargetFunction
}]

(**************************************************************************************************)

PublicTypesettingForm[FromToForm]

DefineInfixBinaryForm[FromToForm, WideOpBox @ "\[RightTeeArrow]"];

(**************************************************************************************************)

PublicTypesettingForm[MapsToForm]

DefineInfixBinaryForm[MapsToForm, WideOpBox @ "\[RightTeeArrow]"];

(**************************************************************************************************)

PublicTypesettingForm[MapsBetweenForm]

DefineInfixBinaryForm[MapsBetweenForm, WideOpBox @ "\[LeftRightArrow]"];
