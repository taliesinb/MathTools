PublicTypesettingForm[VertexListFunction, MinimalContractionsFunction, MinimalContractionSetsFunction, EdgeListFunction, PathListFunction, CardinalListFunction, SignedCardinalListFunction, SignedLengthFunction, LengthFunction, LCMFunction, HeadVertexFunction, TailVertexFunction, WordFunction]

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

PublicTypesettingForm[SplitFunction, BasisFunction, SupportFunction, GradeFunction, CoefficientFunction]

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

PublicTypesettingForm[TorusFunction, MobiusFunction]

DefineNamedFunctionSymbolForm[{
  TorusFunction,
  MobiusFunction
}]

(**************************************************************************************************)

PublicTypesettingForm[ModFunction, ClipFunction, SignFunction, StepFunction, ProjectionFunction, LiftFunction, TotalFunction]

DefineNamedFunctionSymbolForm[{
  ModFunction,
  ClipFunction,
  SignFunction -> "sgn",
  StepFunction,
  ProjectionFunction -> "proj",
  LiftFunction,
  TotalFunction -> "tot"
}]

(**************************************************************************************************)

PublicTypesettingForm[StateMeetFunction, StateJoinFunction, StateExtentFunction, StateIntentFunction, StateComposeFunction, StateDecomposeFunction]

DefineNamedFunctionSymbolForm[{
  StateMeetFunction -> "disj",
  StateJoinFunction -> "conj",
  StateExtentFunction -> "extent",
  StateIntentFunction -> "intent",
  StateComposeFunction -> "glue",
  StateDecomposeFunction -> "melt"
}]
