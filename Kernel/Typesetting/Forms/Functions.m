PublicForm[FunctionSignatureForm]

FunctionSignatureForm[f_, a_List, b_] :=
  FunctionSignatureForm[f, TupleForm @@ a, b];

FunctionSignatureForm[f_, a_, b_List] :=
  FunctionSignatureForm[f, a, TupleForm @@ b];

declareBoxFormatting[
  FunctionSignatureForm[f_, a_, b_] :>
    makeTypedTemplateBox[f -> FunctionSymbol, a, b, "FunctionSignatureForm"]
]

$TemplateKatexFunction["FunctionSignatureForm"] = "functionSignature";

(**************************************************************************************************)

PublicForm[PartialFunctionSignatureForm]

PartialFunctionSignatureForm[f_, a_List, b_] :=
  PartialFunctionSignatureForm[f, TupleForm @@ a, b];

PartialFunctionSignatureForm[f_, a_, b_List] :=
  PartialFunctionSignatureForm[f, a, TupleForm @@ b];

declareBoxFormatting[
  PartialFunctionSignatureForm[f_, a_, b_] :>
    makeTypedTemplateBox[f -> FunctionSymbol, a, b, "PartialFunctionSignatureForm"]
]

$TemplateKatexFunction["PartialFunctionSignatureForm"] = "partialFunctionSignature";

(**************************************************************************************************)

PublicForm[FunctionGraphForm]

declareUnaryForm[FunctionGraphForm];

(**************************************************************************************************)

PublicForm[FunctionSpaceForm, FiniteTotalFunctionSpaceForm]

declareBoxFormatting[
  FunctionSpaceForm[from_, to_] :> makeHintedTemplateBox[from, to -> BaseFieldSymbol, "FunctionSpaceForm"],
  FiniteTotalFunctionSpaceForm[from_, to_] :> makeHintedTemplateBox[from, to -> BaseFieldSymbol, "FiniteTotalFunctionSpaceForm"]
];

$TemplateKatexFunction["FunctionSpaceForm"] = "functionSpace"
$TemplateKatexFunction["FiniteTotalFunctionSpaceForm"] = "finiteTotalFunctionSpace"

(**************************************************************************************************)

PublicForm[FunctionTypeForm]

declareBinaryForm[FunctionTypeForm]

(**************************************************************************************************)

PublicForm[FunctionSymbol]
PrivateVariable[$namedFunctions]

$namedFunctions = {
  VertexListFunction,
  AndFunction,
  OrFunction,
  NotFunction,
  EdgeListFunction,
  CardinalListFunction,
  SignedCardinalListFunction,
  SignedLengthFunction,
  LengthFunction,
  WordFunction,
  PathListFunction,
  HeadVertexFunction, TailVertexFunction,
  AutomorphismsFunction, EndomorphismsFunction,
  BasisFunction,
  SupportFunction,
  SplitFunction,
  LCMFunction,
  GradeFunction,
  ModFunction,
  MinimalContractionsFunction,
  MinimalContractionSetsFunction,
  CoefficientFunction,
  MaxFunction,
  MinFunction,
  SinFunction,
  CosFunction,
  TanFunction,
  ArcTanFunction,
  TorusFunction,
  MobiusFunction,
  ClipFunction,
  SignFunction,
  StepFunction,
  DomainFunction,
  CodomainFunction,
  ProjectionFunction,
  LiftFunction,
  IdentityFunction,
  TotalFunction,
  
  StateJoinFunction,
  StateMeetFunction,
  StateExtentFunction,
  StateIntentFunction,
  StateComposeFunction,
  StateDecomposeFunction
};

$functionHeads = {
  FunctionSymbol, PathMapSymbol,
  GroupoidHomomorphismSymbol, GroupHomomorphismSymbol,
  GroupoidFunctionSymbol, GroupFunctionSymbol,
  PathHomomorphismSymbol, GraphHomomorphismSymbol,
  InverseForm,
  VertexFieldSymbol, EdgeFieldSymbol,
  TransportMapSymbol,
  VertexSymbol, QuiverSymbol
}

setupGrabbingRule[sym_] := (
  sym /: Subscript[sym[inner_], rest__] := sym[Subscript[inner, rest]];
  sym /: Superscript[sym[inner_], rest__] := sym[Superscript[inner, rest]];
  sym /: Subsuperscript[sym[inner_], rest__] := sym[Subsuperscript[inner, rest]];
  f_sym[args__] := AppliedForm[f, args];
);

PrivateVariable[$functionFormP]

Scan[setupGrabbingRule, $functionHeads];

$functionFormP = Alternatives @@ Join[
  Blank /@ $functionHeads,
  $namedFunctions
];

SetHoldAllComplete[symOrStringMatchingQ];

symOrStringMatchingQ[s_String, patt_] := StringMatchQ[s, patt];
symOrStringMatchingQ[s_Symbol, patt_] := StringMatchQ[SymbolName[Unevaluated @ s], patt];
symOrStringMatchingQ[_, _] := False;

declareBoxFormatting[

  FunctionSymbol[f:$functionFormP] :>
    MakeBoxes @ f,

  FunctionSymbol[f_] :>
    makeTemplateBox[f, "FunctionSymbolForm"]
];

$TemplateKatexFunction["FunctionSymbolForm"] = "function";

(**************************************************************************************************)

PublicForm[FunctionCompositionForm]

declareInfixSymbol[FunctionCompositionForm, FunctionSymbol, True];

declareBoxFormatting[
  f_FunctionCompositionForm[args___] :> MakeBoxes[AppliedForm[ParenthesesForm[f], args]]
]

(**************************************************************************************************)

PublicForm[AppliedForm]

declareBoxFormatting[
  AppliedForm[f_, args__] :> makeTypedTemplateBox[f -> FunctionSymbol, args, "AppliedForm"]
];

$TemplateKatexFunction["AppliedForm"] = appliedKatex;

appliedKatex[f_, args___] := {f, "(", Riffle[{args}, ","], ")"};

(**************************************************************************************************)

PublicForm[OperatorAppliedForm]

declareBoxFormatting[
  OperatorAppliedForm[f_, g_] :> makeTemplateBox[f, g, "OperatorAppliedForm"]
];

$TemplateKatexFunction["OperatorAppliedForm"] = operatorAppliedKatex;

operatorAppliedKatex[f_, g_] := {f, "\,", g};

(**************************************************************************************************)

PublicSymbol[AndFunction, OrFunction, NotFunction]

PublicSymbol[VertexListFunction, MinimalContractionsFunction, MinimalContractionSetsFunction, EdgeListFunction, PathListFunction, CardinalListFunction, SignedCardinalListFunction, SignedLengthFunction, LengthFunction, LCMFunction, HeadVertexFunction, TailVertexFunction, SplitFunction, WordFunction, AutomorphismsFunction, EndomorphismsFunction, BasisFunction, SupportFunction, GradeFunction, ModFunction, CoefficientFunction, MaxFunction, MinFunction, SinFunction, CosFunction, TanFunction, ArcTanFunction, TorusFunction, MobiusFunction]

PublicSymbol[ClipFunction, SignFunction, StepFunction, DomainFunction, CodomainFunction, ProjectionFunction, LiftFunction, IdentityFunction, TotalFunction]

PublicSymbol[StateMeetFunction, StateJoinFunction, StateExtentFunction, StateIntentFunction]

PublicSymbol[StateComposeFunction, StateDecomposeFunction]

declareFunctionFormatting[sym_] := With[
  {name = StringDelete[SymbolName[sym], "Function"]},
  declareBoxFormatting[
    sym :> TemplateBox[{name}, "NamedFunctionSymbolForm"],
    sym[args___] :> makeTypedTemplateBox[sym, args, "AppliedForm"]
  ]
];

Scan[declareFunctionFormatting, $namedFunctions];

$TemplateKatexFunction["NamedFunctionSymbolForm"] = namedFuncKatex;

(* to avoid conflict with built-in Katex: *)
namedFuncKatex["And"] := "\\andFn";
namedFuncKatex["Or"] := "\\orFn";
namedFuncKatex["Not"] := "\\Not";

namedFuncKatex["Word"] := "\\wordOf"; (* because 'word' already menas wordForm *)
namedFuncKatex["LCM"] := "\\lcm"; (* lowercase *)
namedFuncKatex["Mod"] := "\\modFunction"; (* becuase mod already defined *)
namedFuncKatex[s_String] := namedFuncKatex[s] = StringJoin["\\", LowerCaseFirst @ s];
