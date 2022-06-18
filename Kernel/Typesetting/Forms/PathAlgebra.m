toPathWordOrSymbol = Case[
  s:("P" | "Q" | "R" | "T")           := PathSymbol @ s;
  c:"x" | "y" | "z" | "a" | "b" | "c" := CardinalSymbol @ c;
  w_                                  := ToPathWord[w];
];

pathOrWordBoxes = Case[
  s:("P" | "Q" | "R" | "T") := MakeBoxes @ PathSymbol @ s;
  s:symsP                   := MakeBoxes @ PathSymbol @ s;
  s_List                    := wordBoxes @ s;
  c:(colsP[_])              := MakeBoxes @ c;
  p:(_PathSymbol | _VertexFieldSymbol | _EdgeFieldSymbol | _WordForm | _CardinalSymbol) := MakeBoxes @ p,
  {symsP -> $rawSymbolP, colsP -> $colorFormP}
]

(**************************************************************************************************)

PublicForm[VertexFieldSymbol, EdgeFieldSymbol]

declareSymbolForm[VertexFieldSymbol];
declareSymbolForm[EdgeFieldSymbol];

(**************************************************************************************************)

PublicForm[PathVectorSpaceSymbol, PathVectorSymbol, PathWeightSymbol]

PathVectorSpaceSymbol[] := PathVectorSpaceSymbol["P"];

declareSymbolForm[PathVectorSpaceSymbol];
declareSymbolForm[PathVectorSymbol];
declareSymbolForm[PathWeightSymbol];

(**************************************************************************************************)

PublicForm[BasisPathVectorSymbol, BasisPathWeightSymbol]

BasisPathVectorSymbol[sub_] := BasisPathVectorSymbol["p", sub];
BasisPathWeightSymbol[sub_] := BasisPathWeightSymbol["p", sub];

declareBoxFormatting[
  BasisPathVectorSymbol[p_, sub_] :> makeTemplateBox[p, sub, "BasisPathVectorSymbolForm"],
  BasisPathWeightSymbol[p_, sub_] :> makeTemplateBox[p, sub, "BasisPathWeightSymbolForm"]
];

$TemplateKatexFunction["BasisPathVectorSymbolForm"] = "basisPath";
$TemplateKatexFunction["BasisPathWeightSymbolForm"] = "basisPathWeight";

(**************************************************************************************************)

PublicForm[EdgeFieldsSymbol, VertexFieldsSymbol]

declareBoxFormatting[
  EdgeFieldsSymbol[k_] :> MakeBoxes @ FunctionSpaceForm[EdgesSymbol[], k],
  EdgeFieldsSymbol[k_, q_] :> MakeBoxes @ FunctionSpaceForm[EdgesSymbol[q], k],
  VertexFieldsSymbol[k_] :> MakeBoxes @ FunctionSpaceForm[VerticesSymbol[], k],
  VertexFieldsSymbol[k_, q_] :> MakeBoxes @ FunctionSpaceForm[VerticesSymbol[q], k]
];

(**************************************************************************************************)

PublicSymbol[UnitVertexFieldSymbol]

declareConstantSymbol[UnitVertexFieldSymbol];

(**************************************************************************************************)

PublicForm[WordVectorForm]

WordVectorForm[p_String, args___] := WordVectorForm[ToPathWord @ p, args]

declareBoxFormatting[
  WordVectorForm[p_, dir_:"Forward"] :>
    TemplateBox[{wordBoxes @ p, directionBoxes @ dir}, "WordVectorForm"]
];

directionBoxes = Case[
  "Forward"         := SBox["ForwardSymbol"];
  "Backward"        := SBox["BackwardSymbol"];
  "Symmetric"       := SBox["SymmetricSymbol"];
  "Antisymmetric"   := SBox["AntisymmetricSymbol"];
];

$TemplateKatexFunction["WordVectorForm"] = "wordVector";
$TemplateKatexFunction["ForwardSymbol"] = katexAlias["forwardSymbol"];
$TemplateKatexFunction["BackwardSymbol"] = katexAlias["backwardSymbol"];
$TemplateKatexFunction["SymmetricSymbol"] = katexAlias["symmetricSymbol"];
$TemplateKatexFunction["AntisymmetricSymbol"] = katexAlias["antisymmetricSymbol"];

(**************************************************************************************************)

$appliedCompositionP = (_FunctionCompositionForm);

makePathBoxTemplate[left_, rest___, tag_] :=
  TemplateBox[
    Join[
      List @ maybeParen[PathSymbol|RouteSymbol|ParenthesesForm|PathHeadForm|PathTailForm|PathReverseForm|CardinalSymbol|$colorFormP|$functionFormP|$appliedCompositionP|EdgeFieldSymbol|PathVectorSymbol] @ left,
      MapUnevaluated[
        maybeParen[PathSymbol|RouteSymbol|ParenthesesForm|PathHeadForm|PathTailForm|PathReverseForm|CardinalSymbol|EdgeFieldSymbol|VertexFieldSymbol|PathVectorSymbol|PathTranslateForm|PathBackwardTranslateForm|$colorFormP|$appliedCompositionP],
        {rest}
      ]
    ],
    tag
  ];

makePathBoxTemplate[left_, tag_] :=
  TemplateBox[
    List @ maybeParen[PathSymbol|PathReverseForm|ParenthesesForm|EdgeFieldSymbol|VertexFieldSymbol|PathVectorSymbol|PathTranslateForm|PathBackwardTranslateForm|$colorFormP|$appliedCompositionP] @ left,
    tag
  ];

(**************************************************************************************************)

PublicVariable[$PathComposeSymbol]

$PathComposeSymbol = "\[Proportion]";

PublicForm[PathComposeForm]

declareBoxFormatting[
  PathComposeForm[a_, b__] :>
    makePathBoxTemplate[a, b, "PathComposeForm"],

  PathComposeForm[] :>
    SBox["PathComposeSymbol"]
];

$TemplateKatexFunction["PathComposeForm"] = "pathCompose";
$TemplateKatexFunction["PathComposeSymbol"] = "pathComposeSymbol";

(**************************************************************************************************)

PublicForm[PathIntegralForm]

declareBoxFormatting[
  PathIntegralForm[a_, b_] :>
    makePathBoxTemplate[a, b, "PathIntegralForm"],

  PathIntegralForm[] :>
    SBox["PathIntegralSymbol"]
];

$TemplateKatexFunction["PathIntegralForm"] = "pathIntegral";
$TemplateKatexFunction["PathIntegralSymbol"] = "pathIntegralSymbol";

(**************************************************************************************************)

PublicForm[PathDotForm]

declareBoxFormatting[
  PathDotForm[a_, b_] :>
    makePathBoxTemplate[a, b, "PathDotForm"],

  PathDotForm[] :>
    SBox["PathDotSymbol"]
];

$TemplateKatexFunction["PathDotForm"] = "pathDot";
$TemplateKatexFunction["PathDotSymbol"] = "pathDotSymbol";

(**************************************************************************************************)

PublicForm[PathTranslateForm, PathLeftTranslateForm]

declareBoxFormatting[
  PathTranslateForm[] :>
    SBox["PathTranslateSymbol"],
  PathTranslateForm[a_, b__] :>
    makePathBoxTemplate[a, b, "PathTranslateForm"],
  PathTranslateForm[a_] :>
    makePathBoxTemplate[a, "PathAnonymousTranslateForm"],
  PathLeftTranslateForm[a_] :>
    makePathBoxTemplate[a, "PathLeftTranslateForm"]
];

$TemplateKatexFunction["PathTranslateForm"] = "pathTranslate";
$TemplateKatexFunction["PathAnonymousTranslateForm"] = "pathTranslate{}";
$TemplateKatexFunction["PathLeftTranslateForm"] = "pathLeftTranslate";
$TemplateKatexFunction["PathTranslateSymbol"] = katexAlias["translateSymbol"];

(**************************************************************************************************)

PublicForm[PathBackwardTranslateForm, PathLeftBackwardTranslateForm]

declareBoxFormatting[
  PathBackwardTranslateForm[] :>
    SBox["PathBackwardTranslateSymbol"],
  PathBackwardTranslateForm[a_, b__] :>
    makePathBoxTemplate[a, b, "PathBackwardTranslateForm"],
  PathBackwardTranslateForm[a_] :>
    makePathBoxTemplate[a, "PathAnonymousBackwardTranslateForm"],
  PathLeftBackwardTranslateForm[a_] :>
    makePathBoxTemplate[a, "PathLeftBackwardTranslateForm"]
];

$TemplateKatexFunction["PathBackwardTranslateForm"] = "pathBackwardTranslate";
$TemplateKatexFunction["PathAnonymousBackwardTranslateForm"] = "pathBackwardTranslate{}";
$TemplateKatexFunction["PathLeftBackwardTranslateForm"] = "pathLeftBackwardTranslate";
$TemplateKatexFunction["PathBackwardTranslateSymbol"] = katexAlias["backwardTranslateSymbol"];

(**************************************************************************************************)

PublicForm[PathHeadVectorForm]

declareBoxFormatting[
  PathHeadVectorForm[a_] :>
    makePathBoxTemplate[a, "PathHeadVectorForm"]
];

$TemplateKatexFunction["PathHeadVectorForm"] = "pathHeadVector";

(**************************************************************************************************)

PublicForm[PathTailVectorForm]

declareBoxFormatting[
  PathTailVectorForm[a_] :>
    makePathBoxTemplate[a, "PathTailVectorForm"]
];

$TemplateKatexFunction["PathTailVectorForm"] = "pathTailVector";

(**************************************************************************************************)

PublicForm[PathTailVertexForm, PathHeadVertexForm, PathTailForm, PathHeadForm]

declareUnaryForm[PathTailVertexForm, PathSymbol];
declareUnaryForm[PathHeadVertexForm, PathSymbol];
declareUnaryForm[PathTailForm, PathSymbol];
declareUnaryForm[PathHeadForm, PathSymbol];

(**************************************************************************************************)

PublicForm[PathSplitForm]

declareBoxFormatting[
  PathSplitForm[a_] :>
    MakeBoxes @ AppliedForm[SplitFunction, a]
];

(**************************************************************************************************)

PublicForm[PathReverseForm]

declareBoxFormatting[
  PathReverseForm[a_] :>
    makePathBoxTemplate[a, "PathReverseForm"],
  PathReverseForm[] :>
    SBox["PathReverseSymbol"]
];

$TemplateKatexFunction["PathReverseForm"] = "pathReverse";
$TemplateKatexFunction["PathReverseSymbol"] = "pathReverse";

(**************************************************************************************************)

PublicForm[CompactBasisSymbolForm]

declareSymbolForm[CompactBasisSymbolForm, PathVectorSpaceSymbol];

(**************************************************************************************************)

PublicForm[CompactPathCovariantDifferenceForm, PathCovariantDifferenceForm]

declareBinaryForm[CompactPathCovariantDifferenceForm, PathVectorSymbol];
declareBinaryForm[PathCovariantDifferenceForm, PathVectorSymbol];
(*
declareBoxFormatting[
  CompactPathCovariantDifferenceForm[a_, b_] :>

    TemplateBox[MapUnevaluated[pathOrWordBoxes, {a, b}], "CompactPathCovariantDifferenceForm"],
  PathCovariantDifferenceForm[a_, b_] :>
    TemplateBox[MapUnevaluated[pathOrWordBoxes, {a, b}], "PathCovariantDifferenceForm"]
];

$TemplateKatexFunction["CompactPathCovariantDifferenceForm"] = "compactCovariantDifference";
$TemplateKatexFunction["PathCovariantDifferenceForm"] = "covariantDifference";
 *)
(**************************************************************************************************)

PublicForm[PathForwardDifferenceForm]

PathForwardDifferenceForm[w_String] :=
  PathForwardDifferenceForm @ toPathWordOrSymbol @ w;

declareBoxFormatting[
  PathForwardDifferenceForm[w__] :>
    TemplateBox[Map[pathOrWordBoxes, {w}], "PathForwardDifferenceForm"],
  PathForwardDifferenceForm[] :>
    SBox["PathForwardDifferenceSymbol"],
  (op_PathForwardDifferenceForm)[arg_] :>
    MakeBoxes @ OperatorAppliedForm[op, arg]
];

$TemplateKatexFunction["PathForwardDifferenceForm"] = applyRiffled["pathForwardDifference",","];
$TemplateKatexFunction["PathForwardDifferenceSymbol"] = "forwardDifference";

(**************************************************************************************************)

PublicForm[PathBackwardDifferenceForm]

PathBackwardDifferenceForm[w_String] :=
  PathBackwardDifferenceForm @ toPathWordOrSymbol @ w;

declareBoxFormatting[
  PathBackwardDifferenceForm[w__] :>
    TemplateBox[Map[pathOrWordBoxes, {w}], "PathBackwardDifferenceForm"],
  PathBackwardDifferenceForm[] :>
    SBox["PathBackwardDifferenceSymbol"],
  (op_PathBackwardDifferenceForm)[arg_] :>
    MakeBoxes @ OperatorAppliedForm[op, arg]
];

$TemplateKatexFunction["PathBackwardDifferenceForm"] = applyRiffled["pathBackwardDifference",","];
$TemplateKatexFunction["PathBackwardDifferenceSymbol"] = "backwardDifference";

(**************************************************************************************************)

PublicForm[PathCentralDifferenceForm]

PathCentralDifferenceForm[w_String] :=
  PathCentralDifferenceForm @ toPathWordOrSymbol @ w;

declareBoxFormatting[
  PathCentralDifferenceForm[w__] :>
    TemplateBox[Map[pathOrWordBoxes, {w}], "PathCentralDifferenceForm"],
  PathCentralDifferenceForm[] :>
    SBox["PathCentralDifferenceSymbol"],
  (op_PathCentralDifferenceForm)[arg_] :>
    MakeBoxes @ OperatorAppliedForm[op, arg]
];

$TemplateKatexFunction["PathCentralDifferenceForm"] = applyRiffled["pathCentralDifference",","];
$TemplateKatexFunction["PathCentralDifferenceSymbol"] = "centralDifference";

