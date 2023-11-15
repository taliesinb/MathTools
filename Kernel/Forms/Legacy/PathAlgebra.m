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

PublicTypesettingForm[VertexFieldSymbol, EdgeFieldSymbol]

DefineTaggedForm[VertexFieldSymbol];
DefineTaggedForm[EdgeFieldSymbol];

(**************************************************************************************************)

PublicTypesettingForm[PathVectorSpaceSymbol, PathVectorSymbol, PathWeightSymbol]

PathVectorSpaceSymbol[] := PathVectorSpaceSymbol["P"];

DefineTaggedForm[PathVectorSpaceSymbol];
DefineTaggedForm[PathVectorSymbol];
DefineTaggedForm[PathWeightSymbol];

(**************************************************************************************************)

PublicTypesettingForm[BasisPathVectorSymbol, BasisPathWeightSymbol]

BasisPathVectorSymbol[sub_] := BasisPathVectorSymbol["p", sub];
BasisPathWeightSymbol[sub_] := BasisPathWeightSymbol["p", sub];

declareBoxFormatting[
  BasisPathVectorSymbol[p_, sub_] :> makeTemplateBox[p, sub, "BasisPathVectorSymbolForm"],
  BasisPathWeightSymbol[p_, sub_] :> makeTemplateBox[p, sub, "BasisPathWeightSymbolForm"]
];

$TemplateKatexFunction["BasisPathVectorSymbolForm"] = "basisPath";
$TemplateKatexFunction["BasisPathWeightSymbolForm"] = "basisPathWeight";

(**************************************************************************************************)

PublicTypesettingForm[EdgeFieldsSymbol, VertexFieldsSymbol]

declareBoxFormatting[
  EdgeFieldsSymbol[k_] :> MakeBoxes @ FunctionSpaceForm[EdgesSymbol[], k],
  EdgeFieldsSymbol[k_, q_] :> MakeBoxes @ FunctionSpaceForm[EdgesSymbol[q], k],
  VertexFieldsSymbol[k_] :> MakeBoxes @ FunctionSpaceForm[VerticesSymbol[], k],
  VertexFieldsSymbol[k_, q_] :> MakeBoxes @ FunctionSpaceForm[VerticesSymbol[q], k]
];

(**************************************************************************************************)

PublicSymbol[UnitVertexFieldSymbol]

DefineSymbolForm[UnitVertexFieldSymbol -> "1"];

(**************************************************************************************************)

PublicTypesettingForm[WordVectorForm]

WordVectorForm[p_Str, args___] := WordVectorForm[ToPathWord @ p, args]

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

PublicTypesettingForm[PathComposeForm]

declareBoxFormatting[
  PathComposeForm[a_, b__] :>
    makePathBoxTemplate[a, b, "PathComposeForm"],

  PathComposeForm[] :>
    SBox["PathComposeSymbol"]
];

$TemplateKatexFunction["PathComposeForm"] = "pathCompose";
$TemplateKatexFunction["PathComposeSymbol"] = "pathComposeSymbol";

(**************************************************************************************************)

PublicTypesettingForm[PathIntegralForm]

declareBoxFormatting[
  PathIntegralForm[a_, b_] :>
    makePathBoxTemplate[a, b, "PathIntegralForm"],

  PathIntegralForm[] :>
    SBox["PathIntegralSymbol"]
];

$TemplateKatexFunction["PathIntegralForm"] = "pathIntegral";
$TemplateKatexFunction["PathIntegralSymbol"] = "pathIntegralSymbol";

(**************************************************************************************************)

PublicTypesettingForm[PathDotForm]

declareBoxFormatting[
  PathDotForm[a_, b_] :>
    makePathBoxTemplate[a, b, "PathDotForm"],

  PathDotForm[] :>
    SBox["PathDotSymbol"]
];

$TemplateKatexFunction["PathDotForm"] = "pathDot";
$TemplateKatexFunction["PathDotSymbol"] = "pathDotSymbol";

(**************************************************************************************************)

PublicTypesettingForm[PathTranslateForm, PathLeftTranslateForm]

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

PublicTypesettingForm[PathBackwardTranslateForm, PathLeftBackwardTranslateForm]

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

PublicTypesettingForm[PathHeadVectorForm]

declareBoxFormatting[
  PathHeadVectorForm[a_] :>
    makePathBoxTemplate[a, "PathHeadVectorForm"]
];

$TemplateKatexFunction["PathHeadVectorForm"] = "pathHeadVector";

(**************************************************************************************************)

PublicTypesettingForm[PathTailVectorForm]

declareBoxFormatting[
  PathTailVectorForm[a_] :>
    makePathBoxTemplate[a, "PathTailVectorForm"]
];

$TemplateKatexFunction["PathTailVectorForm"] = "pathTailVector";

(**************************************************************************************************)

PublicTypesettingForm[PathTailVertexForm, PathHeadVertexForm, PathTailForm, PathHeadForm]

DefineUnaryForm[PathTailVertexForm, "?"];
DefineUnaryForm[PathHeadVertexForm, "?"];
DefineUnaryForm[PathTailForm, "?"];
DefineUnaryForm[PathHeadForm, "?"];

(**************************************************************************************************)

PublicTypesettingForm[PathSplitForm]

declareBoxFormatting[
  PathSplitForm[a_] :>
    MakeBoxes @ AppliedForm[SplitFunction, a]
];

(**************************************************************************************************)

PublicTypesettingForm[PathReverseForm]

declareBoxFormatting[
  PathReverseForm[a_] :>
    makePathBoxTemplate[a, "PathReverseForm"],
  PathReverseForm[] :>
    SBox["PathReverseSymbol"]
];

$TemplateKatexFunction["PathReverseForm"] = "pathReverse";
$TemplateKatexFunction["PathReverseSymbol"] = "pathReverse";

(**************************************************************************************************)

PublicTypesettingForm[CompactBasisSymbolForm]

DefineTaggedForm[CompactBasisSymbolForm];

(**************************************************************************************************)

PublicTypesettingForm[CompactPathCovariantDifferenceForm, PathCovariantDifferenceForm]

DefineBinaryForm[CompactPathCovariantDifferenceForm, "?"];
DefineBinaryForm[PathCovariantDifferenceForm, "?"];
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

PublicTypesettingForm[PathForwardDifferenceForm]

PathForwardDifferenceForm[w_Str] :=
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

PublicTypesettingForm[PathBackwardDifferenceForm]

PathBackwardDifferenceForm[w_Str] :=
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

PublicTypesettingForm[PathCentralDifferenceForm]

PathCentralDifferenceForm[w_Str] :=
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

