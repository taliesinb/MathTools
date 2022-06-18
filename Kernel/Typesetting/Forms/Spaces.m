PublicForm[TopologicalSpaceSymbolForm, BundleSectionSymbolForm, BundleProjectionSymbolForm]

declareSymbolForm[TopologicalSpaceSymbolForm]
declareSymbolForm[BundleSectionSymbolForm]
declareSymbolForm[BundleProjectionSymbolForm]

(**************************************************************************************************)

PublicForm[BundleFunctionStyleForm, BundleGraphStyleForm, BundleProjectionStyleForm, BundleSectionStyleForm]

f_BundleFunctionStyleForm[arg__] := AppliedForm[f, arg];
f_BundleGraphStyleForm[arg__] := AppliedForm[f, arg];
f_BundleProjectionStyleForm[arg__] := AppliedForm[f, arg];
f_BundleSectionStyleForm[arg__] := AppliedForm[f, arg];

declareUnaryForm[BundleFunctionStyleForm]
declareUnaryForm[BundleGraphStyleForm]
declareUnaryForm[BundleProjectionStyleForm]
declareUnaryForm[BundleSectionStyleForm]

PublicForm[TotalSpaceStyleForm, BaseSpaceStyleForm, FiberSpaceStyleForm]

declareUnaryForm[TotalSpaceStyleForm]
declareUnaryForm[BaseSpaceStyleForm]
declareUnaryForm[FiberSpaceStyleForm]

PublicForm[BaseSpaceElementStyleForm, FiberSpaceElementStyleForm, TotalSpaceElementStyleForm]

declareUnaryForm[BaseSpaceElementStyleForm]
declareUnaryForm[FiberSpaceElementStyleForm]
declareUnaryForm[TotalSpaceElementStyleForm]

(**************************************************************************************************)

PublicForm[TopologicalQuotientSpaceForm]

declareBinaryForm[TopologicalQuotientSpaceForm]

(**************************************************************************************************)

PublicForm[CircleSpaceForm]

declareUnaryForm[CircleSpaceForm];

declareBoxFormatting[
  CircleSpaceForm[] :> SBox["CircleSpaceSymbol"]
]

(**************************************************************************************************)

PublicForm[RealVectorSpaceForm, ComplexVectorSpaceForm]

declareUnaryForm[RealVectorSpaceForm]
declareUnaryForm[ComplexVectorSpaceForm]
