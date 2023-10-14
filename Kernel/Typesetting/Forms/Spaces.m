PublicTypesettingForm[TopologicalSpaceSymbolForm, BundleSectionSymbolForm, BundleProjectionSymbolForm]

declareSymbolForm[TopologicalSpaceSymbolForm]
declareSymbolForm[BundleSectionSymbolForm]
declareSymbolForm[BundleProjectionSymbolForm]

(**************************************************************************************************)

PublicTypesettingForm[BundleFunctionStyleForm, BundleGraphStyleForm, BundleProjectionStyleForm, BundleSectionStyleForm]

f_BundleFunctionStyleForm[arg__] := AppliedForm[f, arg];
f_BundleGraphStyleForm[arg__] := AppliedForm[f, arg];
f_BundleProjectionStyleForm[arg__] := AppliedForm[f, arg];
f_BundleSectionStyleForm[arg__] := AppliedForm[f, arg];

declareUnaryForm[BundleFunctionStyleForm]
declareUnaryForm[BundleGraphStyleForm]
declareUnaryForm[BundleProjectionStyleForm]
declareUnaryForm[BundleSectionStyleForm]

PublicTypesettingForm[TotalSpaceStyleForm, BaseSpaceStyleForm, FiberSpaceStyleForm]

declareUnaryForm[TotalSpaceStyleForm]
declareUnaryForm[BaseSpaceStyleForm]
declareUnaryForm[FiberSpaceStyleForm]

PublicTypesettingForm[BaseSpaceElementStyleForm, FiberSpaceElementStyleForm, TotalSpaceElementStyleForm]

declareUnaryForm[BaseSpaceElementStyleForm]
declareUnaryForm[FiberSpaceElementStyleForm]
declareUnaryForm[TotalSpaceElementStyleForm]

(**************************************************************************************************)

PublicTypesettingForm[TopologicalQuotientSpaceForm]

declareBinaryForm[TopologicalQuotientSpaceForm]

(**************************************************************************************************)

PublicTypesettingForm[CircleSpaceForm]

declareUnaryForm[CircleSpaceForm];

declareBoxFormatting[
  CircleSpaceForm[] :> SBox["CircleSpaceSymbol"]
]

(**************************************************************************************************)

PublicTypesettingForm[RealVectorSpaceForm, ComplexVectorSpaceForm]

declareUnaryForm[RealVectorSpaceForm]
declareUnaryForm[ComplexVectorSpaceForm]
