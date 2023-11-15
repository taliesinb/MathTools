PublicTypesettingForm[TopologicalSpaceSymbolForm, BundleSectionSymbolForm, BundleProjectionSymbolForm]

DefineTaggedForm[TopologicalSpaceSymbolForm]
DefineTaggedForm[BundleSectionSymbolForm]
DefineTaggedForm[BundleProjectionSymbolForm]

(**************************************************************************************************)

PublicTypesettingForm[BundleFunctionStyleForm, BundleGraphStyleForm, BundleProjectionStyleForm, BundleSectionStyleForm]

f_BundleFunctionStyleForm[arg__] := AppliedForm[f, arg];
f_BundleGraphStyleForm[arg__] := AppliedForm[f, arg];
f_BundleProjectionStyleForm[arg__] := AppliedForm[f, arg];
f_BundleSectionStyleForm[arg__] := AppliedForm[f, arg];

DefineUnaryForm[BundleFunctionStyleForm, "?"]
DefineUnaryForm[BundleGraphStyleForm, "?"]
DefineUnaryForm[BundleProjectionStyleForm, "?"]
DefineUnaryForm[BundleSectionStyleForm, "?"]

PublicTypesettingForm[TotalSpaceStyleForm, BaseSpaceStyleForm, FiberSpaceStyleForm]

DefineUnaryForm[TotalSpaceStyleForm, "?"]
DefineUnaryForm[BaseSpaceStyleForm, "?"]
DefineUnaryForm[FiberSpaceStyleForm, "?"]

PublicTypesettingForm[BaseSpaceElementStyleForm, FiberSpaceElementStyleForm, TotalSpaceElementStyleForm]

DefineUnaryForm[BaseSpaceElementStyleForm, "?"]
DefineUnaryForm[FiberSpaceElementStyleForm, "?"]
DefineUnaryForm[TotalSpaceElementStyleForm, "?"]

(**************************************************************************************************)

PublicTypesettingForm[TopologicalQuotientSpaceForm]

DefineBinaryForm[TopologicalQuotientSpaceForm, "?"]

(**************************************************************************************************)

PublicTypesettingForm[CircleSpaceForm]

DefineUnaryForm[CircleSpaceForm, "?"];

declareBoxFormatting[
  CircleSpaceForm[] :> SBox["CircleSpaceSymbol"]
]
