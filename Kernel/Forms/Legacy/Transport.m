PublicTypesettingForm[CardinalRewriteForm]

DefineBinaryForm[CardinalRewriteForm, "?"]

(**************************************************************************************************)

PublicTypesettingForm[ChartSymbolForm]

DefineTaggedForm[ChartSymbolForm]

(**************************************************************************************************)

PublicFunction[TransportMapSymbol]

DefineUnaryForm[TransportMapSymbol, "?"]

(**************************************************************************************************)

(* how is this different from wordgroup? *)
PublicTypesettingForm[CardinalGroupSymbolForm]

DefineTaggedForm[CardinalGroupSymbolForm, SymbolForm];

(**************************************************************************************************)

(* this doesn't have any katex, and what is it for? shouldn't it be a word group? *)
(* PublicTypesettingForm[CardinalGroupoidSymbolForm]

DefineTaggedForm[CardinalGroupoidSymbolForm, SymbolForm];
 *)
(**************************************************************************************************)

PublicTypesettingForm[TransportAtlasSymbolForm]

DefineUnaryForm[TransportAtlasSymbolForm, "?"]
