PublicTypesettingForm[RouteSymbol, MultirouteSymbol, PlanSymbol, MultiwordSymbol]

DefineTaggedForm[RouteSymbol];
DefineTaggedForm[MultirouteSymbol];
DefineTaggedForm[PlanSymbol];
DefineTaggedForm[MultiwordSymbol];

(**************************************************************************************************)

PublicTypesettingForm[RouteForm, MultirouteForm]

RouteForm[a_, b_Str, c_] := RouteForm[a, ToPathWord @ b, c];

DefineTernaryForm[RouteForm, "?"]
DefineTernaryForm[MultirouteForm, "?"]

(**************************************************************************************************)

PublicTypesettingForm[PlanRingSymbol]

DefineTaggedForm[PlanRingSymbol]
DefineTaggedForm[QuiverSymbol]

