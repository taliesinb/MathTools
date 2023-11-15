(* TODO:
this combination of LocalStateSymbolForm, LocalState, and their corresponding katex matching things
should be factorized
*)

PublicTypesettingForm[GlobalStateSymbol, RegionalStateSymbol, LocalStateSymbol]

DefineTaggedForm[LocalStateSymbol];
DefineTaggedForm[RegionalStateSymbol];
DefineTaggedForm[GlobalStateSymbol];

(**************************************************************************************************)

PublicTypesettingForm[KeySubStateSymbol, ValueSubStateSymbol]

DefineTaggedForm[KeySubStateSymbol];
DefineTaggedForm[ValueSubStateSymbol];

(**************************************************************************************************)

PublicTypesettingForm[RegionalSubstateForm, RegionalSuperstateForm, IncomparableRegionalStatesForm, ComparableRegionalStatesForm]

DefineInfixForm[RegionalSubstateForm, "?"];
DefineInfixForm[RegionalSuperstateForm, "?"];
DefineInfixForm[IncomparableRegionalStatesForm, "?"];
DefineInfixForm[ComparableRegionalStatesForm, "?"];

(**************************************************************************************************)

PublicTypesettingForm[LHSStateForm, RHSStateForm]

DefineUnaryForm[LHSStateForm, "?"];
DefineUnaryForm[RHSStateForm, "?"];

(**************************************************************************************************)

PublicTypesettingForm[RewriteLHSRegionalStateForm, RewriteRHSRegionalStateForm]

DefineUnaryForm[RewriteLHSRegionalStateForm, "?"]
DefineUnaryForm[RewriteRHSRegionalStateForm, "?"]

(**************************************************************************************************)

PublicTypesettingForm[LocalStateForm, RegionalStateForm]

DefineBinaryForm[LocalStateForm, "?"]
DefineNAryForm[RegionalStateForm, "?"]
DefineNAryForm[EmptyRegionalState, "?"];

PublicTypesettingForm[StringRegionalStateForm]

StringRegionalStateForm[str_, {i_, j_}] /; j < i :=
  makeStrRegState[str, Join[Range[i, i + StringLength[str] - j - 1], Range[1, j]]];

StringRegionalStateForm[str_, {i_, j_}] :=
  makeStrRegState[str, Range[i, j]];

makeStrRegState[chars_, indices_] := makeRegState[LiteralCharacterForm /@ Characters @ chars, indices];
makeRegState[states_, keys_] := RegionalStateForm @@ MapThread[LocalStateForm, {keys, states}];

(**************************************************************************************************)

PublicSymbol[InvalidRegionalState, EmptyRegionalState]

DefineSymbolForm[{InvalidRegionalState -> "?", EmptyRegionalState -> "?"}];

(**************************************************************************************************)

PublicTypesettingForm[LocalStatesForm, GlobalStatesForm, RegionalStatesForm]

DefineUnaryForm[LocalStatesForm, "?"];
DefineUnaryForm[GlobalStatesForm, "?"];
DefineUnaryForm[RegionalStatesForm, "?"];

PublicTypesettingForm[KeySubStatesForm, ValueSubStatesForm]

DefineUnaryForm[KeySubStatesForm, "?"];
DefineUnaryForm[ValueSubStatesForm, "?"];

(**************************************************************************************************)

PublicTypesettingForm[InfixStateComposeForm, InfixStateMeetForm, InfixStateJoinForm]

DefineInfixForm[InfixStateComposeForm, "?"];
DefineInfixForm[InfixStateMeetForm, "?"];
DefineInfixForm[InfixStateJoinForm, "?"];

(**************************************************************************************************)
