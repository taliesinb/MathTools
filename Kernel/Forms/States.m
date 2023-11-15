(* TODO:
this combination of LocalStateSymbolForm, LocalState, and their corresponding katex matching things
should be factorized
*)

PublicTypesettingForm[GlobalStateSymbol, RegionalStateSymbol, LocalStateSymbol]

declareSymbolFormExplicit[LocalStateSymbol];
declareSymbolFormExplicit[RegionalStateSymbol];
declareSymbolFormExplicit[GlobalStateSymbol];

PublicTypesettingForm[KeySubStateSymbol, ValueSubStateSymbol]
declareSymbolFormExplicit[KeySubStateSymbol];
declareSymbolFormExplicit[ValueSubStateSymbol];

(**************************************************************************************************)

PublicTypesettingForm[RegionalSubstateForm, RegionalSuperstateForm, IncomparableRegionalStatesForm, ComparableRegionalStatesForm]

declareInfixSymbol[RegionalSubstateForm];
declareInfixSymbol[RegionalSuperstateForm];
declareInfixSymbol[IncomparableRegionalStatesForm];
declareInfixSymbol[ComparableRegionalStatesForm];

(**************************************************************************************************)

PublicTypesettingForm[LHSStateForm, RHSStateForm]

declareUnaryForm[LHSStateForm] // usingCustomKatex["lhsState"];
declareUnaryForm[RHSStateForm] // usingCustomKatex["rhsState"];

(**************************************************************************************************)

PublicTypesettingForm[RewriteLHSRegionalStateForm, RewriteRHSRegionalStateForm]

declareUnaryForm[RewriteLHSRegionalStateForm]
declareUnaryForm[RewriteRHSRegionalStateForm]

(**************************************************************************************************)

PublicTypesettingForm[LocalStateForm, RegionalStateForm]

declareBinaryForm[LocalStateForm]
declareNAryForm[RegionalStateForm, EmptyRegionalState];

PublicTypesettingForm[StringRegionalStateForm]

StringRegionalStateForm[str_, {i_, j_}] /; j < i :=
  makeStrRegState[str, Join[Range[i, i + StringLength[str] - j - 1], Range[1, j]]];

StringRegionalStateForm[str_, {i_, j_}] :=
  makeStrRegState[str, Range[i, j]];

makeStrRegState[chars_, indices_] := makeRegState[LiteralCharacterForm /@ Characters @ chars, indices];
makeRegState[states_, keys_] := RegionalStateForm @@ MapThread[LocalStateForm, {keys, states}];

(**************************************************************************************************)

PublicSymbol[InvalidRegionalState, EmptyRegionalState]

declareConstantSymbol[{InvalidRegionalState, EmptyRegionalState}];

(**************************************************************************************************)

PublicTypesettingForm[LocalStatesForm, GlobalStatesForm, RegionalStatesForm]

declareUnaryForm[LocalStatesForm, RewritingSystemSymbol];
declareUnaryForm[GlobalStatesForm, RewritingSystemSymbol];
declareUnaryForm[RegionalStatesForm, RewritingSystemSymbol];

PublicTypesettingForm[KeySubStatesForm, ValueSubStatesForm]

declareUnaryForm[KeySubStatesForm, RewritingSystemSymbol];
declareUnaryForm[ValueSubStatesForm, RewritingSystemSymbol];

(**************************************************************************************************)

PublicTypesettingForm[InfixStateComposeForm, InfixStateMeetForm, InfixStateJoinForm]

declareInfixSymbol[InfixStateComposeForm];
declareInfixSymbol[InfixStateMeetForm];
declareInfixSymbol[InfixStateJoinForm];

(**************************************************************************************************)
