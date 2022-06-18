(* TODO:
this combination of LocalStateSymbolForm, LocalState, and their corresponding katex matching things
should be factorized
*)

PublicForm[GlobalStateSymbol, RegionalStateSymbol, LocalStateSymbol]

declareSymbolFormExplicit[LocalStateSymbol];
declareSymbolFormExplicit[RegionalStateSymbol];
declareSymbolFormExplicit[GlobalStateSymbol];

PublicForm[KeySubStateSymbol, ValueSubStateSymbol]
declareSymbolFormExplicit[KeySubStateSymbol];
declareSymbolFormExplicit[ValueSubStateSymbol];

(**************************************************************************************************)

PublicForm[RegionalSubstateForm, RegionalSuperstateForm, IncomparableRegionalStatesForm, ComparableRegionalStatesForm]

declareInfixSymbol[RegionalSubstateForm];
declareInfixSymbol[RegionalSuperstateForm];
declareInfixSymbol[IncomparableRegionalStatesForm];
declareInfixSymbol[ComparableRegionalStatesForm];

(**************************************************************************************************)

PublicForm[LHSStateForm, RHSStateForm]

declareUnaryForm[LHSStateForm] // usingCustomKatex["lhsState"];
declareUnaryForm[RHSStateForm] // usingCustomKatex["rhsState"];

(**************************************************************************************************)

PublicForm[RewriteLHSRegionalStateForm, RewriteRHSRegionalStateForm]

declareUnaryForm[RewriteLHSRegionalStateForm]
declareUnaryForm[RewriteRHSRegionalStateForm]

(**************************************************************************************************)

PublicForm[LocalStateForm, RegionalStateForm]

declareBinaryForm[LocalStateForm]
declareNAryForm[RegionalStateForm, EmptyRegionalState];

PublicForm[StringRegionalStateForm]

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

PublicForm[LocalStatesForm, GlobalStatesForm, RegionalStatesForm]

declareUnaryForm[LocalStatesForm, RewritingSystemSymbol];
declareUnaryForm[GlobalStatesForm, RewritingSystemSymbol];
declareUnaryForm[RegionalStatesForm, RewritingSystemSymbol];

PublicForm[KeySubStatesForm, ValueSubStatesForm]

declareUnaryForm[KeySubStatesForm, RewritingSystemSymbol];
declareUnaryForm[ValueSubStatesForm, RewritingSystemSymbol];

(**************************************************************************************************)

PublicForm[InfixStateComposeForm, InfixStateMeetForm, InfixStateJoinForm]

declareInfixSymbol[InfixStateComposeForm];
declareInfixSymbol[InfixStateMeetForm];
declareInfixSymbol[InfixStateJoinForm];

(**************************************************************************************************)
