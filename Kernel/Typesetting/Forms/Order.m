PublicForm[LatticeSymbol, MeetSemilatticeSymbol, JoinSemilatticeSymbol, PosetSymbol]

declareSymbolFormExplicit[LatticeSymbol]
declareSymbolFormExplicit[MeetSemilatticeSymbol]
declareSymbolFormExplicit[JoinSemilatticeSymbol]
declareSymbolFormExplicit[PosetSymbol]

(**************************************************************************************************)

PublicSymbol[LatticeTopSymbol, LatticeBottomSymbol, SemilatticeTopSymbol, SemilatticeBottomSymbol]

declareConstantSymbol[{LatticeTopSymbol, LatticeBottomSymbol, SemilatticeTopSymbol, SemilatticeBottomSymbol}];

(**************************************************************************************************)

PublicForm[LatticeElementSymbol, MeetSemilatticeElementSymbol, JoinSemilatticeElementSymbol, PosetElementSymbol]

declareSymbolFormExplicit[LatticeElementSymbol];
declareSymbolFormExplicit[MeetSemilatticeElementSymbol];
declareSymbolFormExplicit[JoinSemilatticeElementSymbol];
declareSymbolFormExplicit[PosetElementSymbol];

(**************************************************************************************************)

PublicForm[LatticeMeetForm, LatticeJoinForm]

declareInfixSymbol[{LatticeMeetForm, LatticeJoinForm}, LatticeElementSymbol];

(**************************************************************************************************)

PublicForm[SemilatticeMeetForm, SemilatticeJoinForm]

declareInfixSymbol[SemilatticeMeetForm, MeetSemilatticeElementSymbol];
declareInfixSymbol[SemilatticeJoinForm, JoinSemilatticeElementSymbol];

(**************************************************************************************************)

PublicForm[SemilatticeSemimeetForm, SemilatticeSemijoinForm]

declareInfixSymbol[SemilatticeSemimeetForm, JoinSemilatticeElementSymbol];
declareInfixSymbol[SemilatticeSemijoinForm, MeetSemilatticeElementSymbol];

(**************************************************************************************************)

PublicForm[PosetGreaterForm, PosetGreaterEqualForm, PosetLessForm, PosetLessEqualForm, PosetCoversForm, PosetCoveredByForm]

declareInfixSymbol[PosetGreaterForm, PosetElementSymbol];
declareInfixSymbol[PosetGreaterEqualForm, PosetElementSymbol];
declareInfixSymbol[PosetLessForm, PosetElementSymbol];
declareInfixSymbol[PosetLessEqualForm, PosetElementSymbol];
declareInfixSymbol[PosetCoversForm, PosetElementSymbol];
declareInfixSymbol[PosetCoveredByForm, PosetElementSymbol];

(**************************************************************************************************)

PublicForm[LatticeGreaterForm, LatticeGreaterEqualForm, LatticeLessForm, LatticeLessEqualForm]

declareInfixSymbol[LatticeGreaterForm, LatticeElementSymbol];
declareInfixSymbol[LatticeGreaterEqualForm, LatticeElementSymbol];
declareInfixSymbol[LatticeLessForm, LatticeElementSymbol];
declareInfixSymbol[LatticeLessEqualForm, LatticeElementSymbol];
