PublicForm[LatticeSymbol, MeetSemilatticeSymbol, JoinSemilatticeSymbol, PosetSymbol]

DefineTaggedForm[{LatticeSymbol, MeetSemilatticeSymbol, JoinSemilatticeSymbol, PosetSymbol}]

PublicForm[LatticeElementSymbol, MeetSemilatticeElementSymbol, JoinSemilatticeElementSymbol, PosetElementSymbol]

DefineTaggedForm[{LatticeElementSymbol, MeetSemilatticeElementSymbol, JoinSemilatticeElementSymbol, PosetElementSymbol}]

(**************************************************************************************************)

PublicSymbol[LatticeTopSymbol, LatticeBottomSymbol, SemilatticeTopSymbol, SemilatticeBottomSymbol]

DefineSymbolForm[{
  LatticeTopSymbol        -> "\[DownTee]",
  SemilatticeTopSymbol    -> "\[DownTee]",
  LatticeBottomSymbol     -> "\[UpTee]",
  SemilatticeBottomSymbol -> "\[UpTee]"
}];

(**************************************************************************************************)

PublicForm[LatticeMeetForm, SemilatticeMeetForm, SemilatticeSemimeetForm]

DefineInfixForm[#, KBox[WideOpBox @ ForceKatexCharBox["\[And]"], "\[Wedge]"]]& /@
  {LatticeMeetForm, SemilatticeMeetForm, SemilatticeSemimeetForm}

PublicForm[LatticeJoinForm, SemilatticeJoinForm, SemilatticeSemijoinForm]

DefineInfixForm[#, KBox[WideOpBox @ ForceKatexCharBox @ "\[Or]", "\[Vee]"]]& /@
  {LatticeJoinForm, SemilatticeJoinForm, SemilatticeSemijoinForm};

(**************************************************************************************************)

PublicForm[PosetGreaterForm, PosetGreaterEqualForm, PosetLessForm, PosetLessEqualForm, PosetCoversForm, PosetCoveredByForm]

DefineInfixForm[#1, OpBox @ #2]& @@@ ExpressionTable[
  PosetGreaterForm        ">"
  PosetGreaterEqualForm   "≥"
  PosetLessForm           "<"
  PosetLessEqualForm      "≤"
  PosetCoversForm         "⋗"
  PosetCoveredByForm      "⋖"
]

(**************************************************************************************************)

PublicForm[LatticeGreaterForm, LatticeGreaterEqualForm, LatticeLessForm, LatticeLessEqualForm]

DefineInfixForm[#1, OpBox @ #2]& @@@ ExpressionTable[
  LatticeGreaterForm        ">"
  LatticeGreaterEqualForm   "≥"
  LatticeLessForm           "<"
  LatticeLessEqualForm      "≤"
]
