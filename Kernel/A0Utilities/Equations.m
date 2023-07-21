PublicFunction[SolveCyclicEquations]

PublicOption[AllowPartialSolutions, EquationVariables, ExpandLinearEquations]

PrivateHead[NoLinearExpand]

SetUsage @ "
SolveCyclicEquations[rules$] solves a set of equations that are expressed as rules, each of the form var$ -> expr$.
* The var$ can be any expression, anywhere it appears in another expr$ will be substituted.
* There should be at least one rule of the form var$ -> pure$, where pure$ does not mention any other variables.
* Equations are executed as they become eligible -- when their RHS consists only of solved variables.
* There can be more than one equation that defines a variable. Only one will be used to compute it, the others will be ignored, rathe than verified.
"

Options[SolveCyclicEquations] = {
  VerifySolutions -> True,
  AllowPartialSolutions -> False,
  EquationVariables -> Automatic,
  ExpandLinearEquations -> True,
  Verbose -> False
};

SolveCyclicEquations::badsol = "The following variables have inconsistent values: ``. More info printed below for each value.";
SolveCyclicEquations::partsol = "Solution is partial. Have `` solved variables out of ``, unsolved variables: ``.";

SolveCyclicEquations[eqns:{___Rule}, OptionsPattern[]] := CatchMessage @ Scope[
  UnpackOptions[verifySolutions, allowPartialSolutions, equationVariables, expandLinearEquations, $verbose];
  lhss = Keys @ eqns;
  If[equationVariables === Automatic,
    vars = Union @ lhss,
    vars = Union[lhss, equationVariables]
  ];
  varP = Alternatives @@ vars;
  If[expandLinearEquations, eqns = ReplaceAll[NoLinearExpand[t_] :> t] @ Flatten @ Map[toSubEquations, eqns]];
  varI = AssociationRange @ vars;
  eqns = DeleteDuplicates @ Flatten @ VectorReplace[eqns, rule:(_ -> varP) :> {rule, Reverse @ rule}];
  (* printEqns[eqns]; *)
  {lhss, rhss} = KeysValues @ eqns;
  varToEqI = Map[var |-> SelectIndices[eqns, ContainsQ[var]], vars];
  eqnIToRhsVarIs = Map[rhs |-> SelectIndices[vars, ContainedInQ[rhs]], rhss];
  eqnIToLhsVarI = Lookup[varI, lhss];
  rhsVarIsToEqnIs = PositionIndex[eqnIToRhsVarIs];
  rhsVarIToEqnIs = PositionIndex[eqnIToRhsVarIs, 2];
  solutions = Data`UnorderedAssociation @ MapThread[
    If[#2 === {}, #1, Nothing]&,
    {eqns, eqnIToRhsVarIs}
  ];
  solved = KeyExistsQ[solutions, #]& /@ vars;
  solvedHistory = {solved};
  solveStep[SelectIndices[solved, TrueQ]];
  If[TrueQ @ verifySolutions,
    If[AnyTrue[MatrixMap[Chop, eqns /. solutions], Apply[Unequal]],
      badEqns =  Select[eqns, TrueQ[Unequal @@ (# /. solutions)]&];
      badVars = Union @ Keys @ badEqns;
      Message[SolveCyclicEquations::badsol, Row[badVars, ", "]];
      (* TODO: print equation dependency graph, highlight bad nodes, label equations, etc *)
      KeyValueScan[printBadSolEqs, TakeOperator[UpTo[2]] @ AssociationMap[Select[eqns, ContainsQ[#]]&, badVars]];
    ];
  ];
  If[FalseQ @ allowPartialSolutions,
    If[Length[solutions] < Length[vars],
      Message[SolveCyclicEquations::partsol, Length[solutions], Length[vars], Complement[vars, Keys @ solutions]];
    ];
  ];
  VPrint @ Row[{Column[vars, Spacings -> .71], CompactArrayPlot[Transpose @ solvedHistory, PixelConstrained -> 20]}, Alignment -> Bottom];
  Association @ solutions
,
  solveStep[newSolved_] := Block[{relevantEqnIs, newEqnsIs, nextSolved},
    (* find all equations that use a recently solved variable *)
    relevantEqnIs = Flatten @ Lookup[rhsVarIToEqnIs, newSolved, {}];
    (* discard those that define an already solved variable *)
    relevantEqnIs0 = relevantEqnIs;
    relevantEqnIs = Discard[relevantEqnIs, Part[solved, Part[eqnIToLhsVarI, #]]&];
    If[relevantEqnIs === {}, Return[]];
    (* select those whose RHS is fully solved *)
    newEqnsIs = Select[relevantEqnIs, Apply[And, Part[solved, Part[eqnIToRhsVarIs, #]]]&];
    (* run those equations and get set of newly solved variables *)
    nextSolved = Union @ Map[runEqn, Part[eqns, newEqnsIs]];
    If[$verbose, AppendTo[solvedHistory, solved]];
    (* recurse *)
    solveStep[nextSolved];
  ]
,
  runEqn[var_ -> rhs_] := Block[{vari},
    vari = Lookup[varI, var];
    solutions[var] = rhs /. solutions;
    Part[solved, vari] = True;
    vari
  ]
,
  toSubEquations := Case[
    rule_ := rule;
    eq:Rule[lhs_, rhs:(_Times|_Plus)] /; FreeQ[rhs, Max|Min|ComposeWhenList] := Scope[
      zero = lhs - rhs;
      vars = DeepUniqueCases[rhs, varP];
      eqns = # -> SolveLinearTermFor[#, zero, 0]& /@ vars;
      {eq, DeleteCases[eqns, _ -> None]}
    ]
  ]
];

printBadSolEqs[var_, eqns_List] := (
  Print["Var ", var, " cannot simultaneously satisfy following equations: "];
  Print[Grid[
    ReplaceAll[
      {Inactive[Equal] @@ #, Construct[HoldForm, Inactive[Equal] @@ #] /. solutions, Last[#] /. solutions}& /@ eqns,
      var -> Style[var, Bold]
    ],
    Spacings -> {1.5, 2}, Dividers -> All, Alignment -> Left
  ]];
);

eqStr[e_] := ToPrettifiedString[e, FullSymbolContext -> False];

(**************************************************************************************************)

PrivateFunction[printEqns]

printEqns[{}] := Print["---no equations---"];
printEqns[eqns_] := Print @ Grid[
  Catenate /@ Partition[
    formatEqn @@@ (Sort @ Normal @ Merge[eqns, Identity]),
    UpTo[4]
  ],
  ItemSize -> Full, Spacings -> 2, Alignment -> {{Right, Left}, Center},
  Dividers -> $Gray
];

formatEqn[lhs_, rhs_] := {lhs, Column[rhs, Dividers -> {None, {None, {$LightGray}, None}}]};

(**************************************************************************************************)

PublicFunction[SolveLinearTermFor]

SolveLinearTermFor[v_, l_] := SolveLinearTermFor[v, l, 0];
SolveLinearTermFor[v_, l_Plus, r_] := SolveLinearTermFor[v, Select[l, ContainsQ[v]], r - Discard[l, ContainsQ[v]]];
SolveLinearTermFor[v_, l_Times, r_] := SolveLinearTermFor[v, Select[l, ContainsQ[v]], r / Discard[l, ContainsQ[v]]];
SolveLinearTermFor[v_, v_, r_] := r;
SolveLinearTermFor[v_, lhs_, rhs_] := ThrowMessage["nsolvelinearterm", lhs, rhs, v];

General::nsolvelinearterm = "Cannot expand linear equation `` == `` to solve for ``.";
