PublicFunction[SolveCyclicEquations]

PublicOption[AllowPartialSolutions, EquationVariables, ExpandLinearEquations]

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

SolveCyclicEquations::badsol = "Could not verify solution: ``.";
SolveCyclicEquations::partsol = "Solution is partial. Have `` solved variables out of ``, unsolved variables: ``.";

SolveCyclicEquations[eqns:{___Rule}, OptionsPattern[]] := CatchMessage @ Scope[
  UnpackOptions[verifySolutions, allowPartialSolutions, equationVariables, expandLinearEquations, $verbose];
  lhss = Keys @ eqns;
  If[equationVariables === Automatic,
    vars = Union @ lhss,
    vars = Union[lhss, equationVariables]
  ];
  varP = Alternatives @@ vars;
  If[expandLinearEquations, eqns = Flatten @ Map[toSubEquations, eqns]];
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
    If[AnyTrue[eqns /. solutions, Apply[Unequal]],
      Message[SolveCyclicEquations::badsol, Column[
        StringForm["``\t(`` = ``)", #, HoldForm[#] /. solutions, Last[#] /. solutions]& /@ Select[eqns, TrueQ[Unequal @@ (# /. solutions)]&]
      ]];
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
    eq:Rule[lhs_, rhs:(_Times|_Plus)] := Scope[
      zero = lhs - rhs;
      vars = DeepUniqueCases[rhs, varP];
      {eq, # -> SolveLinearTermFor[#, zero, 0]& /@ vars}
    ]
  ]
];

printEqns[eqns_] := Print @ Multicolumn[Pane[#1, ImageSize -> 100] -> Pane[Column[#2, Dividers -> All], ImageSize -> 250]& @@@ (Sort @ Normal @ Merge[eqns, Identity]), 4, Appearance -> "Horizontal", ItemSize -> Full];

(**************************************************************************************************)

PublicFunction[SolveLinearTermFor]

SolveLinearTermFor[v_, l_] := SolveLinearTermFor[v, l, 0];
SolveLinearTermFor[v_, l_Plus, r_] := SolveLinearTermFor[v, Select[l, ContainsQ[v]], r - Discard[l, ContainsQ[v]]];
SolveLinearTermFor[v_, l_Times, r_] := SolveLinearTermFor[v, Select[l, ContainsQ[v]], r / Discard[l, ContainsQ[v]]];
SolveLinearTermFor[v_, v_, r_] := r;
SolveLinearTermFor[v_, lhs_, rhs_] := ThrowMessage["nsolvelinearterm", lhs, rhs, v];

General::nsolvelinearterm = "Cannot expand linear equation `` == `` to solve for ``.";
