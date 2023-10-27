PrivateFunction[BoundsToSize]

BoundsToSize[{{x1_, x2_}, {y1_, y2_}}] := {x2 - x1, y2 - y1};

(**************************************************************************************************)

PrivateFunction[ParseAlignment]

General::badAligOrCoords = "Setting `` -> `` is not a symbolic side or coordinate."

ParseAlignment = Case[
  Seq[{x:$NumberP, y:$NumberP}, _, _] := {x, y};
  Seq[side_Symbol, h_, o_]            := Lookup[$SideToUnitCoords, side, Message[MessageName[h, "badAligOrCoords"], o, MsgExpr @ side]; {0, 0}];
  Seq[spec_, h_, o_]                  := (Message[MessageName[h, "badAligOrCoords"], o, MsgExpr @ spec]; {0, 0})
]

(**************************************************************************************************)

PrivateFunction[ParseCyclicSpec]

SetUsage @ "
ParseCyclicSpec[spec$, n$] fills out a vector of n$ specs from a cyclic specification spec$.
ParseCyclicSpec[n$] is the operator form of ParseCyclicSpec.
* it mirrors how e.g. %RowAlignments is implemented for %GridBox.
* Automatic will be used to fill if a too-short list is given.
* $Failed will be returned if it is an invalid spec.
* the more helpful spec %Repeating can be used.
* additionally, a list of rules can be used that send positions to specs.
"

ParseCyclicSpec[spec_, n_] := Scope[
  $n = n;
  parseCyclic @ spec
];

ParseCyclicSpec[n_][spec_] := ParseCyclicSpec[spec, n];

$atomSpecP = _Symbol | _Integer | _Real | _Rational;

parseCyclic = Case[
  rules:$RuleListPattern             := VectorReplace[Range @ $n, Append[compNeg /@ rules, _ -> Automatic]];
  spec:{$atomSpecP..}                := PadRight[spec, $n, Automatic];
  Repeating[mid__]                   := repSpec[{}, {mid}, {}];
  spec:$atomSpecP                    := ConstantArray[spec, $n];
  {{spec:$atomSpecP}}                := ConstantArray[spec, $n];
  {l___, {}, r___}                   := repSpec[{l}, {Automatic}, {r}];
  {l___, Repeating[mid__], r___}     := repSpec[{l}, {mid}, {r}];
  {l___, mid:{$atomSpecP..}, r___}   := repSpec[{l}, mid, {r}];
  _                                  := $Failed;
];

compNeg[head_[lhs_, rhs_]] := With[{lhs2 = lhs /. n_Integer ? Negative :> (n + $n + 1)}, head[lhs2, rhs]];

repSpec[l_, mid_, r_] := Scope[
  n2 = $n - Length[l] - Length[r];
  If[n2 < 0, Return @ parseCyclic @ Join[l, r]];
  mid2 = TakeOperator[n2] @ Catenate @ ConstantArray[mid, Ceiling[n2 / Length[mid]]];
  Join[l, mid2, r]
];
