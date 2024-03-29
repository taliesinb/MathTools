PrivateFunction[BoundsToSize]

BoundsToSize[{{x1_, x2_}, {y1_, y2_}}] := {x2 - x1, y2 - y1};

(**************************************************************************************************)

PrivateFunction[ParseAlignment]

General::badAligOrCoords = "Setting `` -> `` is not a symbolic side or coordinate."

ParseAlignment = Case[
  Seq[{x:$NumberP, y:$NumberP}, _, _] := {x, y};
  Seq[side_Symbol, h_, o_]            := Lookup[$SideToUnitCoords, side, Message[MessageName[h, "badAligOrCoords"], o, side]; {0, 0}];
  Seq[spec_, h_, o_]                  := (Message[MessageName[h, "badAligOrCoords"], o, spec]; {0, 0})
]

(**************************************************************************************************)

PrivateFunction[ParseListSpec]

SetUsage @ "
ParseListSpec[spec$, n$] fills out a vector of n$ specs from a simple list specification spec$.
ParseListSpec[n$] is the operator form of ParseListSpec.
* if spec$ is a non-list, it is simply repeated.
* if spec$ is a list, the last element is repeated as necessary.
"

General::noSpecItemToRepeat = "Empty specification.";
ParseListSpec[item_, n_Int] := Repeat[item, n];
ParseListSpec[{}, n_] := ThrowMessage["noSpecItemToRepeat"];
ParseListSpec[list_List, n_Int] := PadRight[list, n, Last @ list];

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

$atomSpecP = _Symbol | _Int | _Real | _Rational;

parseCyclic = Case[
  rules:$RuleListPattern             := VectorReplace[Range @ $n, App[compNeg /@ rules, _ -> Auto]];
  spec:{$atomSpecP..}                := PadRight[spec, $n, Auto];
  Repeating[mid__]                   := repSpec[{}, {mid}, {}];
  spec:$atomSpecP                    := Repeat[spec, $n];
  {{spec:$atomSpecP}}                := Repeat[spec, $n];
  {l___, {}, r___}                   := repSpec[{l}, {Auto}, {r}];
  {l___, Repeating[mid__], r___}     := repSpec[{l}, {mid}, {r}];
  {l___, mid:{$atomSpecP..}, r___}   := repSpec[{l}, mid, {r}];
  _                                  := $Failed;
];

compNeg[head_[lhs_, rhs_]] := With[{lhs2 = lhs /. n_Int ? Negative :> (n + $n + 1)}, head[lhs2, rhs]];

repSpec[l_, mid_, r_] := Scope[
  n2 = $n - Len[l] - Len[r];
  If[n2 < 0, Return @ parseCyclic @ Join[l, r]];
  mid2 = TakeOperator[n2] @ Catenate @ Repeat[mid, Ceiling[n2 / Len[mid]]];
  Join[l, mid2, r]
];
