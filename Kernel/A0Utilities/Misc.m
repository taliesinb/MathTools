PublicFunction[Tau]

SetUsage @ "
Tau is an alias for 2 * %Pi.
"

Tau = 2 * Pi;

(**************************************************************************************************)

PublicFunction[RoundNear]

RoundNear[n_] := If[n == Round[n], Round @ n, n];

(**************************************************************************************************)

PublicFunction[RescaleTo]

RescaleTo[array_, range_] := Rescale[array, MinMax @ array, range]
RescaleTo[range_][array_] := RescaleTo[array, range];

(**************************************************************************************************)

PublicFunction[ClipUnit]

ClipUnit[e_] := Clip[e, {0, 1}];

(**************************************************************************************************)

PublicFunction[Clip2]

Clip2[{x_, y_}, {xSpec_, ySpec_}] := {Clip[x, xSpec], Clip[y, ySpec]};
Clip2[spec_][e_] := Clip2[e, spec];

(**************************************************************************************************)

PublicFunction[ListPart]

ListPart[l_List, p_] := Part[l, p];

(**************************************************************************************************)

PublicFunction[ToPair]

General::notPair = "Expected a single item or a pair, not ``.";

ToPair = Case[
  a:Except[_List] := {a, a};
  {a_, b_}        := {a, b};
  a_              := Msg::notPair[a]
];

(**************************************************************************************************)

PublicFunction[ListRiffle, ScalarRiffle]

ListRiffle[list_List, {}] := list;
ListRiffle[list_List, riffleList_List] := Scope[
  riff = PadRight[riffleList, Len[list], L @ riffleList];
  Most @ Catenate @ Transpose[{list, riff}]
];

ScalarRiffle[list_List, scalar_] :=
  Most @ Catenate @ Transpose[{list, Repeat[scalar, Len @ list]}];

(**************************************************************************************************)

PublicFunction[DeepFirstCase]

SetHoldRest[DeepFirstCase];
DeepFirstCase[expr_, patt_, except_:None] :=
  FirstCase[expr, patt, except, {0, Inf}, Heads -> True];

(**************************************************************************************************)

PublicFunction[FloorCeiling]

SetListable[FloorCeiling];
FloorCeiling[i_] := {Floor @ i, Ceiling @ i}
FloorCeiling[i_, n_] := {Floor[i, n], Ceiling[i, n]}

(**************************************************************************************************)

PublicFunction[Avg]

Avg[] := 0;
Avg[a_] := a;
Avg[a_, b_] := (a + b)/2;
Avg[a_, b_, c_] := (a + b + c)/3;
Avg[args__] := Mean[{args}];

(**************************************************************************************************)

PublicFunction[Power10, Log10Ceiling, Log10Floor]

Power10[r_] := Power[10, r];

SetUsage @ "
Log10Floor[r$] computes the greatest power of 10 smaller than r$.
Log10Floor[r$, n$] allows for splitting the decimal range into n$ subdivisions.
"

SetUsage @ "
Log10Ceiling is like Log10Floor.
"

Log10Ceiling[r_, n_:1] := log10CF[N @ r, n, Ceiling];
Log10Floor[r_, n_:1]   := log10CF[N @ r, n, Floor];

log10CF = Case[
  Seq[0., n_, _]             := 0;
  Seq[r_List, n_, f_]        := Map[%[#, n, f]&, r];
  Seq[r_ ? Negative, n_, f_] := -%[Abs @ r, n, flipFC @ f];
  Seq[r_Real, n_, f_]        := logt10CFSplit[n, r, f] @ %[r, 1, f];
  Seq[r_Real, 1, f_]         := Power10 @ N @ f[Log10[r]];
  _                          := $Failed
];

logt10CFSplit[n_, r_, Ceiling][p_] := Min @ Select[p/10 * into10[n], GreaterEqualThan[r]];
logt10CFSplit[n_, r_, Floor][p_]   := Max @ Select[p * into10[n], LessEqualThan[r]];

into10[2]   := {1, 5, 10};
into10[4]   := {1, 2.5, 5, 7.5, 10};
into10[All] := {1, 2, 2.5, 3, 4, 5, 6, 7, 7.5, 8, 9, 10};
into10[5]   := {1, 2, 4, 6, 8, 10};
into10[_]   := {1, 10};

flipFC[Floor]   = Ceiling;
flipFC[Ceiling] = Floor;

(**************************************************************************************************)

PublicFunction[NLogN]

SetListable[NLogN];
NLogN[0|0.] := 0;
NLogN[n_] := n * Log2[n];

(**************************************************************************************************)

PublicFunction[RandomSeeded]

SetUsage @ "
RandomSeeded[body$, seeding$] evaluates body$ with %%RandomSeeding -> seeding$.
* seeding$ of Automatic does not localize the RNG when evaluating body$.
"

SetHoldFirst[RandomSeeded];

RandomSeeded[body_, Auto] := body;
RandomSeeded[body_, other_] := BlockRandom[body, RandomSeeding -> other];

(**************************************************************************************************)

PrivateFunction[toListOfLists]

toListOfLists[list:{__List}] := list;
toListOfLists[list_List] := {list};
toListOfLists[_] := $Failed;

(**************************************************************************************************)

PublicFunction[ToInverseFunction]

SetUsage @ "
ToInverseFunction[f$] returns %InverseFunction[f$].
* ToInverseFunction exists to enable a fast-path for MathTools-specific functions.
"

ToInverseFunction[e_] := InverseFunction[e];

(**************************************************************************************************)

PrivateFunction[LookupAnnotation]

SetUsage @ "
LookupAnnotation[object$, key$] gives the value of the annotation associated with key$ in object$.
LookupAnnotation[object$, {key$1, key$2, $$}] gives a list of values.
LookupAnnotation[object$, spec$, default$] evaluates and returns default$ if the key or keys is not present.
* By default, if no value is present, Automatic is returned.
"

SetHoldRest[LookupAnnotation];

LookupAnnotation[obj_, key_, default_:Auto] :=
  OnFailed[AnnotationValue[obj, key], default];

LookupAnnotation[obj_, key_List, default_:Auto] :=
  VectorReplace[AnnotationValue[obj, key], $Failed :> default];

(**************************************************************************************************)

PrivateVariable[$TriangleVectors]

$TriangleVectors = {{0, 1}, {-1/2*Sqrt[3], -1/2}, {Sqrt[3]/2, -1/2}};

(**************************************************************************************************)

PrivateFunction[LengthNormalize]

LengthNormalize[{}] := {};
LengthNormalize[e_] := e / Len[e];

(**************************************************************************************************)

PrivateFunction[TotalNormalize]

TotalNormalize[{}] := {};
TotalNormalize[e_] := e / Max[Total[e], $MachineEpsilon];

(**************************************************************************************************)

PrivateFunction[notInternalSymbolQ]

SetHoldFirst @ notInternalSymbolQ;

notInternalSymbolQ[sym_Symbol] := !SEndsQ[SymbolName @ Uneval @ sym, "$"];
notInternalSymbolQ[_] := True;

(**************************************************************************************************)

PublicFunction[ModIncrement, ModDecrement]

SetHoldFirst[ModIncrement, ModDecrement];
ModIncrement[var_, n_] := Set[var, Mod[var + 1, n, 1]];
ModDecrement[var_, n_] := Set[var, Mod[var - 1, n, 1]];