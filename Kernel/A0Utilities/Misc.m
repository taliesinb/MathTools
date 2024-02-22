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

PublicFunction[ListPart]

ListPart[l_List, p_] := Part[l, p];

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
* ToInverseFunction exists to enable a fast-path for QuiverGeometry-specific functions.
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