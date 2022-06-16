PackageExport["Tau"]

SetUsage @ "
Tau is an alias for 2 * %Pi.
"

Tau = 2 * Pi;

(**************************************************************************************************)

PackageExport["NLogN"]

SetListable[NLogN];
NLogN[0|0.] := 0;
NLogN[n_] := n * Log2[n];

(**************************************************************************************************)

PackageExport["RandomSeeded"]

SetUsage @ "
RandomSeeded[body$, seeding$] evaluates body$ with %%RandomSeeding -> seeding$.
* seeding$ of Automatic does not localize the RNG when evaluating body$.
"

SetHoldFirst[RandomSeeded];

RandomSeeded[body_, Automatic] := body;
RandomSeeded[body_, other_] := BlockRandom[body, RandomSeeding -> other];

(**************************************************************************************************)

PackageScope["summaryItem"]

summaryItem[a_, b_] := BoxForm`SummaryItem[{a <> ": ", b}];

(**************************************************************************************************)

PackageScope["toListOfLists"]

toListOfLists[list:{__List}] := list;
toListOfLists[list_List] := {list};
toListOfLists[_] := $Failed;

(**************************************************************************************************)

PackageExport["ToInverseFunction"]

SetUsage @ "
ToInverseFunction[f$] returns %InverseFunction[f$].
* ToInverseFunction exists to enable a fast-path for QuiverGeometry-specific functions.
"

ToInverseFunction[e_] := InverseFunction[e];

(**************************************************************************************************)

PackageExport["DeleteNull"]

SetUsage @ "
DeleteNull[list$] removes any elements that are Null from list$.
"

DeleteNull[e_] := DeleteCases[e, Null];

(**************************************************************************************************)

PackageExport["LookupAnnotation"]

SetUsage @ "
LookupAnnotation[object$, key$] gives the value of the annotation associated with key$ in object$.
LookupAnnotation[object$, {key$1, key$2, $$}] gives a list of values.
LookupAnnotation[object$, spec$, default$] evaluates and returns default$ if the key or keys is not present.
* By default, if no value is present, Automatic is returned.
"

SetHoldRest[LookupAnnotation];

LookupAnnotation[obj_, key_, default_:Automatic] :=
  OnFailed[AnnotationValue[obj, key], default];

LookupAnnotation[obj_, key_List, default_:Automatic] :=
  VectorReplace[AnnotationValue[obj, key], $Failed :> default];

(**************************************************************************************************)

PackageExport["$TriangleVectors"]

$TriangleVectors = {{0, 1}, {-1/2*Sqrt[3], -1/2}, {Sqrt[3]/2, -1/2}};

(**************************************************************************************************)

PackageExport["LengthNormalize"]

LengthNormalize[{}] := {};
LengthNormalize[e_] := e / Length[e];

