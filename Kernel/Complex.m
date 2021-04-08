Package["GraphTools`"]


PackageImport["GeneralUtilities`"]


PackageExport["RootOfUnity"]

RootOfUnity[n_] := RootOfUnity[n, 1];

RootOfUnity[2, 1] := -1;
RootOfUnity[2, 2] := 1;

RootOfUnity[n_, i_] := RootOfUnity[n, i] = Expand @ ComplexExpand @ Exp[(i)/n * 2 Pi I];


PackageExport["UnitRoot"]

SetUsage @ "
UnitRoot[n$] represents the first n$'th root of unity, and stands in for a complex number.
"

UnitRoot[2] := -1;
UnitRoot /: Power[UnitRoot[n_], k_Integer] /; (k >= n) || Negative[k] := Power[UnitRoot[n], Mod[k, n]];

Format[UnitRoot[n_], StandardForm] := Subscript["\[Xi]", n];
Format[UnitRoot[n_], TraditionalForm] := Subscript["\[Xi]", n];


PackageScope["GetRootPower"]

GetRootPower[1] := 0;
GetRootPower[UnitRoot[n_]] := 1;
GetRootPower[Power[UnitRoot[n_], k_]] := k;


PackageScope["ContainsUnitRootsQ"]

ContainsUnitRootsQ[e_] := ContainsQ[e, _UnitRoot];


PackageScope["ExpandUnitRoots"]

ExpandUnitRoots[e_] :=
  If[ContainsUnitRootsQ[e], e /. UnitRoot[n_] :> RootOfUnity[n, 1], e];