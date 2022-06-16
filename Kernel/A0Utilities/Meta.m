PackageExport["PerformSelfLinting"]

PerformSelfLinting[] := Scope[
  DeleteCases[{} | <||>] @ Association[
    "MissingPackageScopes" -> findMissingPackageScopes[],
    "SuspiciousPackageLines" -> QuiverGeometryPackageLoader`$SuspiciousPackageLines
  ]
];

findMissingPackageScopes[] := Scope[
  privateSymbols = Names["QuiverGeometry`**`*"];
  privateSymbolNames = Last /@ StringSplit[privateSymbols, "`"];
  moduleSymbols = Select[DeleteDuplicates @ privateSymbolNames, StringEndsQ["$"]];
  moduleSymbols = Join[moduleSymbols, StringDrop[moduleSymbols, -1]];
  privateSymbolAssoc = AssociationThread[privateSymbols, privateSymbolNames];
  privateSymbolAssoc //= Select[StringLength[#] >= 4&];
  privateSymbolAssoc //= Discard[ElementQ[moduleSymbols]];
  collisionsAssoc = Select[PositionIndex[privateSymbolAssoc], Length[#] > 1&];
  collisionsAssoc //= Select[possibleMissingPackageScope];
  collisionsAssoc
]

possibleMissingPackageScope[names_] :=
  CountDistinct[ToExpression[names, InputForm, System`Private`HasAnyEvaluationsQ]] > 1;

(**************************************************************************************************)

PackageExport["CreateSymbol"]

CreateSymbol[name_String, value_] :=
  ToExpression[name, InputForm, SetOperator[value]];

(**************************************************************************************************)

PackageExport["CreateMultipleSymbols"]

CreateMultipleSymbols[context_, names:{___String}, values_List] := Block[
  {$Context = context, $ContextPath = {"System`", "Global`"}},
  If[Length[names] =!= Length[values], Message[CreateMultipleSymbols::badlen, Length @ names, Length @ values]];
  ToExpression[StringJoin["{", Riffle[names, ","], "}"], InputForm, SetOperator[values]]
]

CreateMultipleSymbols::badlen = "Was provided `` names and `` values."
CreateMultipleSymbols[___] := $Failed;

(**************************************************************************************************)
