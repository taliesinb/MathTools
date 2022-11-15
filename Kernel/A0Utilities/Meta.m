PublicFunction[CreateSymbol]

CreateSymbol[name_String, value_] :=
  ToExpression[name, InputForm, SetOperator[value]];

(**************************************************************************************************)

PublicFunction[CreateMultipleSymbols]

CreateMultipleSymbols[context_, names:{___String}, values_List] := Block[
  {$Context = context, $ContextPath = {"System`", "Global`"}},
  If[Length[names] =!= Length[values], Message[CreateMultipleSymbols::badlen, Length @ names, Length @ values, Short @ names]];
  ToExpression[StringJoin["{", Riffle[names, ","], "}"], InputForm, SetOperator[values]]
]

CreateMultipleSymbols::badlen = "Was provided `` names and `` values: ``."
CreateMultipleSymbols[___] := $Failed;

(**************************************************************************************************)
