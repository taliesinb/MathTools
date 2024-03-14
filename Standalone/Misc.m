Begin["`Misc`"];

Clear[SWith];

SetAttributes[SWith, HoldAllComplete];

SWith[v_Symbol, body_] :=
  With[{v = v}, body];

SWith[{}, body_] := body;

SWith[{v_Symbol}, body_] :=
  With[{v = v}, body];

SWith[{v1_Symbol, v2_Symbol}, body_] :=
  With[{v1 = v1, v2 = v2}, body];

SWith[{v1_Symbol, v2_Symbol, v3_Symbol}, body_] :=
  With[{v1 = v1, v2 = v2, v3 = v3}, body];

SWith[{v1_Symbol, v2_Symbol, v3_Symbol, v4_Symbol, rest___}, body_] :=
  SWith[{rest}, With[{v1 = v1, v2 = v2, v3 = v3, v4 = v4}, body]];

SWith::badargs = "Bad arguments to SWith: ``.";
SWith[args___] := (Message[SWith::badargs, HoldForm[{args}]]; $Failed);

(**************************************************************************************************)

Clear[RealString];

SetAttributes[RealString, Listable];

$numDigits = 5;

RealString[value_, digits_Integer] := Block[{$numDigits = Max[digits, 1]}, RealString @ value];

RealString[0|0.] := "0";
RealString[real_Real ? Developer`MachineRealQ] := dropDot @ Internal`MRealToString[real, False, $numDigits];
RealString[real_Real] := GeneralUtilities`RealDigitsString[TextString; real, $numDigits];
RealString[Infinity] := "\[Infinity]";
RealString[-Infinity] := "-\[Infinity]";
RealString[number_ ? NumericQ] := RealString[N @ number];

dropDot[s_] := If[StringTake[s, -1] === ".", StringDrop[s, -1], s];

End[];