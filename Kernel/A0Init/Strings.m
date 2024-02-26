PrivateFunction[PrefixSlash]

PrefixSlash[s_] := SJoin["\\", s];

(**************************************************************************************************)

PrivateFunction[StringStartsEndsQ]

StringStartsEndsQ[str_Str, a_, b_] := SStartsQ[str, a] && SEndsQ[str, b];
StringStartsEndsQ[str_List, a_, b_] := Map[StringStartsEndsQ[#, a, b]&, str];
StringStartsEndsQ[a_, b_][str_] := StringStartsEndsQ[str, a, b];

(**************************************************************************************************)

PublicFunction[SingleCharQ]

SingleCharQ[s_Str] := SLen[s] == 1;
SingleCharQ[_] := False;

(**************************************************************************************************)

PublicFunction[RandomString]

$base36Chars = Chars @ "abcdefghijklmnopqrstuvwxyz0123456789";
RandomString[n_Int] := SJoin @ Part[$base36Chars, RandomInteger[{1, 36}, n]];

(**************************************************************************************************)

PublicFunction[StringJoinLeft, StringJoinRight]

StringJoinLeft[prefix_Str, other_Str] := SJoin[prefix, other];
StringJoinLeft[prefix_Str, other_List] := Map[StringJoinLeft[prefix], other];
StringJoinLeft[prefix_Str][other_] := StringJoinLeft[prefix, other];

StringJoinRight[other_Str, suffix_Str] := SJoin[other, suffix];
StringJoinRight[other_List, suffix_Str] := Map[StringJoinRight[suffix], other];
StringJoinRight[suffix_Str][other_] := StringJoinRight[other, suffix];
  
(**************************************************************************************************)

PublicFunction[StringFunction]

StringFunction[template_Str] :=
  Construct[
    Fn,
    SRep[template, $stringFunctionSlotRules]
  ] /. {SExpr -> SJoin, s_Slot :> TextString[s]};

$stringFunctionSlotRules = {
  "##" -> "#",
  "#" ~~ i:DigitCharacter :> Slot[FromDigits[i]],
  "#" ~~ w:LetterCharacter.. :> Slot[w]
};

(**************************************************************************************************)

PublicFunction[StringDeepCases]

StringDeepCases[expr_, pattern_] := Catenate @ DeepCases[expr, s_Str :> SCases[s, pattern]];
StringDeepCases[pattern_][expr_] := StringDeepCases[expr, pattern];

(**************************************************************************************************)

PublicFunction[FirstStringCase]

SetHoldRest[FirstStringCase];

FirstStringCase[string_, pattern_, else_:None] :=
  F[SCases[string, pattern, 1], else];

(**************************************************************************************************)

PublicFunction[LowerCaseFirst, UpperCaseFirst]

LowerCaseFirst[str_Str] := SJoin[ToLowerCase @ STake[str, 1], SDrop[str, 1]];
UpperCaseFirst[str_Str] := SJoin[ToUpperCase @ STake[str, 1], SDrop[str, 1]];

(**************************************************************************************************)

PublicFunction[UpperCaseFirstQ, LowerCaseFirstQ]

SetListable[UpperCaseFirstQ, LowerCaseFirstQ]

UpperCaseFirstQ[""] = False;
UpperCaseFirstQ[str_Str] := UpperCaseQ @ STake[str, 1];

LowerCaseFirstQ[""] = False;
LowerCaseFirstQ[str_Str] := LowerCaseQ @ STake[str, 1];

(**************************************************************************************************)

PublicFunction[LinearSyntaxQ, ContainsLinearSyntaxQ]

LinearSyntaxQ[s_Str] := StringStartsEndsQ[s, "\!\(\*", "\)"] && StringBalancedQ[s, "\(", "\)"];
ContainsLinearSyntaxQ[s_Str] := SContainsQ[s, "\!\(\*"] && SContainsQ[s, "\)"];

(**************************************************************************************************)

PublicFunction[StringBalancedQ]

StringBalancedQ[str_Str, l_, r_] := balancedQ @ Accumulate @ SCases[str, {l -> 1, r -> -1}];
StringBalancedQ[l_, r_][str_] := StringBalancedQ[str, l, r];
balancedQ[{}] := True;
balancedQ[ints_] := Min[ints] >= 0 && L[ints] == 0;

(**************************************************************************************************)

PublicFunction[ToLinearSyntax, FromLinearSyntax]

ToLinearSyntax[e_] := SJoin["\!\(\*", ToString[ToBoxes @ e, InputForm], "\)"];

FromLinearSyntax[str_Str] := If[StringStartsEndsQ[str, "\!\(\*", "\)"], tryEvalStr @ STake[str, {4, -2}], $Failed];
FromLinearSyntax[_] := $Failed;

tryEvalStr[""] := $Failed;
tryEvalStr[str_] := Quiet @ Check[ToExpression[str, InputForm, Hold], $Failed];

(**************************************************************************************************)

PublicFunction[UpperCaseLast, LowerCaseLast]

UpperCaseLast[str_Str] := SJoin[SDrop[str, -1], ToUpperCase @ STake[str, -1]];
LowerCaseLast[str_Str] := SJoin[SDrop[str, -1], ToLowerCase @ STake[str, -1]];

(**************************************************************************************************)

PublicFunction[ToTitleString]

ToTitleString[s_Str] :=
  ToLowerCase @ SRep[s, RegularExpression["([a-z])([A-Z])"] :> "$1 $2"];

(**************************************************************************************************)

PublicFunction[CamelCaseSplit]

CamelCaseSplit[s_Str] := SSplit[s, RegularExpression["(?<=[a-z])(?=[A-Z])"]];

(**************************************************************************************************)

PrivateFunction[commaString]

qs[s_Str] := PrefixSlash[s];
qs[e_] := e;
commaString[list_List] := TextString[Row[Map[qs, list], ", "]];

(**************************************************************************************************)

PublicFunction[StringReplaceRepeated]

StringReplaceRepeated[str_Str, rules_] := FixedPoint[SRep[rules], str];
StringReplaceRepeated[rules_][str_] := StringReplaceRepeated[str, rules];

(**************************************************************************************************)

PrivateFunction[IntStr2]

IntStr2[n_] := IntStr[n, 10, 2];

(**************************************************************************************************)

PublicFunction[FromHexStr, ToHexStr]

SetListable[FromHexStr];
FromHexStr[s_Str] := FromDigits[s, 16];

ToHexStr[n_]      := IntStr[n, 16];
ToHexStr[n_, b_]  := IntStr[n, 16, b];
