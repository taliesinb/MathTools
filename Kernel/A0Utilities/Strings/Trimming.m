PrivateFunction[StringTrimLeft, StringTrimRight, StringTrimLeftRight]

StringTrimLeft[str_Str, left_] := StringDelete[str, StartOfString ~~ left];
StringTrimLeft[list_List, left_] := Map[StringTrimLeft[#, left]&, list];
StringTrimLeft[left_][str_] := StringTrimLeft[str, left];

StringTrimRight[str_Str, right_] := StringDelete[str, right ~~ EndOfString];
StringTrimRight[list_List, right_] := Map[StringTrimRight[#, right]&, list];
StringTrimRight[right_][str_] := StringTrimRight[str, right];

StringTrimLeftRight[str_Str, left_, right_] := StringDelete[StringDelete[str, StartOfString ~~ left], right ~~ EndOfString];
StringTrimLeftRight[list_List, left_, right_] := Map[StringTrimLeftRight[#, left, right]&, list];
StringTrimLeftRight[left_, right_][str_] := StringTrimLeftRight[str, left, right];
