PublicFunction[WrapDQuotes, StripDQuotes, DQuotedStringQ]

SetListable[WrapDQuotes, StripDQuotes, DQuotedStringQ]

WrapDQuotes[s_Str] := SJoin["\"", s, "\""];
StripDQuotes[s_Str] := If[SStartsQ[s, "\""] && SEndsQ[s, "\""], STake[s, {2, -2}], s];
DQuotedStringQ[s_Str] := SStartsQ[s, "\""] && SEndsQ[s, "\""];

(**************************************************************************************************)

PublicFunction[WrapSQuotes, StripSQuotes, SQuotedStringQ]

SetListable[WrapSQuotes, StripSQuotes, SQuotedStringQ]

WrapSQuotes[s_Str] := SJoin["'", s, "'"];
StripSQuotes[s_Str] := If[SStartsQ[s, "'"] && SEndsQ[s, "'"], STake[s, {2, -2}], s];
SQuotedStringQ[s_StrString] := SStartsQ[s, "'"] && SEndsQ[s, "'"];
SQuotedStringQ[_] := False;

(**************************************************************************************************)

PublicFunction[WrapParens, StripParens, ParensStringQ]

SetListable[WrapParens, StripParens, ParensStringQ]

WrapParens[s_Str] := SJoin["(", s, ")"];
StripParens[s_Str] := If[SStartsQ[s, "("] && SEndsQ[s, ")"], STake[s, {2, -2}], s];
ParensStringQ[s_Str] := SStartsQ[s, "("] && SEndsQ[s, ")"];
ParensStringQ[_] := False;

(**************************************************************************************************)

PublicFunction[WrapBraces, StripBraces, BracesStringQ]

SetListable[WrapBraces, StripBraces, BracesStringQ]

WrapBraces[s_Str] := SJoin["{", s, "}"];
StripBraces[s_Str] := If[SStartsQ[s, "{"] && SEndsQ[s, "}"], STake[s, {2, -2}], s];
BracesStringQ[s_Str] := SStartsQ[s, "{"] && SEndsQ[s, "}"];
BracesStringQ[_] := False;

(**************************************************************************************************)

PublicFunction[WrapBrackets, StripBrackets, BracketsStringQ]

SetListable[WrapBrackets, StripBrackets, BracketsStringQ]

WrapBrackets[s_Str] := SJoin["[", s, "]"];
StripBrackets[s_Str] := If[SStartsQ[s, "["] && SEndsQ[s, "]"], STake[s, {2, -2}], s];
BracketsStringQ[s_Str] := SStartsQ[s, "["] && SEndsQ[s, "]"];
BracketsStringQ[_] := False;

(**************************************************************************************************)

PublicFunction[WrapDoubleBrackets, StripDoubleBrackets, DoubleBracketsStringQ]

SetListable[WrapDoubleBrackets, StripDoubleBrackets, DoubleBracketsStringQ]

WrapDoubleBrackets[s_Str] := SJoin["[[", s, "]]"];
StripDoubleBrackets[s_Str] := If[SStartsQ[s, "[["] && SEndsQ[s, "]]"], STake[s, {3, -3}], s];
DoubleBracketsStringQ[s_Str] := SStartsQ[s, "[["] && SEndsQ[s, "]]"];
DoubleBracketsStringQ[_] := False;

