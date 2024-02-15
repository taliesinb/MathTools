PublicFunction[WrapDQuotes, StripDQuotes, DQuotedStringQ]

SetListable[WrapDQuotes, StripDQuotes, DQuotedStringQ]

WrapDQuotes[s_Str] := StringJoin["\"", s, "\""];
StripQuotes[s_Str] := If[StringStartsQ[s, "\""] && StringEndsQ[s, "\""], StringTake[s, {2, -2}], s];
DQuotedStringQ[s_Str] := StringStartsQ[s, "\""] && StringEndsQ[s, "\""];

(**************************************************************************************************)

PublicFunction[WrapSQuotes, StripSQuotes, SQuotedStringQ]

SetListable[WrapSQuotes, StripSQuotes, SQuotedStringQ]

WrapSQuotes[s_Str] := StringJoin["'", s, "'"];
StripSQuotes[s_Str] := If[StringStartsQ[s, "'"] && StringEndsQ[s, "'"], StringTake[s, {2, -2}], s];
SQuotedStringQ[s_StrString] := StringStartsQ[s, "'"] && StringEndsQ[s, "'"];
SQuotedStringQ[_] := False;

(**************************************************************************************************)

PublicFunction[WrapParens, StripParens, ParensStringQ]

SetListable[WrapParens, StripParens, ParensStringQ]

WrapParens[s_Str] := StringJoin["(", s, ")"];
StripParens[s_Str] := If[StringStartsQ[s, "("] && StringEndsQ[s, ")"], StringTake[s, {2, -2}], s];
ParensStringQ[s_Str] := StringStartsQ[s, "("] && StringEndsQ[s, ")"];
ParensStringQ[_] := False;

(**************************************************************************************************)

PublicFunction[WrapBraces, StripBraces, BracesStringQ]

SetListable[WrapBraces, StripBraces, BracesStringQ]

WrapBraces[s_Str] := StringJoin["{", s, "}"];
StripBraces[s_Str] := If[StringStartsQ[s, "{"] && StringEndsQ[s, "}"], StringTake[s, {2, -2}], s];
BracesStringQ[s_Str] := StringStartsQ[s, "{"] && StringEndsQ[s, "}"];
BracesStringQ[_] := False;

(**************************************************************************************************)

PublicFunction[WrapBrackets, StripBrackets, BracketsStringQ]

SetListable[WrapBrackets, StripBrackets, BracketsStringQ]

WrapBrackets[s_Str] := StringJoin["[", s, "]"];
StripBrackets[s_Str] := If[StringStartsQ[s, "["] && StringEndsQ[s, "]"], StringTake[s, {2, -2}], s];
BracketsStringQ[s_Str] := StringStartsQ[s, "["] && StringEndsQ[s, "]"];
BracketsStringQ[_] := False;

(**************************************************************************************************)

PublicFunction[WrapDoubleBrackets, StripDoubleBrackets, DoubleBracketsStringQ]

SetListable[WrapDoubleBrackets, StripDoubleBrackets, DoubleBracketsStringQ]

WrapDoubleBrackets[s_Str] := StringJoin["[[", s, "]]"];
StripDoubleBrackets[s_Str] := If[StringStartsQ[s, "[["] && StringEndsQ[s, "]]"], StringTake[s, {3, -3}], s];
DoubleBracketsStringQ[s_Str] := StringStartsQ[s, "[["] && StringEndsQ[s, "]]"];
DoubleBracketsStringQ[_] := False;

