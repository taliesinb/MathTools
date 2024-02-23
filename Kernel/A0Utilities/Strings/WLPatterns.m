PublicStringPattern[WLSymbolLetter, WLOpenDelimiterLetter, WLCloseDelimiterLetter, WLDelimiterLetter]

SetUsage @ "
WLSymbolLetter matches a letter that can occur as part of a WL symbol (not including backtick).
"

DefineStringLetterClass[
  WLSymbolLetter         -> "$[:alnum:]",
  WLOpenDelimiterLetter  -> "[({\[LeftAssociation]",
  WLCloseDelimiterLetter -> "\\]){\[RightAssociation]",
  WLDelimiterLetter      -> ",()[\\]{}\"\[LeftAssociation]\[RightAssociation]"
]

(**************************************************************************************************)

PublicStringPattern[WLSymbolSpan]

SetUsage @ "
WLSymbolSpan matches a (possibly qualified) WL symbol.
";


DefineStringPattern[
  WLSymbolSpan   :> "(?:[$a-zA-Z][`a-zA-Z0-9]+)"
];

(**************************************************************************************************)

PublicStringPattern[WLSymbolBoundary]

SetUsage @ "
WLSymbolBoundary matches a boundary between WL symbols and other text.
* unlike WordBoundary, '$' won't trigger a boundary and '_' will.
";

DefineStringPatternMacro[
  WLSymbolBoundary :> Alt[
    NegativeLookbehind[WLSymbolLetter] ~~ PositiveLookahead[WLSymbolLetter],
    PositiveLookbehind[WLSymbolLetter] ~~ NegativeLookahead[WLSymbolLetter]
  ]
];

(**************************************************************************************************)

PublicStringPattern[WLStrSpan]

SetUsage @ "
WLStrSpan matches a WL string, properly judging escaped double quotes etc.
";

(* TODO: optimize to not involve a callback! *)

DefineStringPatternMacro[
  WLStrSpan :> makeUniqueWLStrSpan[]
];

makeUniqueWLStrSpan[] := Module[{z}, Construct[Condition, z:("\"" ~~ Shortest[___] ~~ "\""), Uneval @ notPartialStrQ[z]]];

notPartialStrQ[s_] := EvenQ @ StringLength @ First[StringCases[s, "\\".. ~~ "\"" ~~ EndOfString], ""];

(**************************************************************************************************)

PublicStringPattern[WLDelimiterLetter]



(**************************************************************************************************)

PublicStringPattern[WLSpan]

SetUsage @ "
WLSpan matches a possible WL expression, up to an invalid delimiter or comma.
";

(*
TODO: Handle comments
TODO: properly check the jumpahead phrase for lack of <| and |>
TOOD: handle my special triple doublequote syntax
*)

DefineStringPatternMacro[
  WLSpan :> makeUniqueWLSpan[]
];

makeUniqueWLStrSpan[] := Module[{z}, Construct[Condition, z:("\"" ~~ Shortest[___] ~~ "\""), Uneval @ notPartialStrQ[z]]];

notPartialStrQ[s_] := EvenQ @ StringLength @ First[StringCases[s, "\\".. ~~ "\"" ~~ EndOfString], ""];

(* this matches up anything up to a top-level comma *)
makeUniqueWLSpan[] := Module[{z, q, rep, repIn},
  rep = Riffled[PatternRecurse @ z, ","];
  repIn = Function[#1 ~~ rep ~~ #2];
  Nullspace ~~ z:SExpr[
    Alt[
      "{}", "<||>", "[]", WLSymbolSpan,
      "(" ~~ PatternRecurse[z] ~~ ")",
      Construct[Condition, q:("\"" ~~ ShortBlank ~~ "\""), Uneval @ notPartialStrQ @ q],
      repIn["[", "]"], repIn["{", "}"], repIn["<|", "|>"], repIn["\[LeftAssociation]", "\[RightAssociation]"],
      ExceptLetterClass[WLDelimiterLetter].. (* jumpahead *)
    ],
    Maybe @ PatternRecurse[z]
  ] ~~ Nullspace
];

(* TODO: make a delimiter class *)
assocStrFreeQ[q_] := StringFreeQ[q, {"<|", "|>", "\[LeftAssociation]", "\[RightAssociation]"}]
