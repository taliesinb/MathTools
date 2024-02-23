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

(**************************************************************************************************)

PublicStringPattern[WLWholeWord]

SetUsage @ "
WLWholeWord[p$] matches p$, but prevents matches that are part of a larger symbol name on the left or right.
* the left and right side of p$ are examined to determine which parts need negative look ahead/behind conditions.
";

DefineStringPatternMacro[
  WLWholeWord[p_] :> toWLWholeWordPattern[p]
];

toWLWholeWordPattern[p_] := Scope[
  $isL = True;  p //= applyBound; p //= RepRep[$bubbleLBoundRules];
  $isL = False; p //= applyBound; p //= RepRep[$bubbleRBoundRules];
  p //= RepRep[$subLRBoundRules];
  p
];

(* TODO: handle Maybe / Repeated *)
applyBound = Case[
  l_$lbound       := Map[%, l]; (* happens after we've applied l and we are doing r *)
  a_Alt           := Map[%, a];
  l_List          := Map[%, l];
  r:
  s_SExpr         := Apply[SExpr] @ If[$isL, boundSeqL, boundSeqR] @ Apply[List] @ s;
  l_LetterClass   := If[Or @@ StringMatchQ[$boundTestChars, l], l, If[$isL, $lbound, $rbound] @ l];
  p:recurse1      := MapAt[%, p, 1];
  p:recurse2      := MapAt[%, p, 2];
  p_              := Which[
    StringPatternCanStartQ[p, $boundTestStr];
     $isL && !StringPatternCanStartQ[p, $boundTestStr], $lbound @ p,
    !$isL && !StringPatternCanEndQ  [p, $boundTestStr], $rbound @ p,
    True, p
  ];
  s_Str           := Which[
     $isL && SStartsQ[s, WLSymbolLetter], $lbound @ s,
    !$isL &&   SEndsQ[s, WLSymbolLetter], $rbound @ s,
    True, s
  ];
,
  {recurse1 -> _Longest | _Shortest | _Condition | _PatternTest, recurse2 -> _Pattern}
];

(* newlines ensure that its a whole word *)
boundSeqL[s:List[StartOfLine | StartOfString | WLSymbolBoundary, ___]] := s;
boundSeqL[List[l_, r___]] := List[applyBound @ l, r];

boundSeqR[s:List[___, EndOfLine | EndOfString | WLSymbolBoundary]] := s;
boundSeqR[List[l___, r_]] := List[l, applyBound @ r];

(* if a mystery string pattern can match these characters on the given side, it is ineligible for
being wrapped on that side. *)
$boundTestStr = " \t\n+-*/()[]{}!@#%^&";
$boundTestChars = Chars @ $boundTestStr;

$bubbleLBoundRules = {
  a:(Alt|List)[__$lbound] :> $lbound[Col1 @ a]
};

$bubbleRBoundRules = {
  a:(Alt|List)[__$rbound] :> $rbound[Col1 @ a],
  SExpr[$lbound[f_], r___]   :> $lbound[SExpr[f, r]]
};

$subLRBoundRules = {
  $lbound[$rbound[e_]] :> NegativeLookbehind[WLSymbolLetter] ~~ e ~~ NegativeLookahead[WLSymbolLetter],
  $lbound[e_]          :> NegativeLookbehind[WLSymbolLetter] ~~ e,
  $rbound[e_]          :> e ~~ NegativeLookahead[WLSymbolLetter]
};