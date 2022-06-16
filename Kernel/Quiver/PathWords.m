PackageExport["ValidPathWordQ"]

SetUsage @ "
ValidPathWordQ[word$] returns True if word$ is a list of cardinals.
ValidPathWordQ[word$, cards$] returns True if the cardinals in word are a subset of cards$.
* Inverted cardinal can be present in the form Inverted[cardinal$].
"

ValidPathWordQ[word_] := ValidPathWordQ[word, Automatic];

ValidPathWordQ[word_List, Automatic] := True;
ValidPathWordQ[word_List, cards_] := SubsetQ[cards, StripInverted /@ word];
ValidPathWordQ[_, _] := False;

(********************************************)

PackageExport["ToPathWord"]

SetUsage @ "
ToPathWord['word$'] interprets the characters of 'word$' as cardinals and returns a list of them.
ToPathWord['word$', cards$] only allows the cardinals in card$ (or their inversions).
ToPathWord['word$', cards$, default$] evaluates and returns default$ is the path is not valid.
ToPathWord[list$, cards$, $$] checks that a list of cardinals is a subset of cards$.
* If a letter 'c$' is uppercased, it is interpreted as Inverted['c$'].
* By default, $Failed is returned if the path is not valid.
"

SetHoldRest[ToPathWord];

PackageScope["$pathCancellation"]
$pathCancellation = True;

ToPathWord["" | {}, ___] = {};

ToPathWord[word_, validCardinals_, else_] :=
  OnFailed[
    ToPathWord[word, validCardinals],
    else
  ];

ToPathWord[word_, validCardinals_:Automatic] := Scope[
  $validCardinals = validCardinals;
  cardinals = toCardinalList @ word;
  Which[
    !ValidPathWordQ[cardinals, validCardinals],
      $Failed,
    $pathCancellation,
      cardinals //. $backtrackingRules,
    True,
      cardinals
  ]
];

$backtrackingRules = Dispatch @ {
  {l___, i_, Inverted[i_], r___} :> {l, r},
  {l___, Inverted[i_], i_, r___} :> {l, r}
};

toCardinalList = Case[
  list_List           := list;
  n:Inverted[_String]  := {n};
  str_String          := Map[toCardinal, Characters @ str];
  _                   := $Failed;
];

toCardinal = Case[
  s_ /; MemberQ[$validCardinals, s] := s;
  s_ ? UpperCaseQ := Inverted @ ToLowerCase @ s;
  s_ := s
];
