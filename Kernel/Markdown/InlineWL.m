PrivateSymbol[$inlineWLExprReplacement, $inlineWLExprReplacement2, $inlineWLVarReplacement]

ToMarkdownString::inlinewlbox = "Inline WL \"``\" did not generate valid KateX.";
ToMarkdownString::inlinewlsym = "Inline WL \"``\" generated novel symbol(s): ``.";

$inlineWLExprReplacement = "$[" ~~ s:Shortest[Repeated[Except["\n"]]] ~~ "]$" :> createInlineMath[s];
$inlineWLExprReplacement2 = "((" ~~ s:Shortest[Repeated[Except["\n"]]] ~~ "))" :> createInlineMath[s];
$inlineWLVarReplacement = s:("$" ~~ WordCharacter ~~ RepeatedNull[WordCharacter | DigitCharacter | "$"] ~~ WordBoundary) :> createInlineMath[s];
(*
TODO: ue this instead: RegularExpression @ "(\\(\\([^\n]+\\)\\))|(\\$[[:alpha:]][[:alpha:][:digit:]$]*\\b)";
*)

createInlineMath[str_String] := Scope[
  res = toInlineExpression[str, InputForm];
  If[FailureQ[res], res = badInlinePlaceholder[str]];

  katex = $katexPostprocessor @ boxesToKatexString @ ToBoxes[res, StandardForm];
  If[!StringQ[katex], Message[ToMarkdownString::inlinewlbox, str]; Return["BAD KATEX"]];
  $inlineMathTemplate @ katex
];

badInlinePlaceholder[str_String] := RedForm @ PlainTextForm @ str;

ToMarkdownString::inlinewlsyn = "Inline WL \"``\" was not valid syntax.";
ToMarkdownString::inlinewlmsg = "Inline WL \"``\" generated messages while evaluating.";

PrivateFunction[toInlineExpression]

checkedToExpression[str_, form_] :=
  Quiet @ Check[ToExpression[str, form, Hold], $Failed];

(* tuples, very common *)
checkedToExpression[RowBox[{"(", RowBox[list:{Repeated[PatternSequence[_, ","]], _}], ")"}], StandardForm] :=
  Replace[
    Construct[
      Hold,
      TupleForm @@ Map[checkedToExpression[#, StandardForm]&, Part[list, 1;;-1;;2]]
    ],
    {Hold[h_] :> h},
    {2}
  ];

SetHoldComplete[singleScratchSymbolQ]

singleScratchSymbolQ[s_Symbol] := Context[Unevaluated @ s] === "QuiverGeometryPackageLoader`Scratch`" && StringLength[SymbolName[Unevaluated @ s]] === 1;
singleScratchSymbolQ[_] := False;

toInlineExpression[str_, form_] := Block[
  {$Context = "QuiverGeometryPackageLoader`Scratch`",
   $ContextPath = {"System`", "Global`", "QuiverGeometry`", "QuiverGeometry`Shortcuts`"},
   result, scratchNames, held, eval},
  held = checkedToExpression[str, form] /. (s_Symbol ? singleScratchSymbolQ) :> SymbolForm[SymbolName[s]];
  If[FailureQ @ held,
    Message[ToMarkdownString::inlinewlsyn, str];
    Return @ $Failed;
  ];
  (* single-letter symbols are allowed, and evaluate to SymbolForm[...] *)
  scratchNames = Select[Names["QuiverGeometryPackageLoader`Scratch`*"], StringLength[#] > 37&];
  If[scratchNames =!= {},
    Message[ToMarkdownString::inlinewlsym, str, scratchNames];
    Quiet @ Remove[scratchNames];
    Return @ $Failed;
  ];
  held //= Replace[Hold[Times[a___]] :> Hold[CommaRowForm[a]]];
  held //= ReplaceAll[Set -> EqualForm];
  eval = Quiet @ Check[First @ held, $Failed];
  If[FailureQ @ eval,
    Message[ToMarkdownString::inlinewlmsg, str];
    Return @ $Failed;
  ];
  eval
];

