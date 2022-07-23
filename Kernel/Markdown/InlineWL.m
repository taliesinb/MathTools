PrivateSymbol[$inlineWLExprReplacement, $inlineWLExprReplacement2, $inlineWLVarReplacement]

ToMarkdownString::inlinewlbox = "Inline WL \"``\" did not generate valid KateX.";
ToMarkdownString::inlinewlsym = "Inline WL \"``\" generated novel symbol.";

$inlineWLExprReplacement = "$[" ~~ s:Shortest[Repeated[Except["\n"]]] ~~ "]$" :> createInlineMath[s];
$inlineWLExprReplacement2 = "((" ~~ s:Shortest[Repeated[Except["\n"]]] ~~ "))" :> createInlineMath[s];
$inlineWLVarReplacement = s:("$" ~~ WordCharacter ~~ RepeatedNull[WordCharacter | DigitCharacter | "$"] ~~ WordBoundary) :> createInlineMath[s];
(*
TODO: ue this instead: RegularExpression @ "(\\(\\([^\n]+\\)\\))|(\\$[[:alpha:]][[:alpha:][:digit:]$]*\\b)";
*)

createInlineMath[str_String] := Scope[
  res = toInlineExpression[str];
  If[FailureQ[res], res = badInlinePlaceholder[str]];

  katex = $katexPostprocessor @ boxesToKatexString @ ToBoxes[res, StandardForm];
  If[!StringQ[katex], Message[ToMarkdownString::inlinewlbox, str]; Return["BAD KATEX"]];
  $inlineMathTemplate @ katex
];

badInlinePlaceholder[str_String] := RedForm @ PlainTextForm @ str;

ToMarkdownString::inlinewlsyn = "Inline WL \"``\" was not valid syntax.";
ToMarkdownString::inlinewlmsg = "Inline WL \"``\" generated messages while evaluating.";

PrivateFunction[toInlineExpression]

toInlineExpression[str_String] := Block[
  {$Context = "QuiverGeometryPackageLoader`Scratch`",
   $ContextPath = {"System`", "Global`", "QuiverGeometry`", "QuiverGeometry`Shortcuts`"},
   result},
  held = Quiet @ Check[ToExpression[str, InputForm, Hold], $Failed];
  If[FailureQ @ held,
    Message[ToMarkdownString::inlinewlsyn, str];
    Return @ $Failed;
  ];
  If[Names["QuiverGeometryPackageLoader`Scratch`*"] =!= {},
    Message[ToMarkdownString::inlinewlsym, str];
    Quiet @ Remove["QuiverGeometryPackageLoader`Scratch`*"];
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

