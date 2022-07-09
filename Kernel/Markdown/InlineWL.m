PrivateSymbol[$inlineWLExprReplacement, $inlineWLVarReplacement]

ToMarkdownString::inlinewlsyn = "Inline WL \"``\" was not valid syntax.";
ToMarkdownString::inlinewlbox = "Inline WL \"``\" did not generate valid KateX.";
ToMarkdownString::inlinewlsym = "Inline WL \"``\" generated novel symbol.";

$inlineWLExprReplacement = "$[" ~~ s:Shortest[Repeated[Except["\n"]]] ~~ "]$" :> createInlineMath[s];
$inlineWLVarReplacement = s:("$" ~~ WordCharacter ~~ RepeatedNull[WordCharacter | DigitCharacter | "$"] ~~ WordBoundary) :> createInlineMath[s];

createInlineMath[str_String] := Scope[
  res = toInlineExpression[str];
  If[FailureQ[res], Return["BAD MATH"]];
  katex = $katexPostprocessor @ boxesToKatexString @ MakeBoxes[res, StandardForm];
  If[!StringQ[katex], Message[ToMarkdownString::inlinewlbox, str]; Return["BAD KATEX"]];
  $inlineMathTemplate @ katex
];

toInlineExpression[s_String] := Block[
  {$Context = "QuiverGeometryPackageLoader`Scratch`",
   $ContextPath = {"System`", "Global`", "QuiverGeometry`", "QuiverGeometry`Shortcuts`"},
   result},
  result = Quiet @ Check[ToExpression[s, InputForm], $Failed];
  If[FailureQ @ result,
    Message[ToMarkdownString::badinlinemath, str];
  ];
  If[Names["QuiverGeometryPackageLoader`Scratch`*"] =!= {},
    Message[ToMarkdownString::inlinewlsym, s];
    result = $Failed;
    Quiet @ Remove["QuiverGeometryPackageLoader`Scratch`*"];
  ];
  result
];