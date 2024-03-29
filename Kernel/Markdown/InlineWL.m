PrivateSymbol[$inlineWLExprReplacement, $inlineWLExprReplacement2, $inlineWLVarReplacement]

ToMarkdownString::inlinewlbox = "Inline WL \"``\" did not generate valid KateX.";
ToMarkdownString::inlinewlsym = "Inline WL \"``\" generated novel symbol(s): ``.";

$inlineWLExprReplacement = "$[" ~~ s:Shortest[Repeated[Except["\n"]]] ~~ "]$" :> createInlineMath[s];
$inlineWLExprReplacement2 = "((" ~~ s:Shortest[Repeated[Except["\n"]]] ~~ "))" :> createInlineMath[s];
$inlineWLVarReplacement = s:("$" ~~ WordCharacter ~~ RepeatedNull[WordCharacter | DigitCharacter | "$"] ~~ WordBoundary) :> createInlineMath[s];
(*
TODO: ue this instead: RegularExpression @ "(\\(\\([^\n]+\\)\\))|(\\$[[:alpha:]][[:alpha:][:digit:]$]*\\b)";
*)

createInlineMath[str_Str /; SLen[str] == 2] :=
  $katexFontTemplate @ SDrop[str, 1];

createInlineMath[str_Str] := Scope[
  res = toInlineExpression[str, InputForm];
  If[FailureQ[res], res = badInlinePlaceholder[str]];
  boxes = ToBoxes[res, TraditionalForm];
  katex = $katexPostprocessor @ boxesToKatexString @ boxes;
  If[!StrQ[katex], Message[ToMarkdownString::inlinewlbox, str]; Return["BAD KATEX"]];
  $inlineMathTemplate @ katex
];

badInlinePlaceholder[str_Str] := RedForm @ PlainTextForm @ str;

ToMarkdownString::inlinewlsyn = "Inline WL \"``\" was not valid syntax.";
ToMarkdownString::inlinewlmsg = "Inline WL \"``\" generated messages while evaluating.";

(**************************************************************************************************)

PrivateFunction[toInlineExpression]

checkedToExpression[str_, form_] :=
  Quiet @ Check[ToExpression[str, form, Hold], $Failed];

(* tuples, very common *)
checkedToExpression[RowBox[{"(", RowBox[list:{Repeated[PatternSequence[_, ","]], _}], ")"}], StandardForm] :=
  Rep[
    Construct[
      Hold,
      TupleForm @@ Map[checkedToExpression[#, StandardForm]&, Part[list, 1;;-1;;2]]
    ],
    {Hold[h_] :> h},
    {2}
  ];

SetHoldComplete[singleScratchSymbolQ]

singleScratchSymbolQ[s_Symbol] := Context[Uneval @ s] === "MTLoader`Scratch`" && SLen[SymbolName[Uneval @ s]] === 1;
singleScratchSymbolQ[_] := False;

toInlineExpression[str_, form_] := Block[
  {$Context = "MTLoader`Scratch`",
   $ContextPath = {"System`", "Global`", "MathTools`", "MathToolsShortcuts`"},
   result, scratchNames, held, eval},
  held = checkedToExpression[str, form] /. (s_Symbol ? singleScratchSymbolQ) :> SymbolForm[SymbolName[s]];
  If[FailureQ @ held,
    Message[ToMarkdownString::inlinewlsyn, str];
    Return @ $Failed;
  ];
  (* single-letter symbols are allowed, and evaluate to SymbolForm[...] *)
  scratchNames = Select[Names["MTLoader`Scratch`*"], SLen[#] > 37&];
  If[scratchNames =!= {},
    Message[ToMarkdownString::inlinewlsym, str, scratchNames];
    Quiet @ Remove[scratchNames];
    Return @ $Failed;
  ];
  held //= Rep[Hold[Times[a___]] :> Hold[CommaRowForm[a]]];
  held //= RepAll[Set -> EqualForm];
  eval = Quiet @ Check[F @ held, $Failed];
  If[FailureQ @ eval,
    Message[ToMarkdownString::inlinewlmsg, str];
    Return @ $Failed;
  ];
  eval
];

(**************************************************************************************************)

PrivateFunction[inlineCellToMarkdown]

inlineCellToMarkdown[boxes_, inline_] := Scope[
  evalBoxes = tryEvalBoxes @ cleanupInlineBoxes @ boxes;
  Switch[evalBoxes,
    _GraphicsBox,
      complainBoxes[evalBoxes],

    (* TODO: support inline images *)
    TemplateBox[{_, _}, "StringBlockForm"],
      (* TODO: introduce $inlineHTMLCodeTemplate for this *)

      If[inline, "<code>", "<pre>"] <> Part[evalBoxes, 1, 2] <> If[inline, "</code>", "</pre>"],

    _,
      If[inline, $inlineMathTemplate, $multilineMathTemplate] @ toProcessedKatexString @ evalBoxes
  ]
];

$infixBoxP = "@" | "/@" | "@@" | "@@@" | "//" | "+" | "-" | "^" | "/" | ">" | "<" | "<=" | "\[LessEqual]" | ">=" | "\[GreaterEqual]" | "==" | "===" | "\[Equal]";
$unevalBoxP = RowBox[{_, "[", _, "]"}] | RowBox[{_, $infixBoxP, _}] | RowBox[{_, $infixBoxP, _, $infixBoxP, _}] | RowBox[{"(", _, ")"}];

tryEvalBoxes = Case[
  boxes : $unevalBoxP := ToBoxes[cleanupInlineBoxes @ Check[toInlineExpression[boxes, StandardForm], $Failed], StandardForm];
  boxes_ := boxes;
];
