$slotRegularExpression = RegularExpression["<\\*([^*]+)\\*>"];

substituteUsageSlots[s_String] :=
  StringReplace[s, "<*" ~~ Shortest[slot___] ~~ "*>" :> Block[
    {$ContextPath = {"System`", "QuiverGeometry`", "QuiverGeometry`PackageScope`"}},
    toUsageStr[ToExpression[slot, InputForm]]
  ]];

toUsageStr[list:{__String}] := commaString[list];
toUsageStr[e_] := TextString[e];

(**************************************************************************************************)

$literalStringRegex = RegularExpression["'[A-Z][a-zA-Z0-9]+'"];
$literalStringColor = RGBColor[{0.4, 0.4, 0.4}];

PrivateVariable[$literalSymbolRegex]

$literalSymbolStr = "\\$Failed Automatic True False None Inherited Left Right Above Below Center \
Top Bottom Infinity Tiny Small Medium Large Inverted Into";
$literalSymbolRegex = RegularExpression["(" <> StringReplace[$literalSymbolStr, " " -> "|"] <> ")"];
$literalSymbolColor = RGBColor[{0.15, 0.15, 0.15}];

PrivateVariable[$mainSymbolRegex, $currentMainSymbol]

$mainSymbolRegex = RegularExpression["^\\$?[A-Za-z][A-Za-z]*"];
$mainSymbolColor = RGBColor[{0.71, 0.03, 0.}];

colorLiterals[usageString_] := Scope[
  usageString //= StringTrim;
  StringReplace[
    usageString, {
      string:$literalStringRegex :> makeStyleBox[
        "\\\"" <> StringTake[string, {2, -2}] <> "\\\"",
        FontColor -> $literalStringColor, ShowStringCharacters -> True,
        FontWeight -> "Medium"],
      WordBoundary ~~ literal:$literalSymbolRegex ~~ WordBoundary :> makeLiteralSymbolBox[literal]
    }
  ]
];

colorMainSymbol[usageString_] := StringReplace[
  usageString, {
  ("\"" ~~ $currentMainSymbol ~~ "\"") :> StringTake[makeMainSymbolInlineSyntax[], {4, -2}],
  WordBoundary ~~ $currentMainSymbol ~~ WordBoundary :> makeMainSymbolInlineSyntax[],
  "<|" -> "\[LeftAssociation]",
  "|>" -> "\[RightAssociation]",
  "-StyleBox[\"" -> "StyleBox[\"-"
}];

makeMainSymbolInlineSyntax[] := makeStyleBox[$currentMainSymbol,
  FontColor -> $mainSymbolColor,
  FontWeight -> "Medium"
];

(**************************************************************************************************)

$otherSymbolColor = RGBColor[{0.086, 0.367, 0.615}];

colorOtherSymbols[usageString_] := StringReplace[
  usageString, {
    "%%" ~~ w:WordCharacter.. :> makeLiteralSymbolBox[w],
    "%" ~~ w:WordCharacter.. :> makeOtherSymbolBox[w]
  }
];

makeLiteralSymbolBox[w_] := makeStyleBox[w, FontColor -> $literalSymbolColor, FontWeight -> "Medium"];
makeOtherSymbolBox[w_] := makeStyleBox[w, FontColor -> $otherSymbolColor, FontWeight -> "Medium"];

makeStyleBox[str_, opts___] := StringJoin[
  "\!\(\*StyleBox[\"", str, "\", ", StringTake[ToString[{opts}, InputForm], {2, -2}], "]\)"
];

(**************************************************************************************************)

$headerLineRegex = RegularExpression["(?m)^## ([^\n]*)$"];

addHeaderLines[usageString_] := StringReplace[
  usageString, {
    $headerLineRegex :>
      addInlinePane @ makeStyleBox["$1", FontWeight -> "Bold"],
    "\n\n" :>
      "\!\(\*PaneBox[\"\", FrameMargins -> {{0,0}, {5, 0}}]\)\n"
  }
];

addInlinePane[str_String] := StringJoin[
  "\!\(\*PaneBox[",
  StringTake[str, {4, -3}],
  "], FrameMargins -> {{0, 0}, {1, 4}}]\)"
];

(**************************************************************************************************)

$gridBoxL = "\(\*TagBox[GridBox[";
$gridBoxR = ", \"Grid\"]\)";
$shorterGridBoxL = "\(\*PaneBox[GridBox[";
$shorterGridBoxR = ", Rule[ImageMargins, List[List[0,0], List[-5,-5]]], Rule[FrameMargins, List[List[0,0], List[-3,-6]]]]\)";
$shorterGridBoxR = ", Rule[ImageMargins, List[List[0,0], List[$BOT, $TOP]]]]\)";

shortenGridBoxes[usageString_] := StringReplace[
  usageString,
  $gridBoxL ~~ Shortest[content__] ~~ $gridBoxR :> Block[{offset, bot, top},
    offset = StringCount[content, "{\""];
    bot = TextString @ Round[ - 0.5*offset];
    top = TextString @ Round[ - 0.5*offset];
    $shorterGridBoxL <> content <> StringReplace[$shorterGridBoxR, {"$BOT" -> bot, "$TOP" -> top}]
  ]
];

(**************************************************************************************************)

$fmtUsageOuter = True;

PrivateFunction[ClearUsageCache]

ClearUsageCache[] := (
  Clear[GeneralUtilities`Private`$SetUsageFormatCache];
  $RawUsageStringTable = Association[];
  GeneralUtilities`Code`PackagePrivate`$relatedSymbolTable = Data`UnorderedAssociation[];
);

(* this speeds up the processing of usage string messages, which are otherwise quite expensive *)
If[!AssociationQ[GeneralUtilities`Private`$SetUsageFormatCache],
  GeneralUtilities`Private`$SetUsageFormatCache = Data`UnorderedAssociation[];
  GeneralUtilities`Code`PackagePrivate`fmtUsageString[str_String] /; $fmtUsageOuter := Block[
    {$fmtUsageOuter = False},
    storeRawUsageString[str];
    GeneralUtilities`CacheTo[
      GeneralUtilities`Private`$SetUsageFormatCache, Hash[str],
      Compose[customSetUsageProcessor, str]
    ]
  ];
  With[{arn := GeneralUtilities`Code`PackagePrivate`appendRelatedNote},
    If[FreeQ[DownValues[arn], makeOtherSymbolBox],
      DownValues[arn] = ReplaceAll[
        DownValues[arn], HoldPattern[Riffle[z_, ", "]] :> Riffle[makeOtherSymbolBox /@ z, ", "]
      ]
    ];
  ];
];

customSetUsageProcessor = Composition[
  colorMainSymbol,
  addHeaderLines, shortenGridBoxes,
  GeneralUtilities`Code`PackagePrivate`fmtUsageString,
  colorOtherSymbols, colorLiterals
];

(**************************************************************************************************)

PrivateVariable[$RawUsageStringTable]

$RawUsageStringTable = Association[];

storeRawUsageString[rawUsageString_String] := Block[
  {usageString = StringTrim @ rawUsageString},
  (* $currentMainSymbol will be picked up later in the customSetUsageProcessor composition chain *)
  $currentMainSymbol = First @ StringCases[usageString, $mainSymbolRegex, 1];
  $RawUsageStringTable[$currentMainSymbol] = usageString;
];

(**************************************************************************************************)

(* the default behavior of System`InformationDump` will introduce LineSpacing that messes up
my SetUsage inline tables, so remove it. *)

dummy::usage = "Dummy";
ToBoxes[Information[dummy]];
System`InformationDump`subtitleStyled[sub_] := Style[sub, "InformationUsageText"];

(**************************************************************************************************)

PrivateFunction[SetInitialValue]

SetHoldAllComplete[SetInitialValue];

SetInitialValue[sym_Symbol, body_] := If[!System`Private`HasImmediateValueQ[sym], sym = body];

(**************************************************************************************************)

PrivateFunction[SetUsage]

PublicVariable[$DisableSetUsage]

SetInitialValue[$DisableSetUsage, False];

preprocessUsageString[usageString_] :=
  FixedPoint[substituteUsageSlots, usageString]

SetUsage[___] /; $DisableSetUsage := Null;

SetUsage[usageString_String] :=
  GeneralUtilities`SetUsage[Evaluate @ preprocessUsageString @ usageString];

SetUsage[symbol_Symbol, usageString_String] :=
  GeneralUtilities`SetUsage[symbol, Evaluate @ preprocessUsageString @ usageString];

