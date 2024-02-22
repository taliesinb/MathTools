PublicSpecialVariable[$PosixQ, $WindowsQ, $MacOSQ]

$MacOSQ =    $OperatingSystem === "MacOSX";
$PosixQ =    $OperatingSystem =!= "Windows";
$WindowsQ =  $OperatingSystem === "Windows";

(**************************************************************************************************)

PrivateFunction[LocalPath, DataPath]

LocalPath[args___Str] := FileNameJoin @ List[$PackageDirectory, args];
e_LocalPath := (Message[LocalPath::badarguments, MsgExpr @ Uneval @ e]; $Failed)

DataPath[dir_Str, args___Str] := FileNameJoin[{ensureDataDir @ dir, args}];
e_DataPath := (Message[DataPath::badarguments, MsgExpr @ Uneval @ e]; $Failed)

$dataDir = FileNameJoin[{$PackageDirectory, "Data"}];

ensureDataDir[dir_] := Module[
  {fullDir = FileNameJoin[{$dataDir, dir}]},
  If[!FileExistsQ[fullDir], CreateDirectory[fullDir]];
  ensureDataDir[dir] = fullDir
];

(**************************************************************************************************)

If[!$Notebooks,
  (* this is so that e.g. Text inside graphics will still have FormBox[..., TraditionalForm] wrapped around it! *)
  System`Dump`$textFormatType = TraditionalForm;
]

(**************************************************************************************************)

$slotRegularExpression = RegularExpression["<\\*([^*]+)\\*>"];

substituteUsageSlots[s_Str] :=
  SRep[s, "<*" ~~ Shortest[slot___] ~~ "*>" :> Block[
    {$ContextPath = {"System`", "QuiverGeometry`", "QuiverGeometry`Private`"}},
    toUsageStr[ToExpression[slot, InputForm]]
  ]];

toUsageStr[list:{__Str}] := commaString[list];
toUsageStr[e_] := TextString[e];

(**************************************************************************************************)

PrivateSpecialFunction[IfSynaxInfo]

SetHoldFirst[IfSynaxInfo];
IfSynaxInfo[body_] := If[
  $Notebooks && !TrueQ[QuiverGeometryLoader`$DisableSyntaxInformation] && !TrueQ[QuiverGeometryLoader`$FastLoad],
  body
];

(**************************************************************************************************)

$literalStringRegex = RegularExpression["'[A-Z][a-zA-Z0-9]+'"];
$literalStringColor = RGBColor[{0.4, 0.4, 0.4}];

PrivateVariable[$literalSymbolRegex]

$literalSymbolStr = "\\$Failed Automatic True False None Inherited Left Right Above Below Center \
Top Bottom Infinity Tiny Small Medium Large Inverted Into";

$literalSymbolRegex = RegularExpression["(" <> SRep[$literalSymbolStr, " " -> "|"] <> ")"];
$literalSymbolColor = RGBColor[{0.15, 0.15, 0.15}];

PrivateVariable[$mainSymbolRegex, $currentMainSymbol]

$mainSymbolRegex = RegularExpression["^\\$?[A-Za-z][A-Za-z0-9]*"];
$mainSymbolColor = RGBColor[{0.71, 0.03, 0.}];

colorLiterals[usageString_] := Scope[
  usageString = STrim[usageString];
  SRep[
    usageString, {
      string:$literalStringRegex :> makeStyleBox[
        "\\\"" <> STake[string, {2, -2}] <> "\\\"",
        FontColor -> $literalStringColor, ShowStringCharacters -> True,
        FontWeight -> "Medium"],
      WordBoundary ~~ literal:$literalSymbolRegex ~~ WordBoundary :> makeLiteralSymbolBox[literal]
    }
  ]
];

colorMainSymbol[usageString_] := If[$currentMainSymbol === None, usageString, SRep[
  usageString, {
  ("\"" ~~ $currentMainSymbol ~~ "\"") :> STake[makeMainSymbolInlineSyntax[], {4, -2}],
  WordBoundary ~~ $currentMainSymbol ~~ WordBoundary :> makeMainSymbolInlineSyntax[],
  "<|" -> "\[LeftAssociation]",
  "|>" -> "\[RightAssociation]",
  "-StyleBox[\"" -> "StyleBox[\"-"
}]];

makeMainSymbolInlineSyntax[] := makeStyleBox[$currentMainSymbol,
  FontColor -> $mainSymbolColor,
  FontWeight -> "Medium"
];

(**************************************************************************************************)

$otherSymbolColor = RGBColor[{0.086, 0.367, 0.615}];

colorOtherSymbols[usageString_] := SRep[
  usageString, {
    "%%" ~~ w:WordCharacter.. :> makeLiteralSymbolBox[w],
    "%" ~~ w:WordCharacter.. :> makeOtherSymbolBox[w]
  }
];

makeLiteralSymbolBox[w_] := makeStyleBox[w, FontColor -> $literalSymbolColor, FontWeight -> "Medium"];
makeOtherSymbolBox[w_] := makeStyleBox[w, FontColor -> $otherSymbolColor, FontWeight -> "Medium"];

makeStyleBox[str_, opts___] := SJoin[
  "\!\(\*StyleBox[\"", str, "\", ", STake[ToString[{opts}, InputForm], {2, -2}], "]\)"
];

(**************************************************************************************************)

$headerLineRegex = RegularExpression["(?m)^## ([^\n]*)$"];

addHeaderLines[usageString_] := SRep[
  usageString, {
    $headerLineRegex :>
      addInlinePane @ makeStyleBox["$1", FontWeight -> "Bold"],
    "\n\n" :>
      "\!\(\*PaneBox[\"\", FrameMargins -> {{0,0}, {5, 0}}]\)\n"
  }
];

addInlinePane[str_Str] := SJoin[
  "\!\(\*PaneBox[",
  STake[str, {4, -3}],
  "], FrameMargins -> {{0, 0}, {1, 4}}]\)"
];

(**************************************************************************************************)

replaceEllipsis[str_Str] := SRep[str, "\[Ellipsis]" -> "..."];

(**************************************************************************************************)

$gridBoxL = "\(\*TagBox[GridBox[";
$gridBoxR = ", \"Grid\"]\)";
$shorterGridBoxL = "\(\*PaneBox[GridBox[";
$shorterGridBoxR = ", Rule[ImageMargins, List[List[0,0], List[-5,-5]]], Rule[FrameMargins, List[List[0,0], List[-3,-6]]]]\)";
$shorterGridBoxR = ", Rule[ImageMargins, List[List[0,0], List[$BOT, $TOP]]]]\)";

shortenGridBoxes[usageString_] := SRep[
  usageString,
  $gridBoxL ~~ Shortest[content__] ~~ $gridBoxR :> Block[{offset, bot, top},
    offset = StringCount[content, "{\""];
    bot = TextString @ Round[ - 0.5*offset];
    top = TextString @ Round[ - 0.5*offset];
    $shorterGridBoxL <> content <> SRep[$shorterGridBoxR, {"$BOT" -> bot, "$TOP" -> top}]
  ]
];

(**************************************************************************************************)

$fmtUsageOuter = True;

PrivateFunction[ClearUsageCache]

ClearUsageCache[] := (
  Clear[GeneralUtilities`Private`$SetUsageFormatCache];
  $RawUsageStringTable = Assoc[];
  GeneralUtilities`Code`PackagePrivate`$relatedSymbolTable = UAssoc[];
);

(* this speeds up the processing of usage string messages, which are otherwise quite expensive *)
If[!AssocQ[GeneralUtilities`Private`$SetUsageFormatCache],
  GeneralUtilities`Private`$SetUsageFormatCache = UAssoc[];
  GeneralUtilities`Code`PackagePrivate`fmtUsageString[str_Str] /; $fmtUsageOuter := Block[
    {$fmtUsageOuter = False},
    storeRawUsageString[str];
    GeneralUtilities`CacheTo[
      GeneralUtilities`Private`$SetUsageFormatCache, Hash[str],
      Compose[customSetUsageProcessor, str]
    ]
  ];
  With[{arn := GeneralUtilities`Code`PackagePrivate`appendRelatedNote},
    If[FreeQ[DownValues[arn], makeOtherSymbolBox],
      DownValues[arn] = RepAll[
        DownValues[arn], HoldP[Riffle[z_, ", "]] :> Riffle[makeOtherSymbolBox /@ z, ", "]
      ]
    ];
  ];
];

customSetUsageProcessor = Composition[
  replaceEllipsis, colorMainSymbol,
  addHeaderLines, shortenGridBoxes,
  GeneralUtilities`Code`PackagePrivate`fmtUsageString,
  colorOtherSymbols, colorLiterals
];

(**************************************************************************************************)

(* this allows % to be used in LHS of SetUsage lines *)

encodeUppercase[i_] := FromCharCode[65 + IntDigits[i, 26, 2]];
decodeUppercase[s_] := FromDigits[ToCharCode[s] - 65, 26];
GeneralUtilities`Code`PackagePrivate`linearLHS[str_Str, Optional[escapeq_, False]] /; SContainsQ[str, "\!\(\*"] := Module[
  {blocks = {}, i = 0, str2, res},
  If[SStartsQ[str, "\!\(\*"] && SEndsQ[str, "\)"], Return @ str];
  str2 = SRep[str, "\!\(\*" ~~ Shortest[content__] ~~ "\)" :> (
    AppTo[blocks, content];
    "AZA" <> encodeUppercase[++i]
  )];
  res = GeneralUtilities`Code`PackagePrivate`linearLHS[str2, escapeq];
  SRep[res, {
    "\"AZA" ~~ d:(LetterCharacter ~~ LetterCharacter) ~~ "\"" :> Part[blocks, decodeUppercase[d]],
    "AZA" ~~ d:(LetterCharacter ~~ LetterCharacter) :> Part[blocks, decodeUppercase[d]]
  }]
]

(**************************************************************************************************)

PrivateVariable[$RawUsageStringTable]

$RawUsageStringTable = Assoc[];

storeRawUsageString[rawUsageString_Str] := Block[
  {usageString = STrim @ rawUsageString},
  (* $currentMainSymbol will be picked up later in the customSetUsageProcessor composition chain *)
  $currentMainSymbol = F[SCases[usageString, $mainSymbolRegex, 1], None];
  $RawUsageStringTable[$currentMainSymbol] = usageString;
];

(**************************************************************************************************)

PrivateMutatingFunction[AppendUniqueTo]

SetHoldFirst[AppendUniqueTo];
AppendUniqueTo[var_, value_] := If[!MemberQ[var, value], AppTo[var, value]];

(**************************************************************************************************)

PrivateMutatingFunction[SetInitialValue]

SetHoldAllComplete[SetInitialValue, SetDelayedInitialValue];

SetInitialValue[sym_Symbol, other__Symbol, body_] := (SetInitialValue[sym, body]; SetInitialValue[other, body]);

SetInitialValue[sym_Symbol, body_] := If[
  !HasImmediateValueQ[sym],
  QuiverGeometryLoader`DeclarePreservedVariable[sym];
  Set[sym, body]
];

(**************************************************************************************************)

PrivateMutatingFunction[SetDelayedInitialValue]

SetDelayedInitialValue[sym_Symbol, other__Symbol, body_] := (SetDelayedInitialValue[sym, body]; SetDelayedInitialValue[other, body]);

SetDelayedInitialValue[sym_Symbol, body_] := If[!HasOwnEvaluationsQ[sym],
  QuiverGeometryLoader`DeclarePreservedVariable[sym];
  SetDelayed[sym, body]
];

_SetDelayedInitialValue := Print["Bad SetDelayedInitialValue"];

(**************************************************************************************************)

PrivateMutatingFunction[SetUsage]

preprocessUsageString[usageString_] :=
  FixedPoint[substituteUsageSlots, usageString]

SetUsage[___] /; TrueQ[QuiverGeometryLoader`$DisableSetUsage] || TrueQ[QuiverGeometryLoader`$FastLoad] := Null;

SetUsage[usageString_Str] :=
  GeneralUtilities`SetUsage[Eval @ preprocessUsageString @ usageString];

SetUsage[symbol_Symbol, usageString_Str] :=
  GeneralUtilities`SetUsage[symbol, Eval @ preprocessUsageString @ usageString];

