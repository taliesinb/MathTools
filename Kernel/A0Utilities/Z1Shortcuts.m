$shortcutsContext := QuiverGeometryLoader`$ShortcutsContext;

(**************************************************************************************************)

(* QuiverGeometry`Shortcuts`SetSymbol[name_Str, value_] :=
   CreateSymbol[$shortcutsContext <> name, value];

QuiverGeometry`Shortcuts`CopySymbols[from_List, to_Str] :=
    Scan[QuiverGeometry`Shortcuts`CopySymbols[#, to]&, from];

QuiverGeometry`Shortcuts`CopySymbols[from_Str, to_Str] := Scope[
    names = Names[$shortcutsContext <> from <> "*"];
    newNames = Part[StringSplit[names, "`", 1], All, -1];
    newNames = StringDrop[newNames, StringLength @ from];
    newNames = StringJoin[to, #]& /@ newNames;
    CreateMultipleSymbols[$shortcutsContext, newNames, Symbol /@ names]
];
*)
(**************************************************************************************************)

PublicSpecialFunction[LoadShortcuts]

LoadShortcuts[] := LoadShortcuts["Core"];

LoadShortcuts::unknownGroup = "Shortcut group `` is not defined."

(* TODO: make shortcuts live in top level on a per-table basis, e.g. O`x, f`f, s`x, etc *)

LoadShortcuts[names_] := (
  Scan[loadShortcutGroup0, DeleteDuplicates @ Prepend["Core"] @ ToList[names]];
  AppendUniqueTo[$ContextPath, $shortcutsContext];
  $shortcutsContext
)

(**************************************************************************************************)

SetInitialValue[$loadedShortcutGroups, {}];

loadShortcutGroup0[name_] := If[MemberQ[$loadedShortcutGroups, name], Null,
  AppendTo[$loadedShortcutGroups, name];
  loadShortcutGroup[name];
];

loadShortcutGroup[name_] := Message[LoadShortcuts::unknownGroup, name];

(**************************************************************************************************)

strRules[str__Str] := strRules @ Riffle[{str}, " "]
strRules[str_Str] := VectorApply[Rule] @ Partition[StringSplit @ StringTrim @ str, 2];

$greekToRoman = strRules @ "
    \[Alpha] alpha \[Beta] beta \[Gamma] gamma \[Delta] delta \[CurlyEpsilon] ceps
    \[Epsilon] eps \[Zeta] zeta \[Eta] eta \[Theta] theta \[Iota] iota \[Kappa] kappa
    \[Lambda] lambda \[Mu] mu \[Nu] nu \[Xi] xi \[Pi] pi \[Rho] rho \[Sigma] sigma \[Tau] tau
    \[CurlyPhi] cphi \[Phi] phi \[Chi] chi \[Psi] psi \[Omega] omega \[Upsilon] upsilon
    \[CapitalGamma] Gamma \[CapitalDelta] Delta \[CapitalTheta] Theta \[CapitalLambda] Lambda
    \[CapitalXi] Xi \[CapitalPi] Pi \[CapitalSigma] Sigma \[CapitalPhi] Phi \[CapitalPsi] Psi \[CapitalOmega] Omega \[CapitalUpsilon] Upsilon
";
(*
$unicodeToName = strRules @ "
  ↓ darr ↑ uarr ← larr → rarr ↔ harr ↕ varr
  ⅇ exp ⅈ imag
  ⊗ otimes ⊕ oluss ⊖ ominus ⊙ odot
  ◁ ltriw ◀ ltrib ▷ trriw ▶ rtrib △ utriw ▲ utrib ▽ dtriw ▼ dtrib
  ⏵ rtribs ⏶ utriws
  ⋅ cdot
"; *)

$subSlots = {"#1" -> Slot[1], "#2" -> Slot[2], "#3" -> Slot[3], "#4" -> Slot[4], "#" -> Slot[1]}
toSymbolFunc[baseName_] := Construct[Fn, StringReplace[baseName, $subSlots]];

toNameList = Case[
  str_Str := StringReplace[toList @ str, $greekToRoman];
  list_List := list;
  other := (Print["BAD ITERATOR SPEC: ", other]; {});
];

toList = Case[
  str_Str /; StringContainsQ[str, " "] := StringSplit[str];
  str_Str := Characters[str];
  list_List := list;
  other_ := (Print["BAD ITERATOR SPEC: ", other]; {});
];

$valueSimplification = {
  Subscript[e_, None] :> e,
  Superscript[e_, None] :> e,
  Overscript[e_, None] :> e,
  Underscript[e_, None] :> e,
  None[e_] :> e
}

symbolTable[name_Str, valueFn_, iterators___] := Scope[
  nameFn = toSymbolFunc[name];
  {nameTuples, valueTuples} = Transpose @ Map[parseIterator, {iterators}];
  names = ApplyTuples[nameFn, nameTuples];
  values = ApplyTuples[valueFn, valueTuples] //. $valueSimplification;
  result = RuleThread[names, CreateMultipleSymbols[$shortcutsContext, names, values]];
  If[SeqLength[iterators] === 1,
    name2 = StringReplace[name, "#" -> ""];
    If[!StringEndsQ[name2, "$"], name2 = name2 <> "$"];
    (* TODO: if we introduce subcontexts will this collision check fail?? *)
    If[!NameQ[$shortcutsContext <> name2],
      PrependTo[result, makeSym[StringReplace[name2, "#" -> ""], valueFn]]]
  ];
  result
];

makeSym["$", value_] := Null;
makeSym[name_, value_] := With[{name2 = $shortcutsContext <> name},
    CreateSymbol[name2, value];
    name2 -> value
];

SetHoldRest[makeDelayedSym];
makeDelayedSym[name_, value_] :=
  ToExpression[$shortcutsContext <> name, InputForm, Fn[sym, SetDelayed[sym, value];, HoldAll]];

decSymTable[name_, fn_, it_] := decSymTable[name, fn, it, it];
decSymTable[name_, fn_, it_, All] := decSymTable[name, fn, it, it];

decSymTable[name_Str, fn_, it_, subIt_] := Join[
    symbolTable[name, fn, it],
    symbolTable[
        StringReplace[name, "#" -> "#1#2"],
        fn @ #2 @ #1&,
        subIt,
        maybe @ $primedOrSub
    ]
];

decSymTable[___] := (Print["INVALID CALL TO decSymTable"];)

parseIterator = Case[
  spec:{__Str}            := {spec, spec};    (* already parsed *)
  spec:{_List, _List}     := spec;            (* already parsed *)
  str_Str                 := %[str -> str];
  Rule[names_, values_]   := {toNameList @ names, toList @ values};
  maybe[spec_]            := MapThread[Prepend, {% @ spec, {"", None}}];
  specs_joinIts           := CatenateVectors @ Map[%, List @@ specs];
  mapIts[nfn_, vfn_][it_] := Scope[{names, vals} = % @ it; {nfn /@ names, vfn /@ vals}];
  tuplesIt[spec_, n_]     := joinLetters @ Map[makeTuples[#, n]&, % @ spec];
  subsetsIt[spec_, n_]    := joinLetters @ Map[Subsets[#, n]&, % @ spec];
  permsIt[spec_]          := joinLetters @ Map[Permutations, % @ spec];
];

joinLetters[{a_, b_}] := {Map[StringJoin, a], b};
makeTuples[e_, n_Int] := Tuples[e, n];
makeTuples[e_, m_Int ;; n_Int] := Catenate @ Table[Tuples[e, i], {i, m, n}];

(**************************************************************************************************)

$greekL = "\[Alpha]\[Beta]\[Gamma]\[Delta]\[CurlyEpsilon]\[Epsilon]\[Zeta]\[Eta]\[Theta]\[Iota]\[Kappa]\[Lambda]\[Mu]\[Nu]\[Xi]\[Pi]\[Rho]\[Sigma]\[Tau]\[CurlyPhi]\[Phi]\[Chi]\[Psi]\[Omega]";
$greekLS = "\[Alpha]\[Beta]\[Gamma]\[Pi]\[Rho]\[Sigma]\[Omega]\[Phi]";

$greekU = "\[CapitalGamma]\[CapitalDelta]\[CapitalTheta]\[CapitalLambda]\[CapitalXi]\[CapitalPi]\[CapitalSigma]\[CapitalPhi]\[CapitalPsi]\[CapitalOmega]";
$greekUS = "\[CapitalGamma]\[CapitalPi]\[CapitalSigma]\[CapitalOmega]\[CapitalPhi]";

toIteratorData[lower_, upper_] := parseIterator /@ {lower, upper, lower <> upper};

{$roman,        $ROMAN,         $Roman}         = toIteratorData[$LowercaseRomanLetters, $UppercaseRomanLetters];
{$greek,        $GREEK,         $Greek}         = toIteratorData[$greekL, $greekU];
{$romangreek,   $ROMANGREEK,    $RomanGreek}    = toIteratorData[$LowercaseRomanLetters <> $greekL, $UppercaseRomanLetters <> $greekU]
{$rom,          $ROM,           $Rom}           = toIteratorData[$LowercaseRomanLetters, $UppercaseRomanLetters];
{$gre,          $GRE,           $Gre}           = toIteratorData[$greekLS, $greekUS];
{$romgre,       $ROMGRE,        $RomGre}        = toIteratorData[$LowercaseRomanLetters <> $greekLS, $UppercaseRomanLetters <> $greekUS];

(**************************************************************************************************)

SetListable[shortcutSymbol];
shortcutSymbol[name_Str] := Symbol[$shortcutsContext <> name];

(**************************************************************************************************)

subOp[i_][e_] := Subscript[e, i];
supOp[i_][e_] := Superscript[e, i];

$style := $style = parseIterator["r g b rg gb rb p" -> {RedForm, GreenForm, BlueForm, OrangeForm, TealForm, PinkForm, PurpleForm}];
$extendedStyle := $extendedStyle = parseIterator[
    "r dr lr g dg lg b db lb rg drg lrg gb dgb lgb rb drb lrb p dp lp w0 w1 w2 dk k lk" -> 
    {RedForm, DarkRedForm, LightRedForm, GreenForm, DarkGreenForm, LightGreenForm, BlueForm, DarkBlueForm, LightBlueForm, OrangeForm, DarkOrangeForm, LightOrangeForm, TealForm, DarkTealForm, LightTealForm, PinkForm, DarkPinkForm, LightPinkForm, PurpleForm, DarkPurpleForm, LightPurpleForm, DarkGrayForm, GrayForm, LightGrayForm, DarkGrayForm, GrayForm, LightGrayForm}
];

(* these are delayed because $si hasn't been resolved via shortcutSymbol until we load Core *)
$intScripts := $intScripts = {-4, -3, -2, -1, 0, 1, 2, 3, 4, $si, $sj, $sk, $sm, $sn, $sM, $sN, \[FormalI]};
$natScripts := $natScripts = {0, 1, 2, 3, 4, $si, $sj, $sk, $sm, $sn, $sM, $sN, \[FormalI]};
$posScripts := $posScripts = {1, 2, 3, 4, $si, $sj, $sk, $sm, $sn, $sM, $sN, \[FormalI]};
intOps[op_] := "m4 m3 m2 m1 0 1 2 3 4 i j k m n M N $" -> Map[supOp, $intScripts];
natOps[op_] := "01234ijkmnMN$" -> Map[op, $natScripts];
posOps[op_] := "1234ijkmnMN$" -> Map[op, $posScripts];
$subscript := $subscript = natOps[subOp];
$superscript := $superscript = natOps[supOp];
$dimensions := $dimensions = parseIterator["1234mndMND" -> {1, 2, 3, 4, $sm, $sn, $sd, $sM, $sN, $sD}];

$plusMinus = {{"p", "m", "pm", "mp"}, supOp /@ {"+", "-", "\[PlusMinus]", "\[MinusPlus]"}};
$primed = {{"pr"}, {PrimedForm}};
$inverse = {{"inv"}, {InverseForm}};
$inverted = {{"i"}, {InvertedForm}};
$primedOrSub := $primedOrSub = joinIts[$subscript, $primed];
$cyclic = "c" -> {supOp["\[SmallCircle]"]};

(**************************************************************************************************)

loadShortcutGroup["Core"] := (

  symbolTable["$s#", SymbolForm, $Roman];

  {$si, $sj, $sk, $sl, $sm, $sn, $sp, $sd, $sM, $sN, $sP, $sD} = shortcutSymbol @
  {"$si", "$sj", "$sk", "$sl", "$sm", "$sn", "$sp", "$sd", "$sM", "$sN", "$sP", "$sD"};

  makeSym["$I",     \[FormalI]];
  makeSym["Sub",    Subscript[#, \[FormalI]]&];
  makeSym["Sup",    Superscript[#, \[FormalI]]&];

  makeSym["Div2",   InlineDivideForm[#, 2]&];
  makeSym["Div3",   InlineDivideForm[#, 3]&];
  makeSym["Div4",   InlineDivideForm[#, 4]&];
  makeSym["Div5",   InlineDivideForm[#, 5]&];
  makeSym["Div6",   InlineDivideForm[#, 6]&];
  makeSym["Div8",   InlineDivideForm[#, 8]&];

  nakeSym["Sub0",   Subscript[#, 0]&];
  nakeSym["Sub1",   Subscript[#, 1]&];
  nakeSym["Sub2",   Subscript[#, 2]&];
  nakeSym["Sub3",   Subscript[#, 3]&];
  nakeSym["Sub4",   Subscript[#, 4]&];
  nakeSym["Sub5",   Subscript[#, 5]&];
  nakeSym["Sub6",   Subscript[#, 6]&];
  nakeSym["Sub7",   Subscript[#, 7]&];
  nakeSym["Sub8",   Subscript[#, 8]&];
  nakeSym["Sub9",   Subscript[#, 9]&];
  nakeSym["SubI",   Subscript[#, $si]&];
  nakeSym["SubIm1", Subscript[#, SubtractForm[$si, 1]]&];
  nakeSym["SubIp1", Subscript[#, PlusForm[$si, 1]]&];
  nakeSym["SubJ",   Subscript[#, $sj]&];
  nakeSym["SubK",   Subscript[#, $sk]&];
  nakeSym["SubN",   Subscript[#, $sn]&];
  nakeSym["SubNm1", Subscript[#, SubtractForm[$sn, 1]]&];
  nakeSym["SubNp1", Subscript[#, PlusForm[$sn, 1]]&];

  makeSym["RF",     RedForm];
  makeSym["GF",     GreenForm];
  makeSym["BF",     BlueForm];
  makeSym["RGF",    OrangeForm];
  makeSym["RBF",    PinkForm];
  makeSym["GBF",    TealForm];
  makeSym["DRF",    DarkRedForm];
  makeSym["DGF",    DarkGreenForm];
  makeSym["DBF",    DarkBlueForm];
  makeSym["DRGF",   DarkOrangeForm];
  makeSym["DRBF",   DarkPinkForm];
  makeSym["DGBF",   DarkTealForm];

  makeSym["PFSF",   PartialFunctionSignatureForm];
  makeSym["FSF",    FunctionSignatureForm];
  makeSym["AF",     AppliedForm];
  makeSym["IAF",    ImplicitAppliedForm];
  makeSym["SetF",   SetForm];
  makeSym["AAF",    AssociativeArrayForm];
  makeSym["EOF",    ElementOfForm];
  makeSym["SF",     SymbolForm];

  makeSym["DDD",    EllipsisSymbol];
  makeSym["CRF",    CommaRowForm];
  makeSym["SCRF",   SpacedCommaRowForm];
  makeSym["DefEF",  DefEqualForm];
  makeSym["SynEF",  SyntaxEqualForm];
  makeSym["EF",     EqualForm];
  makeSym["NEF",    NotEqualForm];
  makeSym["ParenF", ParenthesesForm];
  makeSym["TAF",    TextAndForm];
  makeSym["CF",     GeneralUtilities`CommaForm];

  makeSym["VIF",    VertexIndexForm];
  makeSym["VLF",    VertexLabelForm];
  makeSym["ELF",    EdgeLabelForm];
  makeSym["ELF",    EdgeLabelForm];
  makeSym["GIF",    GraphIndexForm];
  makeSym["GLF",    GraphLabelForm];
  makeSym["GTIF",   GraphTooltipIndexForm];
  makeSym["GTLF",   GraphTooltipLabelForm];

  makeSym["CCR",    ClickCopyRow];

  makeSym["EGP",    ExtendedGraphPlot];
  makeSym["EG",     ExtendedGraph];

  makeSym["PCF",    PreformattedCodeForm]
  makeSym["NAF",    NestedArrayForm]
  makeSym["SBF",    StringBlockForm]

  makeSym["TGB",    ToGraphicsBoxes]

  makeSym["ACG",    ArrayCircuitGraphics]

  makeSym["ITF",    ImplicitTimesForm];
  makeSym["M1",     SubtractForm[#, 1]&];

  makeSym["$BF",    BaseFieldSymbol["K"]];

  makeSym["TMF",    ThreeMatrixForm];
  makeSym["EOL",    EndOfLine];

  makeSym["EOFSeq", eofSeq];
  eofSeq[a_, b_] := Sequence[a, ElementOfForm[a, b], b];

  decSymTable["$#",               Id,                                   $Greek, $Greek];

  decSymTable["$s#",              SymbolForm,                                 $RomanGreek];
  symbolTable["$s#1#2",           #2[SymbolForm[#1]]&,                        $RomGre, $style];

  symbolTable["$disk#1",          #1[FilledCircleSymbol]&,                     maybe @ $extendedStyle];
  symbolTable["$circ#1",          #1[CircleSymbol]&,                      maybe @ $extendedStyle];
  symbolTable["$bar#1",           #1[BarSymbol]&,                        maybe @ $extendedStyle];
  makeSym["$Sq$k",                DarkGrayForm[FilledSquareSymbol]];
  symbolTable["$Sq$#",            #1[FilledSquareSymbol]&, $style];

  decSymTable["$f#",              FunctionSymbolForm,                         $RomanGreek, $RomGre];

  $imagePreimage = {{"im", "pim", "mim", "mpim"}, {ImageModifierForm, PreimageModifierForm, MultiImageModifierForm, MultiPreimageModifierForm}};
  symbolTable["$f#1#2",           #2[FunctionSymbolForm[#1]]&,                $Rom, $imagePreimage];

  decSymTable["$set#",            SetSymbolForm,                              $ROMAN, $ROM];
  symbolTable["$set#1#2",         #2[SetSymbolForm[#1]]&,                     $ROM, $style];
  decSymTable["$set$#",           SetElementSymbolForm,                       $rom, $rom];
  symbolTable["$set$#1#2",        #2[SetElementSymbolForm[#1]]&,              $rom, $style];

  $fieldLetters = "kKRCQ";
  symbolTable["$Fi#",             FieldSymbol,                                $fieldLetters];
  symbolTable["$Fi#1#2",          FieldSymbol[#1, #2]&,                       $fieldLetters, $dimensions];
  symbolTable["$Vs#",             FieldSymbol,                                $fieldLetters];
  symbolTable["$lcc#",            LinearCombinationCoefficientSymbol,         $romangreek];
  symbolTable["$VsR#",            RealVectorSpaceForm,                        $fieldLetters, $dimensions];
  symbolTable["$VsC#",            ComplexVectorSpaceForm,                     $fieldLetters, $dimensions];

  makeDelayedSym["PLC", QuiverGeometry`PreviewLastCell[]];

);

(**************************************************************************************************)

loadShortcutGroup["Categories"] := (

  $scriptCapitals = Characters /@ {"ABCDEFGHIJKLMNOPQRSTUVWXYZ", "𝒜ℬ𝒞𝒟ℰℱ𝒢ℋℐ𝒥𝒦ℒℳ𝒩𝒪𝒫𝒬ℛ𝒮𝒯𝒰𝒱𝒲𝒳𝒴𝒵"};

  decSymTable["$C#",              CategorySymbol,                             $scriptCapitals];
  decSymTable["$Fun#",            FunctorSymbol,                              "ABCDEFGHUVPLRST"];
  decSymTable["$O#",              CategoryObjectSymbol,                       "01abcdeuvwxyzstABCDEIUVWMNLPRSTXYZ"];
  symbolTable["$IdA#",            IdArrow[CategoryObjectSymbol[#]]&,          "01abcdeuvwxyzstABCDEIUVWMNPXYZST"];
  decSymTable["$NT#",             NaturalTransformationSymbol,                "\[Mu]\[Eta]\[Lambda]\[Rho]\[Alpha]\[Beta]\[Gamma]\[Delta]\[Epsilon]\[Sigma]"];
  decSymTable["$A#",              CategoryArrowSymbol,                        "01efghjklmnpqrstuvwILMNR\[Mu]\[Eta]\[Lambda]\[Rho]\[Alpha]\[Beta]"];
  symbolTable["$1A#",             OneArrow[CategoryObjectSymbol[#]]&,         "abcdeuvwxyzstABCDEIUVWMNPXYZST"];

  makeSym["HCF",                  HorizontalCompositionForm];
  makeSym["VCF",                  VerticalCompositionForm];

  makeSym["$Ostar",               CategoryObjectSymbol["*"]];

  makeSym["MPF",                  MonoidalProductForm];
  makeSym["TMPF",                 TightMonoidalProductForm];
  makeSym["CPF",                  CartesianProductForm];

  makeSym["Hom", hom];
  hom[x_, y_] :=     HomForm[x, y];
  hom[c_, x_, y_] := ExplicitHomForm[c, x, y];

  makeSym["Arr", arr];
  arr[x_, y_] :=     MorphismForm[x, y];
  arr[l_, x_, y_] := NamedMorphismForm[l, x, y];

);

(**************************************************************************************************)

loadShortcutGroup["Graphs"] := (

    decSymTable["$Gr#",             GraphSymbol,                                "FGHJR"];

    decSymTable["$GrH#",            GraphHomomorphismSymbol,                                  $gre];
    decSymTable["$GrH#V",           Subscript[GraphHomomorphismSymbol[#], VerticesSymbol[]]&, $gre];
    decSymTable["$GrH#E",           Subscript[GraphHomomorphismSymbol[#], EdgesSymbol[]]&,    $gre];

    decSymTable["$gv#",             VertexSymbol,                               $rom, "ghuvwtxyz"];
    symbolTable["$gv#",             VertexSymbol,                               "NESW"];
    decSymTable["$gv#",             VertexSymbol,                               "o", "o"];
    symbolTable["$gv#1#2#3",        VertexSymbol[#2[#3[#1]]]&,                  joinIts[$roman, "012345"], maybe @ $style, maybe @ {{"$h", "$t"}, {HeadVertexForm, TailVertexForm}}];

    makeSym["VVPF",                 VerticalVertexProductForm];
    makeSym["VPF",                  VertexProductForm];
    makeSym["EPF",                  EdgeProductForm];
    makeSym["QPAF",                 QuiverProductAppliedForm];
    makeSym["CQPAF",                CompactQuiverProductAppliedForm];
    makeSym["LQPF",                 LockedQuiverProductForm];
    makeSym["RFQPF",                RightFreeQuiverProductForm];
    makeSym["LFQPF",                LeftFreeQuiverProductForm];
    makeSym["CQPF",                 CartesianQuiverProductForm];
    makeSym["SQPF",                 StrongQuiverProductForm];

    symbolTable["$gvdisk#1",        VertexSymbol[#1[FilledCircleSymbol]]&,       maybe @ $extendedStyle];
    symbolTable["$gvcirc#1",        VertexSymbol[#1[CircleSymbol]]&,        maybe @ $extendedStyle];

    symbolTable["$gvsqr#1",         VertexSymbol[#1[PlaceholderVertexSymbol]]&, maybe @ $extendedStyle];
    symbolTable["$gvhead#1",        VertexSymbol[#1[HeadVertexSymbol]]&,        maybe @ $extendedStyle];
    symbolTable["$gvtail#1",        VertexSymbol[#1[TailVertexSymbol]]&,        maybe @ $extendedStyle];

    decSymTable["$ge#",             EdgeSymbol,                                 $rom, "e"];
    symbolTable["$ge#1#2",          EdgeSymbol[#2[#1]]&,                        $roman, $style];

    makeSym["PPWF",                 ParenPathWordForm];
    makeSym["DE",                   DirectedEdgeForm];
    makeSym["UE",                   UndirectedEdgeForm];
    makeSym["DEdgeF",               DirectedEdgeForm];
    makeSym["UEdgeF",               UndirectedEdgeForm];

);

(**************************************************************************************************)

loadShortcutGroup["Quivers"] := (

  $quiverLetters = "FPQRGBS";
  $quiverDims := $quiverDims = parseIterator["01234imnpk" -> {0, 1, 2, 3, 4,Infinity, $sm, $sn, $sp, $sk}];
  $modQuiverDims := $modQuiverDims = parseIterator @ joinIts[$quiverDims, mapIts[StringJoinLeft["mod"], ModuloForm] @ $quiverDims];

  decSymTable["$Q#",              QuiverSymbol,                               $ROMAN, "QGHMRPF"];
  symbolTable["$Q#1#2",           QuiverSymbol[#2[#1]]&,                      $quiverLetters, $style];

  symbolTable["$Q#1#2",           QuiverSymbol[#2[#1]]&,                      "QMRPF", $cyclic];

  symbolTable["$BqQ#",            BouquetQuiverSymbol,                        $quiverDims];
  symbolTable["$LiQ#",            LineQuiverSymbol,                           $quiverDims];
  symbolTable["$SqQ#",            SquareQuiverSymbol,                         $modQuiverDims];
  symbolTable["$CbcQ#",           CubicQuiverSymbol,                          $modQuiverDims];
  symbolTable["$CbcQ#",           TriangularQuiverSymbol,                     $modQuiverDims];
  symbolTable["$CbcQ#",           GridQuiverSymbol,                           $modQuiverDims];

  (* all roman letters get inverted form *)
  symbolTable["$#2c#1",           #2[CardinalSymbol[#1]]&,                    $roman, maybe @ $inverted];

  (* common cardinals get inversion + subscripts + styling *)
  symbolTable["$#3c#1#2#4",       #3[#2[CardinalSymbol[#4[#1]]]]&,            "1abcdrsthvfgjwxyz", maybe @ $extendedStyle, maybe @ $inverted, maybe @ $subscript];

  (* arrow symbols *)
  $arrowheads = {{"left", "right", "up", "down"}, {LeftArrowheadSymbol, RightArrowheadSymbol, UpArrowheadSymbol, DownArrowheadSymbol}};
  symbolTable["$c#1#2",           CardinalSymbol[#2[#1]]&,                    $arrowheads, maybe @ $style];

  (* factor symbols *)
  $factors = {{"f", "b", "n", "F", "FB"}, {ForwardFactorSymbol, BackwardFactorSymbol, NeutralFactorSymbol, ForwardBackwardNeutralFactorSymbol, ForwardBackwardFactorSymbol}};
  symbolTable["$fs#1#2",          #2[#1]&,                                    $factors, maybe @ $extendedStyle];

  makeSym["QS",                   QuiverSymbol];
  makeSym["QPPF",                 QuiverProductPolyForm];

);

(**************************************************************************************************)

loadShortcutGroup["Arrays"] := (

  makeSym["MS", MatrixSymbol];

  decSymTable["$M#",              MatrixSymbol,                               $ROM <> "I", "ABCDMNPQRSTUVWXYZ"];

  decSymTable["$Arr#",            ArraySymbol,                                $ROM <> "I", "ABCDMNPQRSTUVWXYZ"];

  $coloredCards = "rgb" -> shortcutSymbol[{"$crr", "$cgg", "$cbb"}];
  decSymTable["$MM#",             MatrixSymbol[Subscript["M", #]]&,           $coloredCards];

);

(**************************************************************************************************)

loadShortcutGroup["Topology"] := (

  decSymTable["$top#",            TopologicalSpaceSymbolForm,                 $ROMAN, $ROM];

  decSymTable["$Bproj#",          BundleProjectionSymbolForm,                 "\[Pi]"];

  decSymTable["$Bsec#",           BundleSectionSymbolForm,                    $greek, $gre];

);

(**************************************************************************************************)

loadShortcutGroup["Data"] := (

  makeSym["SSetF",                SignedSetForm];
  makeSym["MSetF",                MultisetForm];
  makeSym["SMSetF",               SignedMultisetForm];
  makeSym["MSetsF",               MultisetsForm];
  makeSym["SMSetsF",              SignedMultisetsForm];
  makeSym["RepMSet",              RepeatedMultisetForm];

  decSymTable["$sset#",           SignedSetSymbolForm,                        $ROMAN, $ROM];
  symbolTable["$sset#1#2",        #2[SignedSetSymbolForm[#1]]&,               $ROM, $style];
  decSymTable["$sset$#",          SignedSetElementSymbolForm,                 $rom, $rom];

  $maybeNegated = {{"", "n"}, {Id, NegatedForm}};
  symbolTable["$sset$#3#1#2",     #2[#3[SignedSetElementSymbolForm[#1]]]&,    $rom, maybe @ $style, $maybeNegated];

  $ssset$abc = SignedSubsets[shortcutSymbol[{"$sset$ar", "$sset$bb", "$sset$cg"}]];

  ssetElToName = Case[
      {}              := "";
      list_List       := StringJoin @ Map[%, list];
      NegatedForm[n_] := ToUpperCase @ %[n];
      _[s_]           := % @ s;
      s_Str        := s;
  ];

  symbolTable["$lsset$#",         Apply[SignedSetForm],                       {Map[ssetElToName, $ssset$abc], $ssset$abc /. NegatedForm[head_[e_]] :> head[NegatedForm[e]]}];

  symbolTable["$sets#",           SubsetsForm,                                $ROM];
  symbolTable["$ssets#",          SignedSubsetsForm,                          $ROM];

  decSymTable["$mset#",           MultisetSymbolForm,                         $ROMAN, $ROM];
  decSymTable["$mset$#",          MultisetElementSymbolForm,                  $rom];

  decSymTable["$smset#",          SignedMultisetSymbolForm,                   $ROM];
  decSymTable["$smset$#",         SignedMultisetElementSymbolForm,            $rom];
  symbolTable["$smset$#3#1#2",    #2[#3[SignedMultisetElementSymbolForm[#1]]]&, $rom, maybe @ $style, $maybeNegated];

  symbolTable["$msets#",          MultisetsForm[SetSymbolForm[#]]&,           $ROM];
  symbolTable["$smsets#",         SignedMultisetsForm[SetSymbolForm[#]]&,     $ROM];

  makeSym["MMF",                  MultisetMultiplicityForm];
  makeSym["SMMF",                 SignedMultisetMultiplicityForm];

  makeSym["BSMF",                 BoundSignedMultiplicityFunctionForm];
  makeSym["BMF",                  BoundMultiplicityFunctionForm];

  symbolTable["$mult#",           BoundMultiplicityFunctionForm[MultisetSymbolForm[#]]&, $ROMAN];
  symbolTable["$smult#",          BoundSignedMultiplicityFunctionForm[SignedMultisetSymbolForm[#]]&, $ROMAN];

  $bl = "\[FilledSquare]";
  $wl = "\[EmptySquare]";

  $normAb = {{"a", "b"}, {"a", "b"}};
  $normBw = {{"0", "1"}, {$bl, $wl}};
  $wildBw = {{"1", "0", "z"}, {$bl, $wl, "-"}};
  $wildAb = {{"a", "b", "z"}, {"a", "b", "-"}};

  makeSym["Char0", $bl];
  makeSym["Char1", $wl];

  symbolTable["$lc#",             LiteralCharacterForm,                       joinIts[$normAb, $normBw]];

  $normAbBwIt = joinIts[tuplesIt[$normAb, 1 ;; 4], tuplesIt[$normBw, 1 ;; 4]];
  $wildAbBwIt = joinIts[tuplesIt[$wildAb, 1 ;; 4], tuplesIt[$wildBw, 1 ;; 4]];

  symbolTable["$ls$#",            LiteralStringForm[StringJoin @ #]&,         $normAbBwIt];
  symbolTable["$qs$#",            DoubleQuotedStringForm[StringJoin @ #]&,    $normAbBwIt];
  symbolTable["$ws$#",            WildcardStringForm[StringJoin @ #]&,        $wildAbBwIt];

  $normBw6 = permsIt["000111" -> {$wl, $wl, $wl, $bl, $bl, $bl}];
  symbolTable["$qs$#",            DoubleQuotedStringForm[StringJoin @ #]&,    $normBw6];

  makeSym["DQSF",                 DoubleQuotedStringForm];
  makeSym["DQSSet",               SetForm /* Map[DoubleQuotedStringForm]];

);

(**************************************************************************************************)

loadShortcutGroup["Algebra"] := (

  makeSym["$sgp",                 SemigroupProductForm[]];
  makeSym["$srp",                 SemiringProductForm[]];
  makeSym["$msetsrp",             MultisetSemiringProductForm[]];
  makeSym["$msetrp",              SignedMultisetRingProductForm[]];
  makeSym["$msetsum",             MultisetSumForm[]];
  makeSym["MSP",                  MultisetSemiringProductForm];
  makeSym["MSS",                  MultisetSemiringSumForm];
  makeSym["SMSP",                 SignedMultisetRingProductForm];
  makeSym["SMSS",                 SignedMultisetRingSumForm];
  makeSym["SGP",                  SemigroupProductForm];
  makeSym["WF",                   WordForm];
  makeSym["GMF",                  GroupMultiplicationForm];
  makeSym["gMF",                  GroupoidMultiplicationForm];
  makeSym["GS",                   GroupoidSymbol];

  makeSym["WS", WordSymbol];

  decSymTable["$Gp#",             GroupSymbol,                                "GHNVZ"];

  symbolTable["$WGp#",            WordGroupSymbol[QuiverSymbol[#]]&,          $quiverLetters];
  symbolTable["$WGp#",            WordGroupSymbol,                            $quiverDims];
  makeSym["$WGp",                 WordGroupSymbol[]];

  $groupLetters = "abcrfgjwvhxy01\[Alpha]\[Beta]\[Gamma]\[Omega]\[Upsilon]";
  decSymTable["$Gp$#",            GroupElementSymbol,                         $groupLetters];
  decSymTable["$Gp$i#",           GroupInverseForm[GroupElementSymbol[#]]&,   $groupLetters];

  symbolTable["$Gp$#",            GroupIdentitySymbol,                        "e"];
  decSymTable["$Gp$gen$#",        GroupGeneratorSymbol,                       $groupLetters];
  symbolTable["$Gp$gen$#1#2",     GroupGeneratorSymbol[#2[#1]]&,              $groupLetters, $style];

  decSymTable["$GpH#",            GroupHomomorphismSymbol,                    $greek];

  decSymTable["$Gpd#",            GroupoidSymbol,                             "GHN"];
  decSymTable["$Gpd$#",           GroupoidElementSymbol,                      "gh"];
  decSymTable["$Gpd$#",           GroupoidIdentitySymbol,                     "e"];
  decSymTable["$GpdH#",           GroupoidHomomorphismSymbol,                 $greek];

  decSymTable["$Ri#",             RingSymbol,                                 "RSPZQ"];
  makeSym["$Ri",                  RingSymbol[]];

  makeBasis[name_, sym_] := (
      symbolTable[name <> "$#", sym, "0123456789ijkn"];
      makeSym[UpperCaseLast @ name, FamilyModifierForm[sym[]]];
  );

  decSymTable["$Ri$#",            RingElementSymbol,                          $groupLetters];
  makeBasis["$Ri$e",              RingBasisElementForm];

  symbolTable["$WRi#",            WordRingSymbol[QuiverSymbol[#]]&,           $quiverLetters <> "1234"];
  makeSym["$WRi",                 WordRingSymbol[]];

  decSymTable["$WRi$#",           WordRingElementSymbol,                      $groupLetters];
  makeBasis["$WRi$e",             WordRingBasisElementForm];

  decSymTable["$Act#",            ActionSymbol,                               "A"];
  decSymTable["$ActGpd#",         ActionGroupoidSymbol[ActionSymbol[#]]&,     "G"];

  decSymTable["$La#",             LatticeSymbol,                              $ROM];
  decSymTable["$La$#",            LatticeElementSymbol,                       $Rom];
  decSymTable["$MSLa#",           MeetSemilatticeSymbol,                      $ROM];
  decSymTable["$MSLa$#",          MeetSemilatticeElementSymbol,               $rom];
  decSymTable["$JSLa#",           JoinSemilatticeSymbol,                      $ROM];
  decSymTable["$JSLa$#",          JoinSemilatticeElementSymbol,               $rom];

  decSymTable["$Pos#",            PosetSymbol,                                $ROM];
  decSymTable["$Pos$#",           PosetElementSymbol,                         $rom];

  $cellRingLetters = "kKRCQZNQ";
  symbolTable["$GL#1#2",          GeneralLinearGroupForm,                     $dimensions, $cellRingLetters];
  symbolTable["$SL#1#2",          SpecialLinearGroupForm,                     $dimensions, $cellRingLetters];
  symbolTable["$SO#1#2",          SpecialOrthogonalGroupForm,                 $dimensions, $cellRingLetters];
  symbolTable["$PGL#1#2",         ProjectiveGeneralLinearGroupForm,           $dimensions, $cellRingLetters];
  symbolTable["$PSL#1#2",         ProjectiveSpecialLinearGroupForm,           $dimensions, $cellRingLetters];

);

(**************************************************************************************************)

loadShortcutGroup["Paths"] := (

  decSymTable["$Pa#",             PathSymbol,                                 "PR"];

  decSymTable["$PaH#",            PathHomomorphismSymbol,                     $greek];
  decSymTable["$APaH#",           AffineModifierForm[PathHomomorphismSymbol[#]]&, $greek];

  symbolTable["$PaW#1#2",         #2[WordSymbol[#1]]&,                        "WPRSL", maybe @ $subscript];

  symbolTable["$PaGpd#",          PathGroupoidSymbol[QuiverSymbol[#]]&,       $quiverLetters];
  makeSym["$PaGpd",               PathGroupoidSymbol[]];

  $pathOrigins = "uvwxyzo";
  symbolTable["$PaQ#1",           PathQuiverSymbol[QuiverSymbol[#1]]&,        $quiverLetters];
  symbolTable["$FPaQ#1#2",        ForwardPathQuiverSymbol[QuiverSymbol[#1], VertexSymbol[#2]]&,  $quiverLetters, $pathOrigins];
  symbolTable["$BPaQ#1#2",        BackwardPathQuiverSymbol[QuiverSymbol[#1], VertexSymbol[#2]]&, $quiverLetters, $pathOrigins];

  makeSym["PCF",                  PathComposeForm];
  makeSym["PTF",                  PathTranslateForm];
  makeSym["PRF",                  PathReverseForm];
  makeSym["PPWF",                 ParenPathWordForm];
  makeSym["PWF",                  PathWordForm];
  makeSym["EPWF",                 EmptyPathWordForm];
  makeSym["PEPWF",                ParenEmptyPathWordForm];
  makeSym["PGS",                  PathGroupoidSymbol];
  makeSym["PQS",                  PathQuiverSymbol];
  makeSym["FPQS",                 ForwardPathQuiverSymbol];
  makeSym["BPQS",                 BackwardPathQuiverSymbol];

  makeSym["PMS",                  PathMapSymbol];
  makeSym["PS",                   PathSymbol];

);

(**************************************************************************************************)

loadShortcutGroup["Rewriting"] := (

  decSymTable["$Rs#",             RewritingSystemSymbol,                      $ROM, "RS"];
  symbolTable["$Rs#1#2",          RewritingSystemSymbol[#2[#1]]&, "RS",       $cyclic];

  decSymTable["$GS#",             GlobalStateSymbol,                          "gst"];
  decSymTable["$RS#",             RegionalStateSymbol,                        "RST"];
  decSymTable["$LS#",             LocalStateSymbol,                           "r"];

  makeSym["RSF",                  RegionalStateForm];

  makeSym["RRF",                  RewritingRuleForm];

);
