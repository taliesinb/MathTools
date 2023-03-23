$qgShortcutsContext = "QuiverGeometry`Shortcuts`";

Quiet[
ClearAll[$qgShortcutsContext <> "*"];
Remove[$qgShortcutsContext <> "*"];
];

makeSym["EOFSeq[a_, b_]", Sequence[a, ElementOfForm[a, b], b]];

makeSym["TMF", ThreeMatrixForm];

makeSym["EOL", EndOfLine];

QuiverGeometry`Shortcuts`SetSymbol[name_String, value_] :=
   CreateSymbol[$qgShortcutsContext <> name, value];

QuiverGeometry`Shortcuts`CopySymbols[from_List, to_String] :=
    Scan[QuiverGeometry`Shortcuts`CopySymbols[#, to]&, from];

QuiverGeometry`Shortcuts`CopySymbols[from_String, to_String] := Scope[
    names = Names[$qgShortcutsContext <> from <> "*"];
    newNames = Part[StringSplit[names, "`", 1], All, -1];
    newNames = StringDrop[newNames, StringLength @ from];
    newNames = StringJoin[to, #]& /@ newNames;
    CreateMultipleSymbols[$qgShortcutsContext, newNames, Symbol /@ names]
];

strRules[str__String] := strRules @ Riffle[{str}, " "]
strRules[str_String] := VectorApply[Rule] @ Partition[StringSplit @ StringTrim @ str, 2];

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
toSymbolFunc[baseName_] := Construct[Function, StringReplace[baseName, $subSlots]];

toNameList = Case[
    str_String := StringReplace[toList @ str, $greekToRoman];
    list_List := list;
    other := (Print["BAD ITERATOR SPEC: ", other]; {});
];

toList = Case[
    str_String /; StringContainsQ[str, " "] := StringSplit[str];
    str_String := Characters[str];
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

symbolTable[name_String, valueFn_, iterators___] := Scope[
    nameFn = toSymbolFunc[name];
    {nameTuples, valueTuples} = Transpose @ Map[parseIterator, {iterators}];
    names = ApplyTuples[nameFn, nameTuples];
    values = ApplyTuples[valueFn, valueTuples] //. $valueSimplification;
    result = RuleThread[names, CreateMultipleSymbols[$qgShortcutsContext, names, values]];
    If[SeqLength[iterators] === 1,
        name2 = StringReplace[name, "#" -> ""];
        If[!StringEndsQ[name2, "$"], name2 = name2 <> "$"];
        If[!NameQ[$qgShortcutsContext <> name2],
            PrependTo[result, makeSym[StringReplace[name2, "#" -> ""], valueFn]]]
    ];
    result
];

makeSym["$", value_] := Null;
makeSym[name_, value_] := (
    CreateSymbol[$qgShortcutsContext <> name, value];
    $qgShortcutsContext <> name -> value
);

decSymTable[name_, fn_, it_] := decSymTable[name, fn, it, it];
decSymTable[name_, fn_, it_, All] := decSymTable[name, fn, it, it];

decSymTable[name_String, fn_, it_, subIt_] := Join[
    symbolTable[name, fn, it],
    symbolTable[
        StringReplace[name, "#" -> "#1#2"],
        fn @ #2 @ #1&,
        subIt,
        Maybe @ $PrimeOrSub
    ]
];

decSymTable[___] := (Print["INVALID CALL TO decSymTable"];)

makeBasis[name_, sym_] := (
    symbolTable[name <> "$#", sym, "0123456789ijkn"];
    makeSym[UpperCaseLast @ name, FamilyModifierForm[sym[]]];
);

parseIterator = Case[
    spec:{__String}         := {spec, spec};    (* already parsed *)
    spec:{_List, _List}     := spec;            (* already parsed *)
    str_String              := %[str -> str];
    Rule[names_, values_]   := {toNameList @ names, toList @ values};
    Maybe[spec_]            := MapThread[Prepend, {% @ spec, {"", None}}];
    specs_JoinIts           := CatenateVectors @ Map[%, List @@ specs];
    MapIts[nfn_, vfn_][it_] := Scope[{names, vals} = % @ it; {nfn /@ names, vfn /@ vals}];
    TuplesIt[spec_, n_]     := joinLetters @ Map[makeTuples[#, n]&, % @ spec];
    SubsetsIt[spec_, n_]    := joinLetters @ Map[Subsets[#, n]&, % @ spec];
    PermsIt[spec_]          := joinLetters @ Map[Permutations, % @ spec];
];


joinLetters[{a_, b_}] := {Map[StringJoin, a], b};
makeTuples[e_, n_Integer] := Tuples[e, n];
makeTuples[e_, m_Integer ;; n_Integer] := Catenate @ Table[Tuples[e, i], {i, m, n}];

$romanL = "abcdefghijklmnpqrstuvwxyz";
$romanU = ToUpperCase @ $romanL;

$romanLS = "abcfghstuvwxyz";
$romanUS = ToUpperCase @ $romanLS;

$greekL = "\[Alpha]\[Beta]\[Gamma]\[Delta]\[CurlyEpsilon]\[Epsilon]\[Zeta]\[Eta]\[Theta]\[Iota]\[Kappa]\[Lambda]\[Mu]\[Nu]\[Xi]\[Pi]\[Rho]\[Sigma]\[Tau]\[CurlyPhi]\[Phi]\[Chi]\[Psi]\[Omega]";
$greekLS = "\[Alpha]\[Beta]\[Gamma]\[Pi]\[Rho]\[Sigma]\[Omega]\[Phi]";

$greekU = "\[CapitalGamma]\[CapitalDelta]\[CapitalTheta]\[CapitalLambda]\[CapitalXi]\[CapitalPi]\[CapitalSigma]\[CapitalPhi]\[CapitalPsi]\[CapitalOmega]";
$greekUS = "\[CapitalGamma]\[CapitalPi]\[CapitalSigma]\[CapitalOmega]\[CapitalPhi]";

toIteratorData[lower_, upper_] := parseIterator /@ {lower, upper, lower <> upper};

{$roman,        $ROMAN,         $Roman}         = toIteratorData[$romanL, $romanU];
{$greek,        $GREEK,         $Greek}         = toIteratorData[$greekL, $greekU];
{$romangreek,   $ROMANGREEK,    $RomanGreek}    = toIteratorData[$romanL <> $greekL, $romanU <> $greekU]
{$rom,          $ROM,           $Rom}           = toIteratorData[$romanLS, $romanUS];
{$gre,          $GRE,           $Gre}           = toIteratorData[$greekLS, $greekUS];
{$romgre,       $ROMGRE,        $RomGre}        = toIteratorData[$romanLS <> $greekLS, $romanUS <> $greekUS];

SubOp[i_][e_] := Subscript[e, i];
SupOp[i_][e_] := Superscript[e, i];

$Style := $Style = parseIterator["r g b rg gb rb p" -> {RedForm, GreenForm, BlueForm, OrangeForm, TealForm, PinkForm, PurpleForm}];
$ExtendedStyle := $ExtendedStyle = parseIterator["r dr lr g dg lg b db lb rg drg lrg gb dgb lgb rb drb lrb p dp lp w0 w1 w2 dk k lk" -> {RedForm, DarkRedForm, LightRedForm, GreenForm, DarkGreenForm, LightGreenForm, BlueForm, DarkBlueForm, LightBlueForm, OrangeForm, DarkOrangeForm, LightOrangeForm, TealForm, DarkTealForm, LightTealForm, PinkForm, DarkPinkForm, LightPinkForm, PurpleForm, DarkPurpleForm, LightPurpleForm, DarkGrayForm, MediumGrayForm, LightGrayForm, DarkGrayForm, MediumGrayForm, LightGrayForm}];

$intScripts := $intScripts = {-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, $si, $sj, $sk, $sm, $sn, $sM, $sN, \[FormalI]};
$natScripts := $natScripts = {0, 1, 2, 3, 4, 5, 6, $si, $sj, $sk, $sm, $sn, $sM, $sN, \[FormalI]};
$posScripts := $posScripts = {1, 2, 3, 4, 5, 6, $si, $sj, $sk, $sm, $sn, $sM, $sN, \[FormalI]};
IntOps[op_] := "m5 m4 m3 m2 m1 0 1 2 3 4 5 i j k m n M N $" -> Map[SupOp, $intScripts];
NatOps[op_] := "0123456ijkmnMN$" -> Map[op, $natScripts];
PosOps[op_] := "123456ijkmnMN$" -> Map[op, $posScripts];

$Subscript := $Subscript = NatOps[SubOp];
$Superscript := $Superscript = NatOps[SupOp];
$Dimensions := $Dimensions = parseIterator["12345mndMND" -> {1, 2, 3, 4, 5, $sm, $sn, $sd, $sM, $sN, $sD}];
$QuiverDims := $QuiverDims = parseIterator["012345imnpk" -> {0, 1, 2, 3, 4, 5, Infinity, $sm, $sn, $sp, $sk}];
$ModQuiverDims := $ModQuiverDims = parseIterator @ JoinIts[$QuiverDims, MapIts[StringJoinLeft["mod"], ModuloForm] @ $QuiverDims];

$PlusMinus = {{"p", "m", "pm", "mp"}, SupOp /@ {"+", "-", "\[PlusMinus]", "\[MinusPlus]"}};
$Prime = {{"pr"}, {PrimedForm}};
$Inverse = {{"inv"}, {InverseForm}};
$Inverted = {{"i"}, {InvertedForm}};

$PrimeOrSub := $PrimeOrSub = JoinIts[$Subscript, $Prime];

$ImagePreimage = {{"im", "pim", "mim", "mpim"}, {ImageModifierForm, PreimageModifierForm, MultiImageModifierForm, MultiPreimageModifierForm}};

$Cyclic = "c" -> {SupOp["\[SmallCircle]"]};

symbolTable["$s#", SymbolForm, $Roman];

{$si, $sj, $sk, $sl, $sm, $sn, $sp, $sd, $sM, $sN, $sP, $sD} = {
    QuiverGeometry`Shortcuts`$si, QuiverGeometry`Shortcuts`$sj, QuiverGeometry`Shortcuts`$sk,  QuiverGeometry`Shortcuts`$sl,
    QuiverGeometry`Shortcuts`$sm, QuiverGeometry`Shortcuts`$sn, QuiverGeometry`Shortcuts`$sp, QuiverGeometry`Shortcuts`$sd,
    QuiverGeometry`Shortcuts`$sM, QuiverGeometry`Shortcuts`$sN, QuiverGeometry`Shortcuts`$sP, QuiverGeometry`Shortcuts`$sD
};

$HeadTail = {{"$h", "$t"}, {HeadVertexForm, TailVertexForm}};
$MaybeNegated = {{"", "n"}, {Identity, NegatedForm}};

$Arrowheads = {{"left", "right", "up", "down"}, {LeftArrowheadSymbol, RightArrowheadSymbol, UpArrowheadSymbol, DownArrowheadSymbol}};
    
$Factors = {{"f", "b", "n", "F", "FB"}, {ForwardFactorSymbol, BackwardFactorSymbol, NeutralFactorSymbol, ForwardBackwardNeutralFactorSymbol, ForwardBackwardFactorSymbol}};

$scriptCapitals = Characters /@ {"ABCDEFGHIJKLMNOPQRSTUVWXYZ", "𝒜ℬ𝒞𝒟ℰℱ𝒢ℋℐ𝒥𝒦ℒℳ𝒩𝒪𝒫𝒬ℛ𝒮𝒯𝒰𝒱𝒲𝒳𝒴𝒵"};

$quiverLetters = "FPQRGBS";

decSymTable["$#",               Identity,                                   $Greek, $Greek];
                    
decSymTable["$s#",              SymbolForm,                                 $RomanGreek];
symbolTable["$s#1#2",           #2[SymbolForm[#1]]&,                        $RomGre, $Style];
                    
decSymTable["$Q#",              QuiverSymbol,                               $ROMAN, "QGHMRPF"];
symbolTable["$Q#1#2",           QuiverSymbol[#2[#1]]&,                      $quiverLetters, $Style];
                    
symbolTable["$Q#1#2",           QuiverSymbol[#2[#1]]&,                      "QMRPF", $Cyclic];

symbolTable["$BqQ#",            BouquetQuiverSymbol,                        $QuiverDims];
symbolTable["$LiQ#",            LineQuiverSymbol,                           $QuiverDims];
symbolTable["$SqQ#",            SquareQuiverSymbol,                         $ModQuiverDims];
symbolTable["$CbcQ#",           CubicQuiverSymbol,                          $ModQuiverDims];
symbolTable["$CbcQ#",           TriangularQuiverSymbol,                     $ModQuiverDims];
symbolTable["$CbcQ#",           GridQuiverSymbol,                           $ModQuiverDims];


QuiverGeometry`Shortcuts`Arr[x_, y_] := MorphismForm[x, y];
QuiverGeometry`Shortcuts`Arr[l_, x_, y_] := NamedMorphismForm[l, x, y];

decSymTable["$C#",              CategorySymbol,                             $scriptCapitals];
decSymTable["$O#",              CategoryObjectSymbol,                       "01abcdeuvwxyzst"];
symbolTable["$IdA#",            IdArrow[CategoryObjectSymbol[#]]&,          "abcdeuvwxyzst"];
decSymTable["$A#",              CategoryArrowSymbol,                        "efghjklmnpqrstuvw"];

QuiverGeometry`Shortcuts`$Ostar = CategoryObjectSymbol["*"];
QuiverGeometry`Shortcuts`Hom[x_, y_] := HomForm[x, y];
QuiverGeometry`Shortcuts`Hom[c_, x_, y_] := ExplicitHomForm[c, x, y];

makeSym["MPF",                  MonoidalProductForm];
makeSym["CPF",                  CartesianProductForm];

decSymTable["$gv#",             VertexSymbol,                               $rom, "ghuvwtxyz"];
symbolTable["$gv#",             VertexSymbol,                               "NESW"];
decSymTable["$gv#",             VertexSymbol,                               "o", "o"];
symbolTable["$gv#1#2#3",        VertexSymbol[#2[#3[#1]]]&,                  JoinIts[$roman, "012345"], Maybe @ $Style, Maybe @ $HeadTail];

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

symbolTable["$disk#1",          #1[FilledTokenSymbol]&,                     Maybe @ $ExtendedStyle];
symbolTable["$circ#1",          #1[EmptyTokenSymbol]&,                      Maybe @ $ExtendedStyle];
symbolTable["$bar#1",           #1[BarTokenSymbol]&,                        Maybe @ $ExtendedStyle];

symbolTable["$gvdisk#1",        VertexSymbol[#1[FilledTokenSymbol]]&,       Maybe @ $ExtendedStyle];
symbolTable["$gvcirc#1",        VertexSymbol[#1[EmptyTokenSymbol]]&,        Maybe @ $ExtendedStyle];

symbolTable["$gvsqr#1",         VertexSymbol[#1[PlaceholderVertexSymbol]]&, Maybe @ $ExtendedStyle];
symbolTable["$gvhead#1",        VertexSymbol[#1[HeadVertexSymbol]]&,        Maybe @ $ExtendedStyle];
symbolTable["$gvtail#1",        VertexSymbol[#1[TailVertexSymbol]]&,        Maybe @ $ExtendedStyle];
                    
decSymTable["$ge#",             EdgeSymbol,                                 $rom, "e"];
symbolTable["$ge#1#2",          EdgeSymbol[#2[#1]]&,                        $roman, $Style];
                    
decSymTable["$M#",              MatrixSymbol,                               $ROM <> "I", "ABCMXYZ"];

$coloredCards = "rgb" -> {QuiverGeometry`Shortcuts`$crr, QuiverGeometry`Shortcuts`$cgg, QuiverGeometry`Shortcuts`$cbb};
decSymTable["$MM#",             MatrixSymbol[Subscript["M", #]]&,           $coloredCards];

decSymTable["$f#",              FunctionSymbolForm,                         $RomanGreek, $RomGre];
symbolTable["$f#1#2",           #2[FunctionSymbolForm[#1]]&,                $Rom, $ImagePreimage];
                    
decSymTable["$top#",            TopologicalSpaceSymbolForm,                 $ROMAN, $ROM];
decSymTable["$Bproj#",          BundleProjectionSymbolForm,                 "\[Pi]"];
decSymTable["$Bsec#",           BundleSectionSymbolForm,                    $greek, $gre];
                    
decSymTable["$set#",            SetSymbolForm,                              $ROMAN, $ROM];
symbolTable["$set#1#2",         #2[SetSymbolForm[#1]]&,                     $ROM, $Style];
decSymTable["$set$#",           SetElementSymbolForm,                       $rom, $rom];
symbolTable["$set$#1#2",        #2[SetElementSymbolForm[#1]]&,              $rom, $Style];
    
decSymTable["$sset#",           SignedSetSymbolForm,                        $ROMAN, $ROM];
symbolTable["$sset#1#2",        #2[SignedSetSymbolForm[#1]]&,               $ROM, $Style];
decSymTable["$sset$#",          SignedSetElementSymbolForm,                 $rom, $rom];
symbolTable["$sset$#3#1#2",     #2[#3[SignedSetElementSymbolForm[#1]]]&,    $rom, Maybe @ $Style, $MaybeNegated];

$ssset$abc = SignedSubsets[{QuiverGeometry`Shortcuts`$sset$ar, QuiverGeometry`Shortcuts`$sset$bb, QuiverGeometry`Shortcuts`$sset$cg}];

ssetElToName = Case[
    {}              := "";
    list_List       := StringJoin @ Map[%, list];
    NegatedForm[n_] := ToUpperCase @ %[n];
    _[s_]           := % @ s;
    s_String        := s;
];

symbolTable["$lsset$#",         Apply[SignedSetForm],                       {Map[ssetElToName, $ssset$abc], $ssset$abc /. NegatedForm[head_[e_]] :> head[NegatedForm[e]]}];
                    
symbolTable["$sets#",           SubsetsForm,                                $ROM];
symbolTable["$ssets#",          SignedSubsetsForm,                          $ROM];
                    
decSymTable["$mset#",           MultisetSymbolForm,                         $ROMAN, $ROM];
decSymTable["$mset$#",          MultisetElementSymbolForm,                  $rom];
            
decSymTable["$smset#",          SignedMultisetSymbolForm,                   $ROM];
decSymTable["$smset$#",         SignedMultisetElementSymbolForm,            $rom];
symbolTable["$smset$#3#1#2",    #2[#3[SignedMultisetElementSymbolForm[#1]]]&, $rom, Maybe @ $Style, $MaybeNegated];

symbolTable["$msets#",          MultisetsForm[SetSymbolForm[#]]&,           $ROM];
symbolTable["$smsets#",         SignedMultisetsForm[SetSymbolForm[#]]&,     $ROM];
        
symbolTable["$mult#",           BoundMultiplicityFunctionForm[MultisetSymbolForm[#]]&, $ROMAN];
symbolTable["$smult#",          BoundSignedMultiplicityFunctionForm[SignedMultisetSymbolForm[#]]&, $ROMAN];

makeSym["$sgp",                 SemigroupProductForm[]];
makeSym["$srp",                 SemiringProductForm[]];
makeSym["$msetsrp",             MultisetSemiringProductForm[]];
makeSym["$msetrp",              SignedMultisetRingProductForm[]];
makeSym["$msetsum",             MultisetSumForm[]];
makeSym["ITF",                  ImplicitTimesForm];
makeSym["M1",                   SubtractForm[#, 1]&];
makeSym["AAF",                  AssociativeArrayForm];
makeSym["MMF",                  MultisetMultiplicityForm];
makeSym["SMMF",                 SignedMultisetMultiplicityForm];
makeSym["MSP",                  MultisetSemiringProductForm];
makeSym["MSS",                  MultisetSemiringSumForm];
makeSym["SMSP",                 SignedMultisetRingProductForm];
makeSym["SMSS",                 SignedMultisetRingSumForm];
makeSym["SGP",                  SemigroupProductForm];

decSymTable["$Pa#",             PathSymbol,                                 "PR"];

decSymTable["$PaH#",            PathHomomorphismSymbol,                     $greek];
decSymTable["$APaH#",           AffineModifierForm[PathHomomorphismSymbol[#]]&, $greek];

symbolTable["$PaW#1#2",         #2[WordSymbol[#1]]&,                        "WPRSL", Maybe @ $Subscript];

decSymTable["$Gp#",             GroupSymbol,                                "GHNVZ"];

symbolTable["$WGp#",            WordGroupSymbol[QuiverSymbol[#]]&,          $quiverLetters];
symbolTable["$WGp#",            WordGroupSymbol,                            $QuiverDims];
makeSym["$WGp",                 WordGroupSymbol[]];

symbolTable["$PaGpd#",          PathGroupoidSymbol[QuiverSymbol[#]]&,       $quiverLetters];
makeSym["$PaGpd",               PathGroupoidSymbol[]];

$pathOrigins = "uvwxyzo";
symbolTable["$PaQ#1",           PathQuiverSymbol[QuiverSymbol[#1]]&,        $quiverLetters];
symbolTable["$FPaQ#1#2",        ForwardPathQuiverSymbol[QuiverSymbol[#1], VertexSymbol[#2]]&,  $quiverLetters, $pathOrigins];
symbolTable["$BPaQ#1#2",        BackwardPathQuiverSymbol[QuiverSymbol[#1], VertexSymbol[#2]]&, $quiverLetters, $pathOrigins];

$groupLetters = "abcrfgjwvhxy01\[Alpha]\[Beta]\[Gamma]\[Omega]\[Upsilon]";
decSymTable["$Gp$#",            GroupElementSymbol,                         $groupLetters];
decSymTable["$Gp$i#",           GroupInverseForm[GroupElementSymbol[#]]&,   $groupLetters];
                    
symbolTable["$Gp$#",            GroupIdentitySymbol,                        "e"];
decSymTable["$Gp$gen$#",        GroupGeneratorSymbol,                       $groupLetters];
symbolTable["$Gp$gen$#1#2",     GroupGeneratorSymbol[#2[#1]]&,              $groupLetters, $Style];

decSymTable["$GpH#",            GroupHomomorphismSymbol,                    $greek];
                    
decSymTable["$Gpd#",            GroupoidSymbol,                             "GHN"];
decSymTable["$Gpd$#",           GroupoidElementSymbol,                      "gh"];
decSymTable["$Gpd$#",           GroupoidIdentitySymbol,                     "e"];
decSymTable["$GpdH#",           GroupoidHomomorphismSymbol,                 $greek];

decSymTable["$Ri#",             RingSymbol,                                 "RSPZQ"];
makeSym["$Ri",                  RingSymbol[]];

decSymTable["$Ri$#",            RingElementSymbol,                          $groupLetters];
makeBasis["$Ri$e",              RingBasisElementForm];

symbolTable["$WRi#",            WordRingSymbol[QuiverSymbol[#]]&,           $quiverLetters <> "1234"];
makeSym["$WRi",                 WordRingSymbol[]];

decSymTable["$WRi$#",           WordRingElementSymbol,                      $groupLetters];
makeBasis["$WRi$e",             WordRingBasisElementForm];

symbolTable["$lcc#",            LinearCombinationCoefficientSymbol,         $romangreek];

decSymTable["$Act#",            ActionSymbol,                               "A"];
decSymTable["$ActGpd#",         ActionGroupoidSymbol[ActionSymbol[#]]&,     "G"];
symbolTable["$Sq$#",             #1[FilledSquareTokenSymbol]&,               $Style];
makeSym["$Sq$k",                DarkGrayForm[FilledSquareTokenSymbol]];

decSymTable["$La#",             LatticeSymbol,                              $ROM];
decSymTable["$La$#",            LatticeElementSymbol,                       $Rom];
decSymTable["$MSLa#",           MeetSemilatticeSymbol,                      $ROM];
decSymTable["$MSLa$#",          MeetSemilatticeElementSymbol,               $rom];
decSymTable["$JSLa#",           JoinSemilatticeSymbol,                      $ROM];
decSymTable["$JSLa$#",          JoinSemilatticeElementSymbol,               $rom];
                    
decSymTable["$Pos#",            PosetSymbol,                                $ROM];
decSymTable["$Pos$#",           PosetElementSymbol,                         $rom];
            
decSymTable["$Gr#",             GraphSymbol,                                "FGHJR"];
decSymTable["$GrH#",            GraphHomomorphismSymbol,                    $gre];
decSymTable["$GrH#V",           Subscript[GraphHomomorphismSymbol[#], VerticesSymbol[]]&, $gre];
decSymTable["$GrH#E",           Subscript[GraphHomomorphismSymbol[#], EdgesSymbol[]]&, $gre];
            
$fieldLetters = "kKRCQ";
symbolTable["$Fi#",             FieldSymbol,                                $fieldLetters];
symbolTable["$Fi#1#2",          FieldSymbol[#1, #2]&,                       $fieldLetters, $Dimensions];

symbolTable["$Vs#",             FieldSymbol,                                $fieldLetters];
symbolTable["$VsR#",            RealVectorSpaceForm,                        $fieldLetters, $Dimensions];
symbolTable["$VsC#",            ComplexVectorSpaceForm,                     $fieldLetters, $Dimensions];
    
$cellRingLetters = "kKRCQZNQ";
$matrixDims =
symbolTable["$GL#1#2",          GeneralLinearGroupForm,                     $Dimensions, $cellRingLetters];
symbolTable["$SL#1#2",          SpecialLinearGroupForm,                     $Dimensions, $cellRingLetters];
symbolTable["$SO#1#2",          SpecialOrthogonalGroupForm,                 $Dimensions, $cellRingLetters];
symbolTable["$PGL#1#2",         ProjectiveGeneralLinearGroupForm,           $Dimensions, $cellRingLetters];
symbolTable["$PSL#1#2",         ProjectiveSpecialLinearGroupForm,           $Dimensions, $cellRingLetters];

(* all roman letters get inverted form *)
symbolTable["$#2c#1",           #2[CardinalSymbol[#1]]&,                    $roman, Maybe @ $Inverted];

(* common cardinals get inversion + subscripts + styling *)
symbolTable["$#3c#1#2#4",       #3[#2[CardinalSymbol[#4[#1]]]]&,            "1abcdrsthvfgjwxyz", Maybe @ $ExtendedStyle, Maybe @ $Inverted, Maybe @ $Subscript];

(* arrow symbols *)
symbolTable["$c#1#2",           CardinalSymbol[#2[#1]]&,                    $Arrowheads, Maybe @ $Style];

(* factor symbols *)
symbolTable["$fs#1#2",          #2[#1]&,                                    $Factors, Maybe @ $ExtendedStyle];

decSymTable["$Rs#",             RewritingSystemSymbol,                      $ROM, "RS"];
symbolTable["$Rs#1#2",          RewritingSystemSymbol[#2[#1]]&, "RS",       $Cyclic];
            
decSymTable["$GS#",             GlobalStateSymbol,                          "gst"]
decSymTable["$RS#",             RegionalStateSymbol,                        "RST"]
decSymTable["$LS#",             LocalStateSymbol,                           "r"];

$bl = "\[FilledSquare]";
$wl = "\[EmptySquare]";

$normAb = {{"a", "b"}, {"a", "b"}};
$normBw = {{"0", "1"}, {$bl, $wl}};
$wildBw = {{"1", "0", "z"}, {$bl, $wl, "-"}};
$wildAb = {{"a", "b", "z"}, {"a", "b", "-"}};

makeSym["Char0", $bl];
makeSym["Char1", $wl];

symbolTable["$lc#",             LiteralCharacterForm,                       JoinIts[$normAb, $normBw]];

$normAbBwIt = JoinIts[TuplesIt[$normAb, 1 ;; 4], TuplesIt[$normBw, 1 ;; 4]];
$wildAbBwIt = JoinIts[TuplesIt[$wildAb, 1 ;; 4], TuplesIt[$wildBw, 1 ;; 4]];

symbolTable["$ls$#",            LiteralStringForm[StringJoin @ #]&,         $normAbBwIt];
symbolTable["$qs$#",            DoubleQuotedStringForm[StringJoin @ #]&,    $normAbBwIt];
symbolTable["$ws$#",            WildcardStringForm[StringJoin @ #]&,        $wildAbBwIt];

$normBw6 = PermsIt["000111" -> {$wl, $wl, $wl, $bl, $bl, $bl}];
symbolTable["$qs$#",            DoubleQuotedStringForm[StringJoin @ #]&,    $normBw6];

(* decorated: prime, dagger, inverse, head, tail, etc *)

makeSym["$BF", BaseFieldSymbol["K"]];

makeSym["$I", \[FormalI]];
QuiverGeometry`Shortcuts`Sub[e_] := Subscript[e, QuiverGeometry`Shortcuts`$I];
QuiverGeometry`Shortcuts`Sup[e_] := Superscript[e, QuiverGeometry`Shortcuts`$I];

makeSym["Div2", InlineDivideForm[#, 2]&];
makeSym["Div3", InlineDivideForm[#, 3]&];
makeSym["Div4", InlineDivideForm[#, 4]&];
makeSym["Div5", InlineDivideForm[#, 5]&];
makeSym["Div6", InlineDivideForm[#, 6]&];
makeSym["Div8", InlineDivideForm[#, 8]&];

QuiverGeometry`Shortcuts`Sub0[e_] := Subscript[e, 0];
QuiverGeometry`Shortcuts`Sub1[e_] := Subscript[e, 1];
QuiverGeometry`Shortcuts`Sub2[e_] := Subscript[e, 2];
QuiverGeometry`Shortcuts`Sub3[e_] := Subscript[e, 3];
QuiverGeometry`Shortcuts`Sub4[e_] := Subscript[e, 4];
QuiverGeometry`Shortcuts`Sub5[e_] := Subscript[e, 5];
QuiverGeometry`Shortcuts`Sub6[e_] := Subscript[e, 6];
QuiverGeometry`Shortcuts`Sub7[e_] := Subscript[e, 7];
QuiverGeometry`Shortcuts`Sub8[e_] := Subscript[e, 8];
QuiverGeometry`Shortcuts`Sub9[e_] := Subscript[e, 9];
QuiverGeometry`Shortcuts`SubI[e_] := Subscript[e, $si];
QuiverGeometry`Shortcuts`SubIm1[e_] := Subscript[e, SubtractForm[$si, 1]];
QuiverGeometry`Shortcuts`SubIp1[e_] := Subscript[e, PlusForm[$si, 1]];
QuiverGeometry`Shortcuts`SubJ[e_] := Subscript[e, $sj];
QuiverGeometry`Shortcuts`SubK[e_] := Subscript[e, $sk];
QuiverGeometry`Shortcuts`SubN[e_] := Subscript[e, $sn];
QuiverGeometry`Shortcuts`SubNm1[e_] := Subscript[e, SubtractForm[$sn, 1]];
QuiverGeometry`Shortcuts`SubNp1[e_] := Subscript[e, PlusForm[$sn, 1]];

makeSym["DQSF", DoubleQuotedStringForm];
makeSym["DQSSet", SetForm /* Map[DoubleQuotedStringForm]];
makeSym["RSF", RegionalStateForm];

makeSym["RRF", RewritingRuleForm];

makeSym["DDD", EllipsisSymbol];
makeSym["SetF", SetForm];
makeSym["SSetF", SignedSetForm];

makeSym["MSetF", MultisetForm];
makeSym["SMSetF", SignedMultisetForm];

makeSym["MSetsF", MultisetsForm];
makeSym["SMSetsF", SignedMultisetsForm];
makeSym["RepMSet", RepeatedMultisetForm];

makeSym["CRF", CommaRowForm];
makeSym["SCRF", SpacedCommaRowForm];

makeSym["WS", WordSymbol];

makeSym["PCF", PathComposeForm];
makeSym["PTF", PathTranslateForm];
makeSym["PRF", PathReverseForm];
makeSym["PPWF", ParenPathWordForm];
makeSym["PWF", PathWordForm];
makeSym["EPWF", EmptyPathWordForm];
makeSym["PEPWF", ParenEmptyPathWordForm];
makeSym["WF", WordForm];
makeSym["DefEF", DefEqualForm];
makeSym["SynEF", SyntaxEqualForm];
makeSym["EF", EqualForm];
makeSym["NEF", NotEqualForm];
makeSym["PPWF", ParenPathWordForm];
makeSym["ParenF", ParenthesesForm];

makeSym["GMF", GroupMultiplicationForm];
makeSym["gMF", GroupoidMultiplicationForm];

makeSym["PGS", PathGroupoidSymbol];
makeSym["PQS", PathQuiverSymbol];
makeSym["FPQS", ForwardPathQuiverSymbol];
makeSym["BPQS", BackwardPathQuiverSymbol];

makeSym["PMS", PathMapSymbol];
makeSym["QS", QuiverSymbol];
makeSym["PS", PathSymbol];
makeSym["GS", GroupoidSymbol];
makeSym["QS", QuiverSymbol];
makeSym["MS", MatrixSymbol];
makeSym["PFSF", PartialFunctionSignatureForm];
makeSym["FSF", FunctionSignatureForm];
makeSym["AF", AppliedForm];

makeSym["EOF", ElementOfForm];

makeSym["SF", SymbolForm];

makeSym["QPPF", QuiverProductPolyForm];

makeSym["VIF",  VertexIndexForm];
makeSym["VLF",  VertexLabelForm];
makeSym["ELF",  EdgeLabelForm];
makeSym["ELF",  EdgeLabelForm];
makeSym["GIF",  GraphIndexForm];
makeSym["GLF",  GraphLabelForm];
makeSym["GTIF", GraphTooltipIndexForm];
makeSym["GTLF", GraphTooltipLabelForm];

makeSym["TAF", TextAndForm];
makeSym["CF", GeneralUtilities`CommaForm];

makeSym["DE", DirectedEdgeForm];
makeSym["UE", UndirectedEdgeForm];

makeSym["DEdgeF", DirectedEdgeForm];
makeSym["UEdgeF", UndirectedEdgeForm];

makeSym["CCR", ClickCopyRow];

makeSym["QuiverGeometry`Shortcuts`EGP" ExtendedGraphPlot];
makeSym["QuiverGeometry`Shortcuts`EG", ExtendedGraph];

makeSym["RF", RedForm];
makeSym["GF", GreenForm];
makeSym["BF", BlueForm];
makeSym["RGF", OrangeForm];
makeSym["RBF", PinkForm];
makeSym["GBF", TealForm];

makeSym["BSMF", BoundSignedMultiplicityFunctionForm];
makeSym["BMF", BoundMultiplicityFunctionForm];

makeSym["EGP", ExtendedGraphPlot];

makeSym["EG", ExtendedGraph];

makeSym["PS", ToPrettifiedString]