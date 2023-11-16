LoadShortcuts["Categories"]

(**************************************************************************************************)

$homColorRules = {
	$OA -> RF, $OB -> BF, $OApr -> RGF, $OBpr -> GF,
	$Af -> {RF, RGF}, $Ag -> {BF, GF},
	$Ak -> {RF, BF}, $Akpr -> {RGF, BF},
	$Aj -> {RGF, RF},
	$Al -> {BF, GF},
	$OI -> White
};

$homOptions = Sequence[
	TextModifiers -> ObjectArrowIconForm,
	ColorRules -> $homColorRules,
	CloneOptions -> "Element",
	LabelOrientation -> Aligned
];

(**************************************************************************************************)

NamedDiagram["HomFunctor/CompositionSquare"] := CommutativeSquare[
	{$OA, $OB, $OApr, $OBpr},
	{LineMorphism[$Ak],
	 LineMorphism[SpacedCompositionForm[$Al, $Ak, $Aj]],
	 Reversed @ LineMorphism[$Aj], LineMorphism[$Al]},
	$homOptions,
	LabelRectification -> False,
	DiagramScaling -> .8
];

(**************************************************************************************************)

NamedDiagram["HomFunctor/CompositionHalfSquare"] := CommutativeSquare[
	{$OA, $OB, $OApr, $OBpr},
	{LineMorphism[$Ak],
	 Null,
	 Reversed @ LineMorphism[$Aj], LineMorphism[$Al]},
	$homOptions,
	LabelRectification -> False,
	DiagramScaling -> .5, FlipY -> True
];

(**************************************************************************************************)

hom[a_, b_] := ExplicitHomForm[$CC, a, b];
vhom[a_, b_] := CompactHomForm[a, b];
cohom[a_, b_] := CompactCovariantHomFunctorForm[a][b];
contrahom[a_, b_] := CompactContravariantHomFunctorForm[b][a];

cohom2[a_, b_] := Morphism[{Placed[CompactCovariantHomFunctorForm[a][b], Outwards], Placed[Row[{"\[Square]", b}], Inwards]}];
contrahom2[a_, b_] := Morphism[{Placed[CompactContravariantHomFunctorForm[b][a], Outwards], Placed[Row[{a, "\[Square]"}], Inwards]}];

(**************************************************************************************************)

NamedDiagram["HomFunctor/Variance"] := CommutativeDiagram[{
	{1,1} -> vhom[$OA,$OB],
	{2,1} -> vhom[$OApr,$OB],  Morphism[{1, 2}, contrahom[$Aj, $OB]],
	{0,1} -> vhom[$OA, $OBpr], Morphism[{1, 3}, cohom[$OA, $Al]],

	MapsToMorphism[MorphismCoordinates[{"A"\[DirectedEdge]"B", "A'" \[DirectedEdge] "B2"}]],
	MapsToMorphism[MorphismCoordinates[{"A"\[DirectedEdge]"B", "A2" \[DirectedEdge] "B'"}]],

	Morphism[{"A", "hom(A, B)"}, None, "Element"],
	Morphism[{"A'", "hom(A', B)"}, None, "Element"],
	Morphism[{"A2", "hom(A, B')"}, None, "Element"],

	y1 = 1.5; y2 = 2.15; Setback -> 10,
	{1,y1} -> $OA,   {1,y2} -> $OB, LineMorphism[{"A", "B"}, $Ak],
	{2,y1} -> $OApr, {2,y2} -> $OB, LineMorphism[{"A'", "B2"}, SpacedCompositionForm[$Ak, $Aj], {-10, 0}],
	{0,y1} -> $OA,   {0,y2} -> $OBpr, LineMorphism[{"A2", "B'"}, SpacedCompositionForm[$Al, $Ak], {10, 0}]

}, $homOptions, LabelPosition -> Outwards, Setback -> 30, FlipX -> True,
DiagramScaling -> {1.1, 1}];

(**************************************************************************************************)

NamedDiagram["HomFunctor/PullbackSquare"] := CommutativeSquare[
	{vhom[$OA,$OB],       vhom[$OApr,$OB],
	 vhom[$OA,$OBpr],     vhom[$OApr,$OBpr]},
	{contrahom[$Aj, $OB], contrahom[$Aj, $OBpr],
	cohom[$OA, $Ag],      cohom[$OApr, $Ag]
	},
	$homOptions
];

(**************************************************************************************************)

NamedDiagram["HomFunctor/PullbackSquareElements"] := CommutativeSquare[
	{Cloned[vhom[$OA,$OB], $Ak],              Cloned[vhom[$OApr,$OB], SpacedCompositionForm[$Ak, $Aj]],
	 Cloned[vhom[$OA,$OBpr], SpacedCompositionForm[$Al, $Ak]], Cloned[vhom[$OApr,$OBpr], SpacedCompositionForm[$Al, $Ak, $Aj]]},
	{contrahom2[$Aj, $OB],                    contrahom2[$Aj, $OBpr],
	 cohom2[$OA, $Ag],                        cohom2[$OApr, $Ag]},
	$homOptions,
	GraphicsScale -> 250
];

