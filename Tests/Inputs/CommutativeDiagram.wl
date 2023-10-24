(* ::Package:: *)

(* ::Section:: *)
(*Hom functor*)

(* ::Subsubsection:: *)
(*Setup*)

LoadShortcuts["Categories"]

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

hom[a_, b_] := ExplicitHomForm[$CC, a, b];
vhom[a_, b_] := CompactHomForm[a, b];
cohom[a_, b_] := CompactCovariantHomFunctorForm[a][b];
contrahom[a_, b_] := CompactContravariantHomFunctorForm[b][a];

cohom2[a_, b_] := Morphism[{Placed[CompactCovariantHomFunctorForm[a][b], Outwards], Placed[Row[{"\[Square]", b}], Inwards]}];
contrahom2[a_, b_] := Morphism[{Placed[CompactContravariantHomFunctorForm[b][a], Outwards], Placed[Row[{a, "\[Square]"}], Inwards]}];


(* ::Subsubsection:: *)
(*Commutative square*)


TestRaster @ CommutativeSquare[
	{$OA, $OB, $OApr, $OBpr},
	{LineMorphism[$Ak, -1], 
	 LineMorphism[SpacedCompositionForm[$Aj, $Ak, $Al], -2], 
	 Reversed @ LineMorphism[$Aj, -1], LineMorphism[$Al, -1]},
	$homOptions, 
	LabelRectification -> False,
	DiagramScaling -> .8
]


(* ::Subsubsection:: *)
(*Commutative half square*)


TestRaster @ CommutativeSquare[
	{$OA, $OB, $OApr, $OBpr},
	{LineMorphism[$Ak, -1], 
	 Null, 
	 Reversed @ LineMorphism[$Aj, -1], LineMorphism[$Al, -1]},
	$homOptions, 
	LabelRectification -> False,
	DiagramScaling -> .5, FlipY -> True
]


(* ::Subsubsection:: *)
(*Contra and covariant, pullbacks*)


TestRaster @ CommutativeDiagram[{
	{1,1} -> vhom[$OA,$OB],
	{2,1} -> vhom[$OApr,$OB],  Morphism[{1, 2}, contrahom[$Aj, $OB]],
	{0,1} -> vhom[$OA, $OBpr], Morphism[{1, 3}, cohom[$OA, $Al]],
		
	MapsToMorphism[MorphismCoordinates[{"A"\[DirectedEdge]"B", "A'" \[DirectedEdge] "B2"}]],
	MapsToMorphism[MorphismCoordinates[{"A"\[DirectedEdge]"B", "A2" \[DirectedEdge] "B'"}]],
	
	Morphism[{"A", "hom(A, B)"}, None, "Element"],
	Morphism[{"A'", "hom(A', B)"}, None, "Element"],
	Morphism[{"A2", "hom(A, B')"}, None, "Element"],
	
	y1 = 1.5; y2 = 2.15; Setback -> 10,
	{1,y1} -> $OA,   {1,y2} -> $OB, LineMorphism[{"A", "B"}, $Ak, {0, -1}],
	{2,y1} -> $OApr, {2,y2} -> $OB, LineMorphism[{"A'", "B2"}, SpacedCompositionForm[$Aj, $Ak], {-10, -2}],
	{0,y1} -> $OA,   {0,y2} -> $OBpr, LineMorphism[{"A2", "B'"}, SpacedCompositionForm[$Ak, $Al], {10, -2}]

}, $homOptions, LabelPosition -> Outwards, Setback -> 30, FlipX -> True,
DiagramScaling -> {1.1, 1}]


(* ::Subsubsection:: *)
(*Pullback square*)


TestRaster @ CommutativeSquare[
	{vhom[$OA,$OB],       vhom[$OApr,$OB], 
	 vhom[$OA,$OBpr],     vhom[$OApr,$OBpr]},
	{contrahom[$Aj, $OB], contrahom[$Aj, $OBpr], 
	cohom[$OA, $Ag],      cohom[$OApr, $Ag]
	}, 
	$homOptions
]


(* ::Subsubsection:: *)
(*Element square*)


TestRaster @ CommutativeSquare[
	{Cloned[vhom[$OA,$OB], $Ak],              Cloned[vhom[$OApr,$OB], SpacedCompositionForm[$Ak, $Aj]], 
	 Cloned[vhom[$OA,$OBpr], SpacedCompositionForm[$Al, $Ak]], Cloned[vhom[$OApr,$OBpr], SpacedCompositionForm[$Al, $Ak, $Aj]]},
	{contrahom2[$Aj, $OB],                    contrahom2[$Aj, $OBpr], 
	 cohom2[$OA, $Ag],                        cohom2[$OApr, $Ag]}, 
	$homOptions,
	GraphicsScale -> 250
]


(* ::Section:: *)
(*Functors*)


(* ::Subsubsection:: *)
(*Functor mapping*)


TestRaster @ CommutativeDiagram[{
	{1,1} -> "A", {2, 1} -> $OB, Morphism[{1,2}, "F"]},
	CloneOptions -> {"FullFunctor", "Displacement" -> {Bottom, 1}, "AbsoluteSetback" -> {30,40}, "ExteriorLinkOptions" -> {ArrowColor -> $Gray, ArrowheadSize -> 10}},
	CloningFunction -> {FramedForm, FramedForm}, LabelPosition -> Above
]


(* ::Subsubsection:: *)
(*Composition triangle*)


TestRaster @ CompositionTriangleDiagram[
	{$OX, $OY, $OZ}, 
	{$Af, $Ag, SpacedCompositionForm[$Ag,$Af]}
]


(* ::Subsubsection:: *)
(*Colored composition triangle*)


TestRaster @ RainbowCategoryForm @ CompositionTriangleDiagram[
	{$OX, $OY, $OZ}, 
	{$Af, $Ag, SpacedCompositionForm[$Ag,$Af]},
	DefaultMorphism -> LineMorphism
]


(* ::Subsubsection:: *)
(*Composition triangle with image*)


TestRaster @ UsingExplicitAppliedForm @ RainbowCategoryForm @ CompositionTriangleDiagram[
	{$OX, $OY, $OZ}, 
	{LineMorphism[$Af,-1], LineMorphism[$Ag,-1], LineMorphism[SpacedCompositionForm[$Ag,$Af],-2]},
	CloneOptions -> {"FullFunctor", "ExteriorLinkOptions" -> {ArrowheadSize -> 5}},
	GraphicsScale -> 250, CloningFunction -> {FramedForm, FramedForm}
]


(* ::Section:: *)
(*Naturality*)


(* ::Subsubsection:: *)
(*Naturality square*)


TestRaster @ CommutativeSquare[
	{$FunF[$Ox], $FunG[$Ox], $FunF[$Oy], $FunG[$Oy]},
	{$NTeta[$Ox], $NTeta[$Oy], $FunF[$Af], $FunG[$Af],
	 ArrowDiagram[{0.3, 1} -> $Ox, $Oy, Morphism[$Af, LabelPosition -> Left]]}
]


(* ::Subsubsection:: *)
(*Pullback square with preimage*)


$Odisk = "\[FilledCircle]"; $Osqr = "\[EmptyCircle]";
TestRaster @ CommutativeSquare[
	{$FunF[$Ox], $FunG[$Ox], $FunF[$Oy], $FunG[$Oy]},
	{$NTeta[$Ox], $NTeta[$Oy], $FunF[$Af], $FunG[$Af],
	 $x = 0.6;{$x, $y = 0.5} -> $Ox, {$x, $y + 1} -> $Oy, Morphism[{"x", "y"}, LabelPosition -> Left],
	 ArrowheadSize -> 8, 
	 MapsToMorphism[{"x", "Fx"}], MapsToMorphism[{"x", "Gx"}],
	 MapsToMorphism[{"y", "Fy"}], MapsToMorphism[{"y", "Gy"}]
	 },
	 ColorRules -> {"Functors", "ColoringFunctors", _ -> $Purple},
	 SymbolReplacements -> {$Ox -> $Odisk, $Oy -> $Osqr},
	 MorphismColors -> "Gradient", DebugBounds -> False
]


(* ::Subsubsection:: *)
(*Gradient naturality square*)


TestRaster @ CommutativeSquare[
	{$FunF[$Ox], $FunG[$Ox], $FunF[$Oy], $FunG[$Oy]},
	{$NTeta[$Ox], $NTeta[$Oy], $FunF[$Af], $FunG[$Af],
	 ArrowDiagram[{0.3, 1} -> $Ox, $Oy, Morphism[$Af, LabelPosition -> Left]]},
	 ColorRules -> "ColoringFunctors",
	 MorphismColors -> "Gradient"
]


(* ::Section:: *)
(*Moniodal category*)


(* ::Subsubsection:: *)
(*Associator*)


plainRainbowTreeRasters[e_] := {TestRaster @ e, TestRaster @ RainbowCategoryForm @ e, TestRaster @ MonoidalTreeForm @ e};

plainRainbowTreeRasters @ CommutativeDiagram[{
	{0, 0} -> "(AB)C" -> MPF[MPF[$OA, $OB], $OC], {1.3,0} -> "A(BC)" -> MPF[$OA, MPF[$OB, $OC]],
	Morphism["(AB)C" \[DirectedEdge] "A(BC)", AssociatorForm[$OA, $OB, $OC]]}, 
	FontSize -> 14
]


(* ::Subsubsection:: *)
(*Pentagon law*)


plainRainbowTreeRasters @ CommutativePentagon[{
	TMPF[TMPF[$OA, $OB], TMPF[$OC, $OD]],
	TMPF[$OA, TMPF[$OB, TMPF[$OC, $OD]]],
	TMPF[$OA, TMPF[TMPF[$OB, $OC], $OD]],
	TMPF[TMPF[$OA, TMPF[$OB, $OC]], $OD],
	TMPF[TMPF[TMPF[$OA, $OB], $OC], $OD]}, {
	AssociatorForm[$OA, $OB, TMPF[$OC, $OD]], 
	Reversed @ MPF[OneArrow[$OA], AssociatorForm[$OB, $OC, $OD]], 
	Reversed @ AssociatorForm[$OA, TMPF[$OB, $OC], $OD], 
	Reversed @ MPF[AssociatorForm[$OA, $OB, $OC], $1AD],
	AssociatorForm[TMPF[$OA, $OB], $OC, $OD]
}] 	


(* ::Subsubsection:: *)
(*Interaction of braiding with associator*)


plainRainbowTreeRasters @ CommutativeHexagon[{
	MPF[MPF[$OA,$OB],$OC],
	MPF[$OA,MPF[$OB,$OC]],
	MPF[MPF[$OB,$OC],$OA],
	MPF[$OB,MPF[$OC,$OA]],
	MPF[$OB,MPF[$OA,$OC]],
	MPF[MPF[$OB,$OA],$OC]}, {
	AssociatorForm[$OA, $OB, $OC],
	BraidingForm[$OA, TMPF[$OB, $OC]],
	AssociatorForm[$OB, $OC, $OA],
	Reversed @ MPF[OneArrow[$OB], BraidingForm[$OA, $OC]],
	Reversed @ AssociatorForm[$OB, $OA, $OC],
	Reversed @ MPF[BraidingForm[$OA, $OB], OneArrow @ $OC]
}]


plainRainbowTreeRasters @ CommutativeHexagon[{
	MPF[$OA,MPF[$OB,$OC]],
	MPF[MPF[$OA,$OB],$OC],
	MPF[$OC,MPF[$OA,$OB]],
	MPF[MPF[$OC,$OA],$OB],
	MPF[MPF[$OA,$OC],$OB],
	MPF[$OA,MPF[$OC,$OB]]}, {
	InverseForm @ AssociatorForm[$OA, $OB, $OC], 
	BraidingForm[TMPF[$OA, $OB], $OC], 
	InverseForm @ AssociatorForm[$OA, $OB, $OC], 
	Reversed @ MPF[BraidingForm[$OA, $OC], OneArrow[$OB]], 
	Reversed @ InverseForm @ AssociatorForm[$OA, $OB, $OC], 
	Reversed @ MPF[OneArrow[$OA], BraidingForm[$OB, $OC]]}
]


(* ::Subsubsection:: *)
(*Interaction of unitors with identity and associator*)


plainRainbowTreeRasters @ InTriangleDiagram[
	{MPF[MPF[$OA, $OI], $OB], MPF[$OA, MPF[$OI, $OB]], MPF[$OA, $OB]},
	{AssociatorForm[$OA,$OI,$OB], MPF[RightUnitorForm[$OA], $1AB], MPF[$1AA, LeftUnitorForm[$OB]]},
	FontSize -> 18, DiagramScaling -> {1.4, 1}
]


(* ::Subsubsection:: *)
(*Interaction of unitors with identity and braiding*)


plainRainbowTreeRasters @ InTriangleDiagram[
	{MPF[$OA, $OI], MPF[$OI, $OA], $OA},
	{BraidingForm[$OA, $OI], LeftUnitorForm[$OA], RightUnitorForm[$OA]}
]


(* ::Subsubsection:: *)
(*Invertability of braiding*)


plainRainbowTreeRasters @ InTriangleDiagram[
	{MPF[$OA, $OB], MPF[$OA, $OB], MPF[$OB, $OA]},
	{EqualityMorphism[], BraidingForm[$OA, $OB], Reversed @ BraidingForm[$OB, $OA]}
]


(* ::Section:: *)
(*Kan extensions*)


(* ::Subsubsection:: *)
(*Left and right Kan extension*)


TestRaster @ AdjointTripleDiagram[
	{CompactFunctorCategoryForm[$CD,$CC], CompactFunctorCategoryForm[$CE,$CC]},
	{LeftKanExtensionForm[$FunF], RightKanExtensionForm[$FunF], CompactFunctorCategoryForm[$FunF,$CC],
	ArrowDiagram[{2,1} -> $CD, $CE, $FunF]
	},LabelFontSize -> 16, "AdjointSetback" -> 8
]


(* ::Subsubsection:: *)
(*Left and right Kan extensions for diagram*)


TestRaster @ AdjointTripleDiagram[
	{CompactFunctorCategoryForm[$CD,$CC], CompactFunctorCategoryForm[1,$CC]},
	{LeftKanExtensionForm[$FunP], RightKanExtensionForm[$FunP], CompactFunctorCategoryForm[$FunP,$CC],
	ArrowDiagram[{2,1} -> $CD, 1, $FunP]
	},LabelFontSize -> 16, "AdjointSetback" -> 8
]


(* ::Subsubsection:: *)
(*???*)


TestRaster @ InTriangleDiagram[
	{$CD, $CC, $CE}, 
	{$FunD, $FunF, Reversed @ UniqueMorphism @ $FunE}, Left
]


(* ::Subsubsection:: *)
(*Left and right Kan extension double*)


TestRaster /@ {
	InTriangleDiagram[
		{$CD, $CC, $CDpr}, 
		{$FunF, $FunP, Reversed @ LeftKanExtensionForm[$FunP][$FunF],
		 DoubleMorphism["F" \[DirectedEdge] "D'", $NTeta, Setback -> {15,30}]}
	],
	InTriangleDiagram[
		{$CD, $CC, $CDpr}, 
		{$FunF, $FunP, Reversed @ RightKanExtensionForm[$FunP][$FunF],
		 DoubleMorphism["D'" \[DirectedEdge] "F", $NTeps, Setback -> {30,15}]}
	]
}


(* ::Subsubsection:: *)
(*Left and right Kan extension double*)


TestRaster /@ {
	InTriangleDiagram[
		{$CD, $CC, $CE}, 
		{$FunF, $FunP, Reversed @ UniqueMorphism[$FunE, LabelPosition -> Above],
		 Morphism[ElbowCurve[{"E", "C"}, -.3], $FunEpr],
		 DoubleMorphism[2 \[DirectedEdge] 4, Setback -> 5], Text["left", {1.5,-2.25}]},
		Left
	],
	InTriangleDiagram[
		{$CD, $CC, $CE}, 
		{$FunF, $FunP, Reversed @ UniqueMorphism[$FunE, LabelPosition -> Above],
		 Morphism[ElbowCurve[{"E", "C"}, -.3], $FunEpr],
		 DoubleMorphism[4 \[DirectedEdge] 2, Setback -> 5], Text["right", {1.5,-2.25}]},
		Left, Origin -> {1.75, 0}
	]
}


(* ::Section:: *)
(*Adjunction diagrams*)


(* ::Subsubsection:: *)
(*Left and right adjunction*)


TestRaster @ AdjunctionDiagram[{$CC, $CD}, {$FunL, $FunR}]


(* ::Subsubsection:: *)
(*Left and right adjunction with unit and counit double arrows*)


TestRaster @ AdjunctionDiagram[
	{{1,1} -> $CC, {3, 1} -> $CD}, 
	{$FunL, $FunR, 
	 {2,1} -> "M" -> None,
	 LabelPosition -> {0, -0.6},
	 Morphism[{1, "M"}, {1.15 -> $NTeta}, "DoubleArrow", Setback -> {15, 50}],
	 Morphism[{"M", 2}, {-0.15 -> $NTeps}, "DoubleArrow", Setback -> {50, 15}]}
]


(* ::Subsubsection:: *)
(*Adjunction triangles*)


TestRaster /@ {
	InTriangleDiagram[
		{$FunL, CompositionForm[$FunL, $FunR, $FunL], $FunL},
		{HCF[OneArrow[$FunL], $NTeta], EqualityMorphism[], HCF[$NTeps, OneArrow @ $FunL]},
		Right
	],
	InTriangleDiagram[
		{CompositionForm[$FunR, $FunL, $FunR], $FunR, $FunR},
		{Reversed @ HCF[$NTeta, OneArrow[$FunR]], HCF[OneArrow @ $FunR, $NTeps], EqualityMorphism[]},
		Left, Origin -> {2, 0}
	]
}


(* ::Subsubsection:: *)
(*Double triangles*)


gluedTriangles[a_, b_, f_, g_, x_, y_, revDbl_] := With[
	{rev = If[revDbl, Reverse, Identity], lpos = If[revDbl, 0.4, 0.5]}, CommutativeDiagram[{
	{1,1} -> "C1" -> a, {2,1} -> "D1" -> b, {2,2} -> "C2" -> a, {3, 2} -> "D2" -> b,
	EqualityMorphism[{1, 3}], EqualityMorphism[{2, 4}],
	Morphism[{1, 2}, f], LabelPosition -> BottomRight,  Morphism[{2, 3}, g], Morphism[{3, 4}, f], LabelSpacing -> 0,
	DoubleMorphism[rev @ {MorphismCoordinates[1], ObjectCoordinates[2]}, {lpos -> x}, Setback -> rev[{8, 16}]],
	DoubleMorphism[rev @ {ObjectCoordinates[3], MorphismCoordinates[2]}, {lpos -> y}, Setback -> rev[{16, 8}]]}
]];

filledSquare[a_, b_, f_, isRev_, opts___Rule] := CommutativeDiagram[{
	{1,1} -> "C" -> a, {2,1} -> "C2" -> None, {2,2} -> "D2" -> None, {3, 2} -> "D" -> b}, 
	{Morphism[RollingCurve @ {"C", "C2", "D"}, f], 
	 Morphism[RollingCurve @ {"C", "D2", "D"}, f], 
	 DoubleMorphism[HorizontalCurve[ObjectCoordinates["Center"], {-1,1}*.4*If[isRev, -1, 1]], IdentityFunction,
	 LabelPosition -> Above]},
	LabelPosition -> Outwards, opts
];


TestRaster @ gluedTriangles[$OC, $OD, $FunL, $FunR, $NTeta, $NTeps, False]
TestRaster @ filledSquare[$OC, $OD, $FunL, False, Origin -> {3, 0}]

TestRaster @ gluedTriangles[$OD, $OC, $FunR, $FunL, $NTeps, $NTeta, True]
TestRaster @ filledSquare[$OD, $OC, $FunR, True, Origin -> {3, 0}]


(* ::Subsubsection:: *)
(*Triangles and preimage*)


TestRaster @ OutTriangleDiagram[
	{$FunL[$Oc], $FunL[$FunR[$Od]], $Od},
	{UniqueMorphism @ $FunL[$Ag], $Af, Morphism[$NTeps[$Od], LabelPosition -> Top], 
	 ArrowDiagram[{0.5,1}-> $Oc, $FunR[$Od], UniqueMorphism[$Ag, LabelPosition -> Left]]}, Left
]


TestRaster @ InTriangleDiagram[
	{$Oc, $FunR[$FunL[$Oc]], $FunR[$Od]},
	{Morphism[$NTeps[$Od], LabelPosition -> Bottom], $Ag, UniqueMorphism @ $FunR[$Af],
	ArrowDiagram[{2.75,1} -> $FunL[$Oc], $Od, $Af]}, Right
]


(* ::Subsubsection:: *)
(*Limit and colimit*)


TestRaster @ AdjointTripleDiagram[
	{CompactFunctorCategoryForm[$CD,$CC], $CC},
	{Padded[ColimitFunction, {0,1}], LimitFunction, DiagonalFunctorForm[$CD]}
]


(* ::Subsubsection:: *)
(*Composing adjunctions*)


TestRaster @ CommutativeDiagram[{
	AdjunctionDiagram[{$CC, $CD}, {$FunL, $FunR}],
	AdjunctionDiagram[{Invisible @ $CD, $CE}, {$FunLpr, $FunRpr}, Origin -> {1, 0}]
}]


(* ::Section:: *)
(*Monad algebras*)


TestRaster @ CommutativeSquare[
	{$FunF[$OX], $FunF[$OY], $OX, $OY},
	{$FunF[$Af], $Af, $FunA, $FunB}
]


TestRaster /@ {
	CommutativeSquare[
		{FunctorPowerForm[$FunT, 2] @ $Ox, $FunT @ $Ox, $FunT @ $Ox, $Ox},
		{$FunT @ $Ah, $Ah, $NTmu[$Ox], $Ah}, LabelFontSize -> 18
	],
	InTriangleDiagram[
		{$Ox, $FunT[$Ox], $Ox},
		{$NTeta[$Ox], EqualityMorphism[], $Ah},
		Right, Origin -> {1.5,0}, LabelFontSize -> 18
	]
}


TestRaster @ CommutativeSquare[
	{$FunT[$Ox], $FunT[$Oy], $Ox, $Oy},
	{$FunT[$Af], $Af, $Ah, $Aj},
	ColorRules -> {$Ox -> RedForm, $Oy -> BlueForm, $Af -> DarkPinkForm}
]


(* ::Section:: *)
(*Double categories*)


TestRaster @ CommutativeSquare[
	{$Ox0, $Ox1, $Oy0, $Oy1},
	{$Af, $Ag, $Aalpha0, $Aalpha1, 
	DoubleMorphism[{"f", "g"}, "\[Phi]", Setback -> 30]}
]


TestRaster @ CommutativeSquare[
	{$OA, $OB, $OApr, $OBpr},
	{{$AM, "Proarrow"}, {$AN, "Proarrow"}, $Af, $Ag,
	 DoubleMorphism[{1, 4}, $Aalpha, Setback -> 30]}
]


(* ::Section:: *)
(*Ends*)


(* ::Subsubsection:: *)
(*Wedge*)


TestRaster @ CommutativeSquare[
	{$Ow, $FunF[$Od, $Od], $FunF[$Oc, $Oc], $FunF[$Oc, $Od]},
	{Subscript[$Ae, $Od], 
	 TightAppliedForm[$FunF, $Oc, $Af],
	 Subscript[$Ae, $Oc],
	 TightAppliedForm[$FunF, $Af, $Od]},
	 LabelFontSize -> 16, GraphicsScale -> 140
]


(* ::Subsubsection:: *)
(*Cowedge*)


TestRaster @ CommutativeSquare[
	{$Ow, $FunF[$Od, $Od], $FunF[$Oc, $Oc], $FunF[$Oc, $Od]},
	{Subscript[$Ae, $Od], 
	 TightAppliedForm[$FunF, $Oc, $Af],
	 Subscript[$Ae, $Oc],
	 TightAppliedForm[$FunF, $Af, $Od]},
	LabelFontSize -> 16,
	FlipX -> True, FlipY -> True, 
	ArrowPathReversed -> True,
	GraphicsScale -> 140
]


(* ::Section:: *)
(*Monad*)


(* ::Subsubsection:: *)
(*Monoidal multiplication*)


TestRaster @ CommutativeSquare[
	{HCF[$FunT,$FunT,$FunT], HCF[$FunT,$FunT], HCF[$FunT,$FunT], $FunT},
	{HCF[$FunT,$NTmu], $NTmu, HCF[$NTmu, $FunT], $NTmu}
]


(* ::Subsubsection:: *)
(*Monoidal unit*)


TestRaster @ DoubleTriangleDiagram[
	{$FunT, HCF[$FunT,$FunT], $FunT, $FunT},
	{HCF[$NTeta,$FunT], HCF[$FunT, $NTeta], {None, "Equality"}, $NTmu, {None, "Equality"}}
]



(* ::Subsubsection:: *)
(*Pointed*)


TestRaster /@ {
	CommutativeSquare[
		{FunctorPowerForm[$FunT,3][$OX], FunctorPowerForm[$FunT,2][$OX], FunctorPowerForm[$FunT,2][$OX], $FunT[$OX]},
		{$FunT[$NTmu[$OX]], $NTmu[$OX], $NTmu[$FunT[$OX]], $NTmu[$OX]},
		LabelFontSize -> 15
	],
	DoubleTriangleDiagram[
		{$FunT[$OX], PowerForm[$FunT,2][$OX], $FunT[$OX], $FunT[$OX]},
		{$NTeta[$FunT[$OX]], $FunT[$NTeta[$OX]], {None, "Equality"}, $NTmu[$OX], {None, "Equality"}},
		LabelFontSize -> 15,
		Origin -> {1.75, 0}
	]
}


(* ::Subsubsection:: *)
(*Comonad*)


TestRaster /@ ({
	CommutativeSquare[
		{HCF[$FunT,$FunT,$FunT], HCF[$FunT,$FunT], HCF[$FunT,$FunT], $FunT},
		{Padded[HCF][$FunT,$NTmu], $NTmu, Padded[HCF][$NTmu, $FunT], $NTmu},
		ArrowPathReversed -> True
	],
	DoubleTriangleDiagram[
		{$FunT, HCF[$FunT,$FunT], $FunT, $FunT},
		{Padded[HCF][$NTeta,$FunT], Padded[HCF][$FunT, $NTeta], EqualityMorphism[], $NTmu, {None, "Equality"}},
		Origin -> {1.75, 0}, ArrowPathReversed -> True
	]
} /. {$NTmu -> $NTdelta, $NTeta -> $NTeps})

(* ::Section:: *)
(*Whiskering*)

$FunHF = FunctorSymbol[TightCompositionForm[$FunH, $FunF]];
$FunHG = FunctorSymbol[TightCompositionForm[$FunH, $FunG]];
$NTHeta = NaturalTransformationSymbol[CompositionForm[$FunH, $NTeta]];
$whiskeringOpts = Sequence[
	ColorRules -> {$Oc -> $Black, $NTeta -> RBF, $NTHeta -> $Gray, $FunHF -> RGF, $FunHG -> GBF, $FunF -> RF, $FunG -> BF, $FunH -> GF, $NTeta -> 0}
];
TestRaster @ CommutativeDiagram[{
	{1,1} -> "C" -> $CC, {2,1} -> "D" -> $CD, {3,1} -> "E" -> $CE,
	Morphism[TrapezoidCurve[{"C","D"}, .2], $FunF],
	Morphism[TrapezoidCurve[{"C", "D"}, -.2], $FunG],
	Morphism[{"D", "E"}, $FunH],
	DoubleMorphism[{1, 2}, $NTeta, Setback -> 5]},
	LabelFontSize -> 18, LabelPosition -> "Outer", LabelPosition -> AwayFrom[{0,-1}],
	$whiskeringOpts,
	MorphismColors -> Inherited
]


TestRaster @ CommutativeDiagram[{
	{1, 0} -> "c" -> Sized[$Oc, {30, Automatic}],
	{2, -.35} -> "Fc" -> $FunF[$Oc],
	{3, -.35} -> "HFc" -> $FunHF[$Oc],
	{2, .35} -> "Gc" -> $FunG[$Oc],
	{3, .35} -> "HGc" -> $FunHG[$Oc],

	MapsToMorphism[{"c", "Fc"}, ArrowColor -> $Red],
	MapsToMorphism[{"c", "Gc"}, ArrowColor -> $Blue],

	MapsToMorphism[{"Fc", "HFc"}, ArrowColor -> $Green],
	MapsToMorphism[{"Gc", "HGc"}, ArrowColor -> $Green],

	Morphism[{"Fc", "Gc"}, $NTeta[$Oc], ArrowColor -> $Pink],
	Morphism[{"HFc", "HGc"}, $NTHeta[$Oc], ArrowColor -> $Gray]},
	LabelPosition -> Right,
	$whiskeringOpts
]


TestRaster @ CommutativeDiagram[{
	{1,3} -> "C" -> $CC, {3,3} -> "E" -> $CE,
	MorphismArrow[TrapezoidCurve[{"C","E"}, .3], $FunHF, ArrowColor -> $Orange, LabelPosition -> Above],
	MorphismArrow[TrapezoidCurve[{"C","E"}, -.3], $FunHG, ArrowColor -> $Teal, LabelPosition -> Below],
	DoubleMorphism[{1, 2}, $NTHeta, ArrowColor -> $Gray]},
	MorphismColors -> Inherited, $whiskeringOpts
]