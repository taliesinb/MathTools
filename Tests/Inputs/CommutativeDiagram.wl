(* ::Package:: *)

(* ::Section:: *)
(*Hom functor*)


(* ::Subsubsection:: *)
(*Setup*)


LoadShortcuts["Categories"];


(* ::Section:: *)
(*Hom functors*)


TestRaster @ NamedDiagram["HomFunctor/CompositionSquare"]


TestRaster @ NamedDiagram["HomFunctor/CompositionHalfSquare"]


TestRaster @ NamedDiagram["HomFunctor/Variance"]


TestRaster @ NamedDiagram["HomFunctor/PullbackSquare"]


TestRaster @ NamedDiagram["HomFunctor/PullbackSquareElements"]


(* ::Section:: *)
(*Functors*)


TestRaster @ CommutativeDiagram[{
	{1,1} -> "A", {2, 1} -> $OB, Morphism[{1,2}, "F"]},
	CloneOptions -> {"FullFunctor", "Displacement" -> {Bottom, 1}, "AbsoluteSetback" -> {30,40}, "ExteriorLinkOptions" -> {ArrowColor -> $Gray, ArrowheadSize -> 10}},
	CloningFunction -> {FramedForm, FramedForm}, LabelPosition -> Above
]


TestRaster @ NamedDiagram["Composition/Triangle"]


TestRaster @ RainbowCategoryForm @ NamedDiagram["Composition/Triangle"]


TestRaster @ RainbowCategoryForm @ NamedDiagram["Composition/TriangleFunctorImage"]


(* ::Section:: *)
(*Naturality*)


TestRaster @ NamedDiagram["Naturality/Square"]


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


TestRaster @ NamedDiagram["Naturality/SquareGradient"]


(* ::Section:: *)
(*Moniodal categories*)


plainRainbowTreeRasters[e_] := {TestRaster @ e, TestRaster @ RainbowCategoryForm @ e, TestRaster @ MonoidalTreeForm[e, LabelFontSize -> 10]};


plainRainbowTreeRasters @ NamedDiagram["Monoidal/Associator"]


plainRainbowTreeRasters @ NamedDiagram["Monoidal/Pentagon"]


plainRainbowTreeRasters @ NamedDiagram["Monoidal/Hexagon1"]


plainRainbowTreeRasters @ NamedDiagram["Monoidal/Hexagon2"]


plainRainbowTreeRasters @ NamedDiagram["Monoidal/AssociatorIdentity"]


plainRainbowTreeRasters @ NamedDiagram["Monoidal/BraidingIdentity"]


plainRainbowTreeRasters @ NamedDiagram["Monoidal/BraidingSelfInverse"]


(* ::Section:: *)
(*Kan extensions*)


TestRaster @ NamedDiagram["KanExtension/AdjointTriple"]


TestRaster @ NamedDiagram["KanExtension/DiagramAdjointTriple"]


TestRaster @ NamedDiagram["KanExtension/KanExtension"]


TestRaster /@ {
	NamedDiagram["KanExtension/Left"],
	NamedDiagram["KanExtension/Right"]
}


TestRaster /@ {
	NamedDiagram["KanExtension/LeftUniversalProperty"],
	NamedDiagram["KanExtension/RightUniversalProperty"]
}


(* ::Section:: *)
(*Adjunction diagrams*)


TestRaster @ NamedDiagram["Adjunction/Pair"]


TestRaster @ NamedDiagram["Adjunction/PairUnitCounit"]


TestRaster /@ {
	NamedDiagram["Adjunction/LeftUnitCounitFunctorTriangle"],
	NamedDiagram["Adjunction/RightUnitCounitFunctorTriangle"]
}


TestRaster @ NamedDiagram["Adjunction/LeftGluedTriangles"]
TestRaster @ NamedDiagram["Adjunction/LeftFilledSquare"]

TestRaster @ NamedDiagram["Adjunction/RightGluedTriangles"]
TestRaster @ NamedDiagram["Adjunction/RightFilledSquare"]


TestRaster @ NamedDiagram["Adjunction/LeftTriangleImage"]


TestRaster @ NamedDiagram["Adjunction/RightTriangleImage"]


TestRaster @ NamedDiagram["Adjunction/LimitColimit"]


TestRaster @ NamedDiagram["Adjunction/Composition"]


(* ::Section:: *)
(*Monad algebras*)


TestRaster @ NamedDiagram["MonadAlgebra/Square"]


TestRaster /@ {
	NamedDiagram["MonadAlgebra/MultiplicationSquare"],
	NamedDiagram["MonadAlgebra/UnitTriangle"]
}


TestRaster @ NamedDiagram["MonadAlgebra/SquareRainbow"]


(* ::Section:: *)
(*Double categories*)


TestRaster @ NamedDiagram["DoubleCat/Morphism"]


TestRaster @ NamedDiagram["DoubleCat/ProArrow"]


(* ::Section:: *)
(*Ends*)


TestRaster @ NamedDiagram["End/Wedge"]


TestRaster @ NamedDiagram["End/Cowedge"]


(* ::Section:: *)
(*Monad*)


TestRaster @ NamedDiagram["Monad/MultiplicationSquare"]


TestRaster @ NamedDiagram["Monad/UnitTriangles"]



TestRaster /@ {
	NamedDiagram["Monad/MultiplicationSquareImage"],
	NamedDiagram["Monad/UnitTrianglesImage"]
}


TestRaster /@ {
	NamedDiagram["Monad/ComultiplicationSquare"],
	NamedDiagram["Monad/CounitTriangles"]
}


(* ::Section:: *)
(*Whiskering*)


TestRaster @ NamedDiagram["Whiskering/Left"]


TestRaster @ NamedDiagram["Whiskering/LeftImage"]


TestRaster @ NamedDiagram["Whiskering/LeftResult"]


(* ::Section:: *)
(*Monoidal Functor*)


mfMtf[e_] := TestRaster /@ {e, MonoidalTreeForm[e, LabelFontSize -> 10, FontSize -> 15]};

mfMtf @ NamedDiagram["MonoidalFunctor/AssociatorSquare"]

mfMtf @ NamedDiagram["MonoidalFunctor/RightUnitorSquare"]

mfMtf @ NamedDiagram["MonoidalFunctor/LeftUnitorSquare"]

mfMtf @ NamedDiagram["MonoidalFunctor/BraidingSquare"]