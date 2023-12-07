LoadShortcuts["Categories"]

(**************************************************************************************************)

$monadToComonad = {$NTmu -> $NTdelta, $NTeta -> $NTeps};
comonadDiagram[name_, opts___] :=
  NamedDiagram[name, opts, ArrowPathReversed -> True, SymbolReplacements -> $monadToComonad];

(**************************************************************************************************)

NamedDiagram["Monad/MultiplicationSquare"] := CommutativeSquare[
	{HC[$FunT,$FunT,$FunT], HC[$FunT,$FunT], HC[$FunT,$FunT], $FunT},
	{HC[$FunT,$NTmu], $NTmu, HC[$NTmu, $FunT], $NTmu}
];

NamedDiagram["Monad/ComultiplicationSquare"] :=
  comonadDiagram["Monad/MultiplicationSquare"];

(**************************************************************************************************)

NamedDiagram["Monad/UnitTriangles"] := DoubleTriangleDiagram[
	{$FunT, HC[$FunT,$FunT], $FunT, $FunT},
	{HC[$NTeta,$FunT], HC[$FunT, $NTeta], {None, "Equality"}, Customized[$NTmu, LabelPosition -> Right], {None, "Equality"}}
]

NamedDiagram["Monad/CounitTriangles"] :=
  comonadDiagram["Monad/UnitTriangles"];

(**************************************************************************************************)

NamedDiagram["Monad/MultiplicationSquareImage"] := CommutativeSquare[
	{FunctorPowerForm[$FunT,3][$OX], FunctorPowerForm[$FunT,2][$OX], FunctorPowerForm[$FunT,2][$OX], $FunT[$OX]},
	{$FunT[$NTmu[$OX]], $NTmu[$OX], $NTmu[$FunT[$OX]], $NTmu[$OX]},
	LabelFontSize -> 15
];

NamedDiagram["Monad/ComultiplicationSquareImage"] :=
  comonadDiagram["Monad/MultiplicationSquareImage"];

(**************************************************************************************************)

NamedDiagram["Monad/UnitTrianglesImage"] := DoubleTriangleDiagram[
	{$FunT[$OX], PowerForm[$FunT,2][$OX], $FunT[$OX], $FunT[$OX]},
	{$NTeta[$FunT[$OX]], $FunT[$NTeta[$OX]], {None, "Equality"}, $NTmu[$OX], {None, "Equality"}},
	LabelFontSize -> 15,
	Origin -> {1.75, 0}
]

NamedDiagram["Monad/CounitTrianglesImage"] :=
  comonadDiagram["Monad/UnitTrianglesImage"];

(**************************************************************************************************)

$monAdjColorRules = {
  $Oc -> $DarkBlue, $Od -> $DarkRed, $ff -> $DarkRed, $fg -> $DarkBlue,
  $CC -> 1, $CD -> 2, $CE -> 3,
  $FunT1 -> $DarkRed, $FunT2 -> $DarkBlue, $FunT3 -> 3,
  $NTmu -> $DarkBlue, $NTeta -> $DarkBlue,
  $NTdelta -> $DarkRed, $NTeps -> $DarkRed,
  $FunL -> {1, 2}, $FunR -> {2, 1},
  $FunL1 -> {1, 2}, $FunR1 -> {2, 1}, $NTeps1 -> $Pink, $NTeta1 -> $Pink,
  $FunL2 -> {2, 3}, $FunR2 -> {3, 2}, $NTeps2 -> $Teal, $NTeta2 -> $Teal
};

$monAdjOpts = Sequence[
  RegionFilling -> "Unlabeled", ColorRules -> $monAdjColorRules,
  WireThickness -> 2, NodeEdgeThickness -> 2, NodeLabelFontSize -> 18, NodeSize -> 22, NodeShape -> "Box",
  TextModifiers -> {Subscript[z_, _] :> z}
];

monadMuDiagram[c_, t_, mu_, opts___] :=
	StringDiagram[
		{{0,0} -> mu},
		{1 <=> {Bottom,6} -> t, 1 <=> {Bottom,-6} -> t, 1 <=> Top -> t},
		Background -> c, opts, $monAdjOpts];

monadEtaDiagram[c_, t_, eta_, opts___] :=
	StringDiagram[
		{{0,0} -> eta},
		{1 <=> Top -> t},
		Background -> c, opts, $monAdjOpts];

monadDeltaDiagram[c_, t_, delta_, opts___] :=
	monadMuDiagram[c, t, delta, FlipY -> True, opts];

monadEpsDiagram[c_, t_, eps_, opts___] :=
	monadEtaDiagram[c, t, eps, FlipY -> True, opts];

(**************************************************************************************************)

NamedDiagram["Monad/Mu"] := monadMuDiagram[$CD, $FunT2, $NTmu]
NamedDiagram["Monad/Eta"] := monadEtaDiagram[$CD, $FunT2, $NTeta]
NamedDiagram["Monad/Delta"] := monadDeltaDiagram[$CC, $FunT1, $NTdelta]
NamedDiagram["Monad/Eps"] := monadEpsDiagram[$CC, $FunT1, $NTeps]

(**************************************************************************************************)

adjMonadMuDiagram[c_, d_, l_, r_, eps_, opts___] := StringDiagram[
  {{0,-3} -> eps},
  {1 <=> {Bottom, -4} -> l, 1 <=> {Bottom, 4} -> r,
  Customized[{Bottom, 8} <=> {Top, 2} -> l, SegmentPosition -> 0.55],
   Customized[{Bottom, -8} <=> {Top, -2} -> r, SegmentPosition -> 0.55]},
  {Bottom -> d, Center -> c, Right -> d, Left -> d},
  opts, $monAdjOpts
];

adjMonadEtaDiagram[c_, d_, l_, r_, eta_, opts___] := StringDiagram[
  {{0,0} -> eta},
  {1 <=> {Top, -4} -> r, 1 <=> {Top, 4} -> l},
  {Bottom -> d, Top -> c},
  opts, $monAdjOpts
];

$funRL = CompositionForm[$FunR, $FunL];

NamedDiagram["Monad/AdjunctionMu"] := adjMonadMuDiagram[$CC, $CD, $FunL, $FunR, $NTeps1];
NamedDiagram["Monad/AdjunctionEta"] := adjMonadEtaDiagram[$CC, $CD, $FunL, $FunR, $NTeta1];

NamedDiagram["Monad/AdjunctionMuResult"] := monadMuDiagram[$CD, $funRL, $NTmu];
NamedDiagram["Monad/AdjunctionEtaResult"] := monadEtaDiagram[$CD, $funRL, $NTeta];

(**************************************************************************************************)

adjMonadDeltaDiagram[c_, d_, l_, r_, eta_, opts___] := adjMonadMuDiagram[d, c, r, l, eta, FlipY -> True, opts];
adjMonadEpsDiagram[c_, d_, l_, r_, eps_, opts___] := adjMonadEtaDiagram[d, c, r, l, eps, FlipY -> True, opts];

$funLR = CompositionForm[$FunL, $FunR];

NamedDiagram["Monad/AdjunctionDelta"] := adjMonadDeltaDiagram[$CC, $CD, $FunL, $FunR, $NTeta1];
NamedDiagram["Monad/AdjunctionEps"] := adjMonadEpsDiagram[$CC, $CD, $FunL, $FunR, $NTeps1];

NamedDiagram["Monad/AdjunctionDeltaResult"] := monadDeltaDiagram[$CC, $funLR, $NTdelta];
NamedDiagram["Monad/AdjunctionEpsResult"] := monadEpsDiagram[$CC, $funLR, $NTeps];


