Package["GraphTools`"]


PackageImport["GeneralUtilities`"]


PackageExport["RootSystemObjectQ"]

SetUsage @ "
RootSystemObjectQ[obj$] represents True if obj$ is a valid RootSystemObject[$$].
"

RootSystemObjectQ[_RootSystemObject ? System`Private`NoEntryQ] := True;
RootSystemObjectQ[_] := False;


PackageExport["RootSystemObject"]

SetUsage @ "
RootSystemObject[$$] represents a root system.
* The roots, in the form of RootVector[$$] objects, can be obtained by system$['RootVectors'].
* Particular named root systems can be obtained with RootSystemObject['name$'].
"

RootSystemObject /: Normal[rs_RootSystemObject] := rs["Roots"];

reachableStates[func_, initialStates_] :=
  StateTransitionGraph[func, initialStates, "VertexList", MaxVertices -> 20];

setValidIfNormlized[e_] := If[Norm[e] == 1, System`Private`SetValid[e]];

toCheckedAndPacked[e_] := Scope[
  e2 = Developer`ToPackedArray[List @@ e];
  If[Norm[e2] == 1, System`Private`SetValid[e2]];
  e2
];

makeRootSystemObject[generators_] := Scope[
  generators = Map[toCheckedAndPacked, generators];
  expandedGenerators = toCheckedAndPacked /@ Join[generators, -generators];
  dim = Length @ First @ generators;
  allRoots = reachableStates[
    ApplyThrough[VectorReflect /@ expandedGenerators],
    generators
  ];
  hyperPlane = UnitVector[dim, 1]; hyperPlane[[2]] = 10^-6;
  positiveRoots = Select[allRoots, Dot[hyperPlane, #] > 0&];
  If[dim === 2, positiveRoots = SortBy[positiveRoots, N[{Norm[#], ArcTan @@ #}]&]];
  simpleRoots = FindIndependentVectors[positiveRoots];
  allRoots = allRoots;
  generatorRoots = generators;
  assoc = <|
    "Generators" -> generatorRoots,
    "PositiveRoots" -> positiveRoots,
    "SimpleRoots" -> simpleRoots,
    "Roots" -> allRoots,
    "Dimension" -> dim,
    "Count" -> Length[allRoots]
  |>;
  System`Private`ConstructNoEntry[RootSystemObject, assoc]
];

declareFormatting[
  rs_RootSystemObject ? System`Private`NoEntryQ :>
    plotRootSystemObject @ getObjectData @ rs,
  RootVector[vec_List] :> renderRootVector @ vec
];

RootVector /: Normal[RootVector[vec_]] := vec;

declareObjectPropertyDispatch[RootSystemObject, rootSystemProperty];

rootSystemProperty[data_, "CoordinateFunction"] :=
  rootsCoordinateFunction @ data["SimpleRoots"];


PackageScope["rootsCoordinateFunction"]

rootsCoordinateFunction[simpleRoots_] := With[
  {inverseMatrix = Transpose @ PseudoInverse @ simpleRoots},
  Function[\[FormalV], Dot[inverseMatrix, \[FormalV]]]
];

rootSystemProperty[data_, "CayleyFunction", opts___] := computeCayleyFunction[data, opts];

rootSystemProperty[data_, "RootVectors"] := Map[RootVector, data["Roots"]]
rootSystemProperty[data_, "PositiveRootVectors"] := Map[RootVector, data["PositiveRoots"]]
rootSystemProperty[data_, "SimpleRootVectors"] := Map[RootVector, data["SimpleRoots"]]

rootSystemProperty[data_, "TranslationGroup"] := TranslationGroup @ $SelfObject;
rootSystemProperty[data_, "ReflectionGroup"] := ReflectionGroup @ $SelfObject;

rootSystemProperty[data_, "TranslationMatrices"] := Map[TranslationMatrix, data["PositiveRoots"]];
rootSystemProperty[data_, "ReflectionMatrices"] := Map[computeReflectionMatrix, data["PositiveRoots"]];

computeReflectionMatrix[vec_] := Scope[
  n = Length[vec];
  Table[VectorReflect[vec, UnitVector[n, i]], {i, n}]
];

renderRootVector[vec_List] := Grid[
  {vec},
  ItemSize -> All,
  BaseStyle -> $matrixElementStyle,
  Dividers -> All,
  Sequence @@ matrixGridStyle[0.5]
]

plotRootSystemObject[data_] := Scope[
  UnpackAssociation[data, dimension, simpleRoots, positiveRoots, roots];
  maxNorm = Max[Norm /@ roots];
  tuples = Map[root |-> Which[
    MemberQ[simpleRoots, root], {root, $Red, IndexOf[positiveRoots, root]},
    MemberQ[positiveRoots, root], {root, $Orange, IndexOf[positiveRoots, root]},
    True, {root, Black, None}
  ], roots];
  origin = If[dimension === 2, rootPlot2D, rootPlot3D][tuples, maxNorm]
];

$rootPlotStyle = Sequence[PlotRangePadding -> Scaled[0.1], FrameStyle -> Gray, FrameTicks -> None];

PackageExport["RootPlot"]

RootPlot[roots_, opts___] := Scope[
  i = 1; dim = Length @ First @ roots;
  hyperPlane = UnitVector[dim, 1]; hyperPlane[[2]] = 10^-6;
  tuples = Map[root |-> {root, If[Dot[hyperPlane, root] > 0, $Red, Black], i++}, roots];
  If[dim == 2, rootPlot2D, rootPlot3D][tuples, Max[Norm /@ roots], opts]
];

rootGraphics[tuples_, origin_] := {
  {LightGray, Line[{origin, #}& /@ tuples[[All, 1]]]},
  {AbsolutePointSize[4],
    Apply[
      {#2, Point @ #1, Black, If[#3 =!= None, Text[#3, #1, {0, 1.5}], {}]}&,
      tuples, {1}
    ]},
  {Gray, Point[origin]}
};

rootPlot2D[tuples_, norm_, opts___] := Graphics[
  rootGraphics[tuples, {0, 0}],
  PlotRange -> (norm * {{-1, 1}, {-1, 1}}),
  $rootPlotStyle, ImageSize -> 100, Frame -> True,
  opts
]

rootPlot3D[tuples_, norm_, opts___] := Graphics3D[
  rootGraphics[tuples, {0, 0, 0}],
  PlotRange -> (norm * {{-1, 1}, {-1, 1}, {-1, 1}}),
  $rootPlotStyle, ImageSize -> 150, Boxed -> True, SphericalRegion -> True,
  opts
];


Options[computeCayleyFunction] = {"Symmetric" -> True, "Labeled" -> True};
computeCayleyFunction[data_, OptionsPattern[]] := Scope[
  UnpackOptions[labeled, symmetric];

];




PackageExport["VectorReflect"]

SetUsage @ "
VectorReflect[v$, rv$] reflects the vector v$ in the hyperplane perpendicular to rv$.
"

VectorReflect[v_, rv_] := Expand[v - (2 * Dot[rv, v] / Dot[rv, rv]) * rv];
VectorReflect[v_, rv_ ? System`Private`ValidQ] := Expand[v - (2 * Dot[rv, v]) * rv];
VectorReflect[rv_][v_] := VectorReflect[v, rv];


RotateBy90[vector_] := Dot[{{0, -1}, {1, 0}}, vector];
RotateBy45[vector_] := Dot[{{1/Sqrt[2], -(1/Sqrt[2])}, {1/Sqrt[2], 1/Sqrt[2]}}, vector];


PackageExport["RootSystem"]

SetListable[rangle];
rangle[angle_] := AngleVector[angle * 2 * Pi];

$sq2 = Sqrt[2];
$sq3 = Sqrt[3];

RootSystem["A1"] := Memoized @ makeRootSystemObject @ rangle[{0}];
RootSystem["D2"] := Memoized @ makeRootSystemObject @ rangle[{0, 1/4}];
RootSystem["A2"] := Memoized @ makeRootSystemObject @ rangle[{0, 4/3}];
RootSystem["B2"] := Memoized @ makeRootSystemObject @ {rangle[0], $sq2 * rangle[3/8]};
RootSystem["G2"] := Memoized @ makeRootSystemObject @ {rangle[0], $sq3 * rangle[5/12]};
RootSystem["C2"] := Memoized @ makeRootSystemObject @ {$sq2 * rangle[0], rangle[3/8]};

RootSystem["B3"] := Memoized @ makeRootSystemObject @ {$sq2 * {1, -1, 0}, $sq2 * {0, 1, -1}, {0, 0, 1}}


PackageExport["$NamedRootSystems"]

$NamedRootSystems = StringSplit["A1 D2 A2 B2 G2 C2 B3"];


declareFunctionAutocomplete[RootSystem, {$NamedRootSystems}];