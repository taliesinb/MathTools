(**************************************************************************************************)

PublicHead[UniqueLabel]

SetUsage @ "
UniqueLabel[n$] represents a numeric label in a plot that should be numbered in raster order.
"

(**************************************************************************************************)

PublicFunction[ModulusEdgeShapeFunction]

ModulusEdgeShapeFunction[offsets_][assoc_] := Scope[
  UnpackAssociation[assoc, coordinates, arrowheads, shape, edgeIndex, labelStyle, setback];
  {a, b} = FirstLast @ coordinates; d = If[$GraphIs3D, .6, .4] + Max[setback]/2;
  b2 = findModulusCounterpart[a, b, offsets, d];
  a2 = findModulusCounterpart[b, a, offsets, d];
  setback1 = First[setback, setback]; setback2 = Last[setback, setback];
  If[a2 =!= None && b2 =!= None,
    counter = assoc["Counter"];
    labelPoints = {1.35*(a2 - b) + b, (b2-a)*1.35 + a};
    label = If[labelStyle === None, Nothing, Map[makeWrappedEdgeLabel[counter, labelStyle], labelPoints]];
    arrowheadsA = changeArrowheadPos[arrowheads, 0.8];
    arrowheadsB = changeArrowheadPos[arrowheads, 0.2];
    {
      Style[shape @ setbackCoords[{setback1, 0}] @ {a, b2}, arrowheadsA],
      Style[shape @ setbackCoords[{0, setback2}] @ {a2, b}, arrowheadsB],
      label
    }
  ,
    Style[shape @ setbackCoords[setback] @ {a, b}, arrowheads]
  ]
];

ModulusEdgeShapeFunction[basis_Association][assoc_] := Scope[
  UnpackAssociation[assoc, cardinal, coordinates, arrowheads, shape, edgeIndex, labelStyle];
  {a, b} = FirstLast @ coordinates;
  If[EuclideanDistance[a, b] > 1.1,
    a2 = b - basis[cardinal]/3;
    b2 = a + basis[cardinal]/3;
    counter = assoc["Counter"];
    labelPoints = {1.35*(a2 - b) + b, (b2-a)*1.35 + a};
    label = If[labelStyle === None, Nothing, Map[makeWrappedEdgeLabel[counter, labelStyle], labelPoints]];
    arrowheadsA = changeArrowheadPos[arrowheads, 0.8];
    arrowheadsB = changeArrowheadPos[arrowheads, 0.2];
    {
      Style[shape @ {a, b2}, arrowheadsA],
      Style[shape @ {a2, b}, arrowheadsB],
      label
    }
  ,
    Style[shape @ {a, b}, arrowheads]
  ]
];

makeWrappedEdgeLabel[counter_, labelStyle_][pos_] :=
  Text[Style[UniqueLabel @ counter, Opacity[1], labelStyle], pos,
    {0, 0},
    Background -> White, BaseStyle -> {FontSize -> 8}];

changeArrowheadPos[Arrowheads[{{sz_, _, g_}}], pos_] :=
  Arrowheads[{{sz, pos, g}}];

changeArrowheadPos[g_, _] := g;

findModulusCounterpart[a_, b_, offsets_, d_] := Scope[
  bs = PlusOperator[b] /@ offsets;
  b2 = MinimumBy[bs, EuclideanDistance[a, #]&];
  If[EuclideanDistance[a, b2] >= EuclideanDistance[a, b], None,
    PointAlongLine[{a, b2}, d]
  ]
]
