PublicGraphicsPrimitive[Triad]

PublicOption[TriadColor, TriadOpacity, TriadBendOptions, InteriorOffset, TriadEdgeThickness, TriadStrip, TriadArrowhead, TriadStripRadius, TriadLegs, TriadLegColor, TriadLegThickness, TriadLegWidth]

Options[Triad] = {
  TriadBendOptions -> {BendRadius -> 0.1, BendShape -> None},
  InteriorOffset -> 0.1,
  TriadColor -> GrayLevel[0, 0.5],
  TriadOpacity -> 1,
  TriadEdgeThickness -> 1,
  TriadStrip -> None,
  TriadStripRadius -> 0.1,
  TriadArrowhead -> False,
  TriadLegs -> True,
  TriadLegColor -> $Gray,
  TriadLegThickness -> 3,
  TriadLegWidth -> None
};

AssociateTo[$MakeBoxesStyleData, Options[Triad]];

$triadOffset = 0.05;

DeclareGraphicsPrimitive[Triad, "Matrix", triadBoxes, {2, 3}]

triadBoxes[Triad[p:{$Coord3P, $Coord3P, $Coord3P}, opts___Rule]] := rawTriadBoxes[True, p, opts];
triadBoxes[Triad[p:{$Coord2P, $Coord2P, $Coord2P}, opts___Rule]] := rawTriadBoxes[False, p, opts];

(**************************************************************************************************)
rawTriadBoxes[is3d_, {a_, b_, c_}, opts___Rule] := Scope[
  $3d = is3d;
  UnpackAssociationSymbols[
    {opts} -> $MakeBoxesStyleData,
    triadBendOptions, triadColor, triadOpacity, interiorOffset,
    triadEdgeThickness, triadStrip, triadArrowhead, triadStripRadius, triadLegs, triadLegColor,
    triadLegThickness, triadLegWidth
  ];
  triadEdgeColor = Darker[triadColor, .3];
  triadColor = SetColorOpacity[triadColor, triadOpacity];
  strip = legs = Nothing;
  If[NumberQ[interiorOffset] || CoordinateVectorQ[interiorOffset],
    {a, b, c} = ShrinkPolygon[orig = {a, b, c}, {1,1,1} * interiorOffset];
    If[triadLegs,
      {oa, ob, oc} = orig;
      legs = MapThread[
        makeLine[{ClosestPointOnLine[#1, #2], #2}, #3, triadLegThickness, triadLegWidth]&,
        (* use nearest point on line to find the matching point *)
        {{List[oa, ob], List[ob, oc], List[oc, oa]},
         {Avg[a, b], Avg[b, c], Avg[c, a]},
         triadLegColor * {1, 1, 1}}
      ];
    ];
  ];
  If[triadStrip =!= None,
    stripColor = Darker[triadColor, .1];
    {sa, sb, sc} = ShrinkPolygon[{a, b, c}, UnitVector[triadStrip, 3] * triadStripRadius];
    Switch[triadStrip,
      1, stripCoords = {a, sa, sb, b};
      2, stripCoords = {b, sb, sc, c};
      3, stripCoords = {a, sa, sc, c};
    ];
    strip = Construct[If[$3d, Polygon3DBox, PolygonBox], stripCoords, VertexColors -> {Transparent, Transparent, Black, Black}];
    strip = StyleBox[strip, EdgeForm @ None];
  ];

  points = rollingCurvePoints @ RollingCurve[
    {a, b, c},
    BendRadius -> Lookup[triadBendOptions, BendRadius, .1],
    BendShape -> Lookup[triadBendOptions, BendShape, "Arc"]
  ];
  polygon = makePolygon[points, triadColor, triadEdgeColor, triadEdgeThickness];
  edge = makeLine[{a, b, c, a}, triadEdgeColor, triadEdgeThickness, None];

  If[TrueQ @ triadArrowhead,
    arrowhead = arrowheadBoxes[
      Avg[a, c], c - a,
      ArrowheadShape -> "RightPointedTriangle",
      ArrowheadLength -> 0.2,
      ArrowheadPlane -> PlaneRightTowards[b],
      ArrowheadColor -> triadEdgeColor
    ];
  ,
    arrowhead = Nothing
  ];
  {polygon, strip, arrowhead, edge; legs}
];

offsetToMean[points_, d_] := Scope[
  mean = Mean[points];
  PointAlongLine[{#, mean}, d]& /@ points
];

makeLine[_, _, 0|0., None] := {};

makeLine[points_, color_, thickness_, width_ ? NumericQ] :=
  ToBoxes[Style[ExtrudedLine[points, width], color]];

makeLine[points_, color_, thickness_, _] :=
  StyleBox[
    Construct[If[$3d, Line3DBox, LineBox], points],
    AbsoluteThickness @ thickness, color
  ];

makePolygon[points_, color_, edgeColor_, thickness_] :=
  StyleBox[
    Construct[If[$3d, Polygon3DBox, PolygonBox], points],
    FaceForm @ If[noneColorQ @ color, None, color],
    EdgeForm @ If[thickness == 0 || noneColorQ[edgeColor], None, {edgeColor, AbsoluteThickness @ thickness}]
  ];

noneColorQ = Case[
  None        := True;
  Transparent := True;
  _           := False;
]