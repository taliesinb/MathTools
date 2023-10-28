PublicGraphicsPrimitive[CurveExtrusion]

DeclareGraphicsPrimitive[CurveExtrusion, "Curve,Radius | Matrix,Radius", curveExtrusionBoxes];

SetUsage @ "
CurveExtrusion[curve$, r$] represents the polygon obtained by extruding curve$ by a radius r$.
"

curveExtrusionBoxes[CurveExtrusion[curve_, r:$NumberP]] :=
  ToGraphicsBoxes @ ExtrudeCurveToPolygon[curve, r];

(**************************************************************************************************)

PublicFunction[ExtrudeCurveToPolygon]

CacheSymbol[$CurveExtrusionCache]

(* not sure why, but if you don't do this the resulting polygon is closed from the start to end point *)
doubleUpCurve = Case[
	BezierCurve[c_] := JoinedCurve[{BezierCurve @ c, BezierCurve @ Reverse @ c}];
	Line[c_]        := JoinedCurve[{Line @ c, Line @ Reverse @ c}];
	points_List     := % @ Line @ points;
	curve_          := % @ DiscretizeCurve @ curve;
];

ExtrudeCurveToPolygon[Line[{a_, b_}] | {a_, b_}, r_] := Scope[
  d = VectorRotate90[Normalize[b - a]] * r;
  Polygon[{a + d, b + d, b - d, a - d}]
]

ExtrudeCurveToPolygon::failed = "Could not extrude curve ``."
ExtrudeCurveToPolygon[curve_, r_] := Scope[
	key = Hash @ {curve, r};
	result = $CurveExtrusionCache @ key;
	If[!MissingQ[result], Return @ result];
	doubled = doubleUpCurve @ curve;
  region = DiscretizeGraphics @ doubled;
  dilation = RegionDilation[region, r];
  Switch[dilation,
    _MeshRegion,
      result = RegionPolygon @ BoundaryMesh @ dilation,
    _Polygon,
      result = dilation,
    _,
      Message[ExtrudeCurveToPolygon::failed, MsgExpr @ curve];
  	  result = $Failed;
  ];
	AssociateTo[$CurveExtrusionCache, key -> result];
	result
];

