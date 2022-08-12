PublicFunction[SetFrameColor]

SetFrameColor[g_Graphics, color_] :=
  If[LookupOption[g, Frame] === True,
    ReplaceOptions[g, FrameStyle -> color],
    ReplaceAll[g, a:Annotation[_, "Frame"] :> ReplaceAll[a, EdgeForm[_] :> EdgeForm[color]]]
  ];

SetFrameColor[expr_, _] := expr;

(**************************************************************************************************)

PublicFunction[FrameFadePolygon]

offsetNESW[corners_, 0] := corners;
offsetNESW[corners:{_, _, _, _}, o_] :=
  MapThread[Offset, {o * {{1, -1}, {-1, -1}, {1, 1}, {-1, 1}}, corners}];

FrameFadePolygon[{{l_, b_}, {r_, t_}}, o_] := Scope[
  {nw0, ne0, sw0, se0} = {{l, t}, {r, t}, {l, b}, {r, b}};
  {nw1, ne1, sw1, se1} = offsetNESW[{nw0, ne0, sw0, se0}, o];
  c1 = {1, 1, 1, 1}; c0 = {1, 1, 1, 0};
  Polygon[
    {nw0, ne0, se0, sw0, nw0, nw1, sw1, se1, ne1, nw1},
    VertexColors -> {c1, c1, c1, c1, c1, c0, c0, c0, c0, c0}
  ]
]
