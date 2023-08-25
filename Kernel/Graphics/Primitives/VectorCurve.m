PublicHead[VectorCurve]

declareGraphicsFormatting[vc:(VectorCurve[$Coord3P, $Coord3P] | VectorCurve[$Coord3P]) :> Construct[Line3DBox, DiscretizeCurve @ vc], Graphics3D];
declareGraphicsFormatting[vc:(VectorCurve[$Coord2P, $Coord2P] | VectorCurve[$Coord2P]) :> Construct[LineBox, DiscretizeCurve @ vc], Graphics];

