PublicHead[VectorArrow]

declareGraphicsFormatting[VectorArrow[pos:$CoordP, dir:$CoordP, rest___Rule] :> bendyArrowBoxes[True, {pos, pos + dir}, rest], Graphics3D];
declareGraphicsFormatting[VectorArrow[pos:$CoordP, dir:$CoordP, rest___Rule] :> bendyArrowBoxes[False, {pos, pos + dir}, rest], Graphics];

Options[VectorArrow] = Options[BendyArrow]