PublicHead[EmptyRectangle]

DeclareGraphicsPrimitive[EmptyRectangle, "Vector,Vector", emptyRectangleBoxes]

(* we use CurveClosed to ensure that we dont get endpoint clashes *)

emptyRectangleBoxes[EmptyRectangle[{x1:$NumberP, y1:$NumberP}, {x2:$NumberP, y2:$NumberP}]] :=
  JoinedCurveBox[
    List @ Line @ {{x1, y1}, {x2, y1}, {x2, y2}, {x1, y2}},
    CurveClosed -> True
  ];

(**************************************************************************************************)

PublicHead[EmptyPolygon]

DeclareGraphicsPrimitive[EmptyPolygon, "Matrix | Matrices", emptyPolygonBoxes, {2, 3}]

emptyPolygonBoxes[EmptyPolygon[p:($CoordMatP | $CoordMatsP)]] :=
  JoinedCurveBox[List @ Line @ p, CurveClosed -> True];
