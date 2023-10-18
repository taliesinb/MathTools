PublicGraphicsPrimitive[EmptyRectangle]

DeclareGraphicsPrimitive[EmptyRectangle, "Vector,Vector", emptyRectangleBoxes]

(* we use CurveClosed to ensure that we dont get endpoint clashes *)

emptyRectangleBoxes[EmptyRectangle[a:$Coord2P, b:$Coord2P]] :=
  EmptyRectangleBox[a, b];

PrivateTypesettingBoxFunction[EmptyRectangleBox]

EmptyRectangleBox[{x1_, y1_}, {x2_, y2_}] :=
  JoinedCurveBox[
    List @ Line @ {{x1, y1}, {x2, y1}, {x2, y2}, {x1, y2}},
    CurveClosed -> True
  ];

(**************************************************************************************************)

PublicGraphicsPrimitive[EmptyPolygon]

DeclareGraphicsPrimitive[EmptyPolygon, "Matrix | Matrices", emptyPolygonBoxes, {2, 3}]

emptyPolygonBoxes[EmptyPolygon[p:($CoordMatP | $CoordMatsP)]] :=
  JoinedCurveBox[List @ Line @ p, CurveClosed -> True];
