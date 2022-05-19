PackageExport["SetbackCoordinates"]

SetbackCoordinates[spec_, 0|0.] :=
  spec;

SetbackCoordinates[spec_, d_ ? NumericQ] :=
  SetbackCoordinates[spec, {d, d}];

SetbackCoordinates[spec_ ? CoordinateArrayQ, d_] :=
  SetbackCoordinates[#, d]& /@ spec;

SetbackCoordinates[{a_, b_}, {d1_, d2_}] := Scope[
  If[EuclideanDistance[a, b] < d1 + d2, Return[{}]];
  dx = Normalize[b - a];
  {a + dx * d1 , b - dx * d2}
];

SetbackCoordinates[coords_, {d1_, d2_}] :=
  setbackHalf[setbackHalf[coords, d1], -d2]

setbackHalf[{}, _] := {};
setbackHalf[coords_, 0|0.] := coords;
setbackHalf[coords_, d_ ? Negative] := Reverse @ setbackHalf[Reverse @ coords, Abs[d]];
setbackHalf[coords_, d_] := takeLine[coords, d];

takeLine[coords_List, d_] := Scope[
  prev = First @ coords; total = 0;
  n = LengthWhile[coords, curr |-> (total += EuclideanDistance[curr, prev]; prev = curr; total < d)];
  If[n == Length[coords], Return @ Last @ coords];
  rem = total - d;
  newCoords = Drop[coords, n];
  If[rem == 0,
    newCoords,
    Prepend[newCoords, PointAlongLine[Part[coords, {n + 1, n}], rem]]
  ]
];
(**************************************************************************************************)

PackageExport["PointAlongLine"]

PointAlongLine[{a_, b_}, d_ ? NumericQ] :=
  a + Normalize[b - a] * d;

PointAlongLine[coords_, Scaled[d_]] :=
  PointAlongLine[coords, LineLength[coords] * d];

PointAlongLine[coords_List, d_ ? NumericQ] := Scope[
  prev = First @ coords; total = 0;
  n = LengthWhile[coords, curr |-> (total += EuclideanDistance[curr, prev]; prev = curr; total < d)];
  If[n == Length[coords], Return @ Last @ coords];
  rem = total - d;
  newCoords = Drop[coords, n];
  If[rem == 0,
    Part[coords, n + 1],
    PointAlongLine[Part[coords, {n + 1, n}], rem]
  ]
];

(**************************************************************************************************)

PackageExport["LineLength"]

LineLength[{a_, b_}] := EuclideanDistance[a, b];
LineLength[list_] := Total @ ApplyWindowed[EuclideanDistance, list];

(**************************************************************************************************)

PackageExport["EdgeLengthScale"]

boundingBoxSideLength[line_] :=
  Total[EuclideanDistance @@@ CoordinateBounds @ line];

(* boundingBoxSideLength[line_] :=
  EuclideanDistance @@ CoordinateBoundingBox @ line;
 *)
adjustedLineLength[line_] :=
  If[First[line] === Last[line], 0.8, 1] * Min[LineLength @ line, boundingBoxSideLength @ line];

EdgeLengthScale[{}, q_] := 1.0;

EdgeLengthScale[edgeCoordinateLists_, q_] := Scope[
  edgeLengths = Chop @ Map[adjustedLineLength, edgeCoordinateLists];
  edgeLengths = DeleteCases[edgeLengths, 0|0.];
  If[edgeLengths === {},
    edgeLengths = Map[boundingBoxSideLength, edgeCoordinateLists]];
  If[q === "Average", Mean @ edgeLengths, Quantile[edgeLengths, q]]
];