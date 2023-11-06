PublicFunction[ElectricalBalanceX]

(* applies electrical repulsion, but only along the x axis, and only for y values that are very similar *)

ElectricalBalanceX[x_, y_, n_, delta_] := Scope[
	x = ToPackedReal[x];
	yDist = ToPackedReal[10.0 * SquaredDistanceMatrix[y] + 0.1];
	Do[
		x -= delta * Total[DifferenceMatrix[x] / (SquaredDistanceMatrix[x] + yDist), {1}];
	,
		{n}
	];
	ToPackedReal @ Flatten @ x
]

(**************************************************************************************************)

PublicFunction[ElectricalGravitationalBalanceX]

(*
applies electrical repulsion to points that are near each-other in y values.
It also makes points attracted to the mean x value of all of their children
and parents. the graph itself is passed in (should be an index graph).
*)


ElectricalGravitationalBalanceX[x_, y_, graph_, n_, delta_] := Scope[
	x = ToPackedReal[x];
	yDist = ToPackedReal[100000.0 * SquaredDistanceMatrix[y] + 0.1];
	adj = AdjacencyMatrix @ graph;
	symAdjMatrix = MatrixMax[adj, Transpose @ adj, 0.1 * (IdentityMatrix @ Len @ adj)];
	sqrtNumAdj = Sqrt @ RowTotals[symAdjMatrix];
	meanAdjMatrix = ToPackedReal @ Map[TotalNormalize, symAdjMatrix];
	(* TODO: Test SparseArray applied to meanAdjMatrix *)
	Do[
		dx = DifferenceMatrix[x]; x2 = SquaredDistanceMatrix[x];
		repulse = Total[dx / (x2 + yDist), {1}];
		attract = x - Dot[meanAdjMatrix, x];
		attract *= sqrtNumAdj;
		x -= delta * (repulse + attract);
	,
		{n}
	];
	dx = Max[(Dist @@ MinMax[x]) / 20, 0.0001];
	MeanShift[x, dx]
]



