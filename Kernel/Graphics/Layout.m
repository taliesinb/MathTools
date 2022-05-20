PackageExport["EchoGraphicsScope"]
PackageExport["EchoGraphics"]

SetHoldAllComplete[DebugGraphicsScope];
DebugGraphicsScope[e_] := Scope[
	$prims = {};
	res = e;
	n = Length[$prims]; $i = 1;
	Print @ Graphics[
		Map[{Opacity[$i++ / n], #}&, $prims],
		Frame -> True, PlotRangePadding -> Scaled[0.1]
	];
	res
];


PackageExport["EchoGraphicsScope"]

EchoGraphicsScope[e_] := (AppendTo[$prims, e]; e);
EchoGraphicsScope[{x_ ? RealVectorQ, y_ ? RealVectorQ}] := (EchoGraphicsScope @ Transpose @ {x, y}; {x, y});
EchoGraphicsScope[points_ ? RealMatrixQ] := (AppendTo[$prims, Point @ points]; points);


PackageExport["ElectricalBalanceX"]

(* applies electrical repulsion, but only along the x axis, and only for y values that are very similar *)

ElectricalBalanceX[x_, y_, n_, delta_] := Scope[
	x = ToPackedReal[x];
	yDist = 10.0 * SquaredDistanceMatrix[y] + 0.1;
	Do[
		x -= delta * Total[Outer[Subtract, x, x] / (SquaredDistanceMatrix[x] + yDist), {1}];
	,
		{n}
	];
	ToPackedReal @ Flatten @ x
]


PackageExport["ElectricalGravitationalBalanceX"]

(*
applies electrical repulsion to points that are near each-other in y values.
It also makes points attracted to the mean x value of all of their children
and parents. the graph itself is passed in (should be an index graph).
*)

ElectricalGravitationalBalanceX[x_, y_, graph_, n_, delta_] := Scope[
	x = ToPackedReal[x];
	yDist = 10.0 * SquaredDistanceMatrix[y] + 0.1;
	adj = AdjacencyMatrix @ graph;
	symAdjMatrix = BitOr[adj, Transpose @ adj, IdentityMatrix @ Length @ adj];
	sqrtNumAdj = Sqrt @ Total[symAdjMatrix, {2}];
	meanAdjMatrix = ToPackedReal @ Map[LengthNormalize, symAdjMatrix];
	(* ^ ensure that for sinks, we don't try take the average of no verts *)
	Do[
		dx = Outer[Subtract, x, x]; x2 = SquaredDistanceMatrix[x];
		repulse = Total[dx / (x2 + yDist), {1}];
		attract = (x - Dot[meanAdjMatrix, x]) * sqrtNumAdj;
		x -= delta * (repulse + attract);
	,
		{n}
	];
	x
]

