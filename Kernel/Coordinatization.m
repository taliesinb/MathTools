Package["GraphTools`"]


PackageImport["GeneralUtilities`"]


PackageScope["findCoordinatizationFunction"]

findCoordinatizationFunction[matrices_] := Scope[
  positions = FindDiagonalBlockPositions[matrices];
  {types, functions} = Transpose @ Map[
    pos |-> findSubCoordinatization[DiagonalBlock[matrices, pos], pos],
    positions
  ];
  {types, First /* (ApplyThrough @ functions)}
];

findSubCoordinatization[matrices_, {m_, n_}] := Scope[
  size = n - m + 1;
  Which[
    (size == 1) && Count[matrices, {{-1}}] == 1,
      {{"Cyclic", 2}, Extract[{m, m}] /* Function[# + 1]},
    (size == 1) && ContainsUnitRootsQ[matrices],
      {{"Cyclic", FirstCase[matrices, UnitRoot[k_] :> k, None, Infinity]},
        Extract[{m, m}] /* GetRootPower
      },
    AllTrue[matrices, AbelianMatrixQ],
      type = If[ContainsQ[matrices, -1], "RedundantInfinite", "Infinite"];
      Splice @ Table[{type, Extract[{row, n}]}, {row, m, n-1}],
    True,
      {None, DiagonalBlock[{m, n}] /* RepresentationElement}
  ]
];

PackageScope["chooseLatticeCoordinatization"]

(* this should return {is3D, func}, where func takes a coordinate vector
and produces a 2D or 3D coordinate *)

torusPoint[r1_, r2_, omega1_, omega2_][{a_, b_}] := With[
  {p = r2 + r1 * Cos[a * omega1]},
  {
    p * Cos[b * omega2],
    p * Sin[b * omega2],
    r1 * Sin[a * omega1]
  }
];

chooseLatticeCoordinatization[{{"Cyclic", m_}, {"Cyclic", n_}}] /; m <= n :=
  {True, torusPoint[1, 3, N[2 * Pi / m], N[2 * Pi / n]]};

chooseLatticeCoordinatization[spec:{{"Cyclic", _}, {"Cyclic", _}}] := flipSpec[spec];

tubePoint[r_, omega_][{a_, b_}] := {Sin[a * omega], Cos[a * omega], b};

chooseLatticeCoordinatization[{{"Cyclic", n_}, "Infinite"}] :=
  {True, tubePoint[m, N[2 * Pi / n]]};

chooseLatticeCoordinatization[spec:{"Infinite", {"Cyclic", _}}] := flipSpec[spec];

chooseLatticeCoordinatization[{Repeated["Infinite", 2]}] :=
  {False, Identity};

chooseLatticeCoordinatization[{Repeated["Infinite", 3]}] :=
  {True, Identity};

$abc = Transpose @ N @ {{Sqrt[3]/2, -(1/2)}, {0, 1}, {-(Sqrt[3]/2), -(1/2)}};
chooseLatticeCoordinatization[{Repeated["RedundantInfinite", 3]}] :=
  {False, Dot[$abc, #]&};

flipSpec[spec_] := Scope[
  {is3D, res} = chooseLatticeCoordinatization[Reverse @ spec];
  {is3D, Reverse /* res}
];

(* Todo: 3D redundant *)

chooseLatticeCoordinatization[types_] := {Length[types] >= 3, None};

