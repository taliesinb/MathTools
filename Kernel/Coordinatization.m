Package["GraphTools`"]


PackageImport["GeneralUtilities`"]


PackageScope["findCoordinatizationFunction"]

findCoordinatizationFunction[matrices_, group_] := Scope[
  If[TranslationGroupQ[group](*  || DihedralTranslationGroupQ[group] *),
    translationVectors = ExtractTranslationVector /@ matrices;
    rank = MatrixRank[translationVectors];
    taker = If[rank === Length[translationVectors], Identity, TakeOperator[rank]];
    types = Table["Infinite", rank];
    Return @ {types, First /* ExtractTranslationVector /* taker, False};
  ];
  positions = FindDiagonalBlockPositions[matrices];
  {types, functions} = Transpose @ Map[
    pos |-> findSubCoordinatization[DiagonalBlock[matrices, pos], pos],
    positions
  ];
  isRedundant = False;
  If[Length[matrices] >= 3,
    matrices = matrices;
    If[DihedralTranslationGroupQ[group],
      matrices[[All, -1, -1]] = 1];
    g12 = Dot @@ Take[matrices, 2]; g12i = Inverse[g12];
    isRedundant = AnyTrue[Drop[matrices, 2], SameQ[#, g12] || SameQ[#, g12i]&];
  ];

  {types, First /* (ApplyThrough @ functions), isRedundant}
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
    AllTrue[matrices, DihedralTranslationMatrixQ],
      type = "Infinite";
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


(* i think these things have to be in a special relationship, so that moving in direction a finite
number of times brings us back to 'a', and same for the other directions. but since we can't move FOREVER
in any direction since it is a finite group that gives us a constraint *)
chooseLatticeCoordinatization[{c1:{"Cyclic", m_}, c2:{"Cyclic", n_}, c3:{"Cyclic", p_}}, True] :=
  composeWith[$abc] @ chooseLatticeCoordinatization[{GCD[c1, c2], GCD[c2, c3]}, False];

chooseLatticeCoordinatization[{{"Cyclic", m_}, {"Cyclic", n_}}, redundant_] /; m <= n :=
  {True, torusPoint[1, 3, N[2 * Pi / m], N[2 * Pi / n]]};

chooseLatticeCoordinatization[spec:{{"Cyclic", _}, {"Cyclic", _}}, redundant_] :=
  flipSpec[spec, redundant];

tubePoint[r_, omega_][{a_, b_}] := {Sin[a * omega], Cos[a * omega], b};

chooseLatticeCoordinatization[{{"Cyclic", n_}, "Infinite"}, redundant_] :=
  {True, tubePoint[m, N[2 * Pi / n]]};

chooseLatticeCoordinatization[spec:{"Infinite", {"Cyclic", _}}, redundant_] :=
  flipSpec[spec, redundant];

chooseLatticeCoordinatization[{Repeated["Infinite", 2]}, _] :=
  {False, Identity};

chooseLatticeCoordinatization[{Repeated["Infinite", 3]}, False] :=
  {True, Identity};

$abc = Transpose @ N @ {{Sqrt[3]/2, -(1/2)}, {0, 1}, {-(Sqrt[3]/2), -(1/2)}};
dotABC[e_] := Dot[$abc, e];

PackageExport["DotABC"]

DotABC[e_] := Dot[$abc, e];

chooseLatticeCoordinatization[{Repeated["Infinite", 3]}, True] :=
  {False, dotABC};

composeWith[f_][res_] := MapAt[If[# === None, None, f /* #]&, res, 2];

flipSpec[spec_, redundant_] :=
  composeWith[Reverse] @ chooseLatticeCoordinatization[Reverse @ spec, redundant];

(* Todo: 3D redundant *)

chooseLatticeCoordinatization[types_, _] := {Length[types] >= 3, None};

