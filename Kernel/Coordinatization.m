PackageExport["FindCoordinatizationFunction"]

Options[FindCoordinatizationFunction] = {"Group" -> None, Modulus -> None};

FindCoordinatizationFunction[qr_QuiverRepresentationObject] :=
  FindCoordinatizationFunction[qr["Representation"]];

FindCoordinatizationFunction[rep_RepresentationObject, opts:OptionsPattern[]] := Scope[
  generators = rep["Generators"];
  gen = First @ generators;
  modulus = If[MatchQ[gen, RepresentationElement[_, _]], Last[gen], None];
  FindCoordinatizationFunction[First /@ generators, "Group" -> rep["Group"], Modulus -> modulus]
];

FindCoordinatizationFunction[matrices_List, OptionsPattern[]] := Scope[
  UnpackOptions[group, modulus];
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

getTranslationCyclicity[n_] := Match[modulus,
  None :> "Infinite",
  z_Integer :> {"Cyclic", z},
  matrix_List :> Scope[
    mod = matrix[[n, -1]];
    If[1 < mod < Infinity, {"Cyclic", mod}, "Infinite"]
  ]
]

findSubCoordinatization[matrices_, {m_, n_}] := Scope[
  size = n - m + 1; num = Length[matrices];
  Which[
    (size == 1) && Count[matrices, {{-1}}] == 1,
      {{"Cyclic", 2}, Extract[{m, m}] /* Function[# + 1]},
    (size == 1) && ContainsUnitRootsQ[matrices],
      {{"Cyclic", FirstCase[matrices, UnitRoot[k_] :> k, None, Infinity]},
        Extract[{m, m}] /* GetRootPower
      },
    Count[matrices, _ ? BasisInversionMatrixQ] == 1 && Count[matrices, _ ? TranslationMatrixQ] == (num - 1),
      i = 1;
      Splice @ Table[
        {"Infinite", Extract[{row, n}] /* If[BasisInversionMatrixQ @ Part[matrices, i++], # / 2&, Identity]},
        {row, m, n}
      ],
    AllTrue[matrices, DihedralTranslationMatrixQ[#] || TranslationMatrixQ[#]&],
      Splice @ Table[{getTranslationCyclicity[row], Extract[{row, n}]}, {row, m, n-1}],
    True,
      {None, DiagonalBlock[{m, n}] /* Flatten}
  ]
];

BasisInversionMatrixQ[matrix_] :=
  IdentityMatrixQ[Abs @ matrix] && Count[Diagonal @ matrix, -1] == 1;

PackageScope["chooseLatticeCoordinatization"]

(* this should return {is3D, func}, where func takes a coordinate vector
and produces a 2D or 3D coordinate *)

chooseLatticeCoordinatization[{"Infinite", {"Cyclic", n_}}, _] :=
  {True, TimesOperator[{1, Tau / n}] /* TubeVector[n / Tau]};

chooseLatticeCoordinatization[{{"Cyclic", m_}, {"Cyclic", n_}}, _] /; m <= n :=
  {True, TimesOperator[Tau / {m, n}] /* TorusVector[{m / Tau, (n + m) / Tau}]};

chooseLatticeCoordinatization[spec:{{"Cyclic", _}, {"Cyclic", _}}, redundant_] := flipSpec[spec, redundant];
chooseLatticeCoordinatization[spec:{{"Cyclic", _}, "Infinite"}, redundant_] := flipSpec[spec, redundant];

chooseLatticeCoordinatization[{"Infinite"}, _] :=
  {False, Append[0]};

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

