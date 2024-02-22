triProdInputQ[a_, b_, c_] := And[ArrayQ[a], ArrayQ[b], ArrayQ[c]];

(**************************************************************************************************)

PublicFunction[Tricone]

Tricone[a_, b_, c_] /; triProdInputQ[a, b, c] := Scope[
  dims = Dims /@ {a, b, c};
  If[!MatchQ[dims, {{_, nab_, nac_}, {nab_, _, nbc_}, {nac_, nbc_, _}}], ReturnFailed[]];
  {{na, nab, nac}, {nab, nb, nbc}, {nac, nbc, nc}} = dims;
  Table[
    Sum[
      Part[a, ia, iab, iac] * Part[b, iab, ib, ibc] * Part[c, iac, ibc, ic],
      {iab, nab}, {iac, nac}, {ibc, nbc}
    ],
    {ia, na}, {ib, nb}, {ic, nc}
  ]
]

(**************************************************************************************************)

PublicFunction[Triblade]

Triblade[a_, b_, c_] /; triProdInputQ[a, b, c] := Scope[
  dims = Dims /@ {a, b, c};
  If[!MatchQ[dims, {{_, ab_, ac_}, {ab_, _, bc_}, {ab_, bc_, _}}], ReturnFailed[]];
  {{na, np, nq}, {np, nb, nq}, {nq, np, nc}} = dims;
  Table[
    Sum[
      Part[a, ia, p, q] * Part[b, p, ib, q] * Part[c, q, p, ic],
      {p, np}, {q, nq}
    ],
    {ia, na}, {ib, nb}, {ic, nc}
  ]
]

(**************************************************************************************************)

PublicFunction[Triforce]

Triforce[a_, b_, c_] /; triProdInputQ[a, b, c] := Scope[
  dims = Dims /@ {a, b, c};
  If[!MatchQ[dims, {{_, ab_, ac_}, {ab_, _, bc_}, {ab_, bc_, _}}], ReturnFailed[]];
  {{na, nab, nac}, {nab, nb, nbc}, {nac, nbc, nc}} = dims;
  Table[
    Sum[
      Part[a, ia, iab, iac] * Part[b, iab, ib, ibc] * Part[c, iac, ibc, ic],
      {iab, nab}, {iac, nac}, {ibc, nbc}
    ],
    {ia, na}, {ib, nb}, {ic, nc}
  ]
];

(**************************************************************************************************)

PublicFunction[Fish]

Fish[a_, b_, c_] /; triProdInputQ[a, b, c] := Scope[
  dims = Dims /@ {a, b, c};
  If[!MatchQ[dims, {{_, _, np_}, {nq_, nr_, np_}, {nq_, nr_, _}}], ReturnFailed[]];
  {{n1, n2, np}, {nq, nr, np}, {nq, nr, n3}} = dims;
  Table[
    Sum[
      Part[a, i, j, p] * Part[b, q, r, p] * Part[c, q, r, k],
      {p, np}, {q, nq}, {r, nr}
    ],
    {i, n1}, {j, n2}, {k, n3}
  ]
];

(**************************************************************************************************)

PublicFunction[ThreeArrayQ, CubixQ]

ThreeArrayQ[arr_] := ArrayQ[arr, 3];
CubixQ[arr_, n_] := ArrayQ[arr, 3] && AllSameQ[Dims[arr]];

(**************************************************************************************************)

PublicFunction[KroneckerCubix]

KroneckerCubix[n_] := Array[KroneckerDelta, {n, n, n}];
KroneckerCubix[{na_, nb_, nc_}] := Array[KroneckerDelta, {n, n, n}];

(**************************************************************************************************)

PublicFunction[PartialKroneckerCubices]

PartialKroneckerCubices[n_] := {
  Array[KroneckerDelta[#2, #3]&, {n, n, n}],
  Array[KroneckerDelta[#1, #3]&, {n, n, n}],
  Array[KroneckerDelta[#1, #2]&, {n, n, n}]
}

(**************************************************************************************************)

PublicFunction[RandomCubix]

RandomCubix[n_:3] := RandomInteger[{-1, 1}, {n, n, n}];
RandomCubix[n_, k_] := Table[RandomCubix[n], k];

(**************************************************************************************************)

PublicTypesettingForm[ThreeMatrixForm]

declareBoxFormatting[
  ThreeMatrixForm[expr_] :> ToBoxes[expr /. m_List ? ThreeArrayQ :> SingleThreeMatrixForm[m]],
  SingleThreeMatrixForm[expr_] :> threeMatrixFormBoxes @ expr
];

threeMatrixFormBoxes[expr_] := ToBoxes @ Row[CompactMatrixForm[#, Factor -> False, FrameStyle -> $LightBlue]& /@ expr]
