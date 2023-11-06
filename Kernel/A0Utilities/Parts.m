PublicFunction[P2, P3]

(* H, P1, PN are aliases *)
P2[e_] := Part[e, 2];
P3[e_] := Part[e, 3];

PublicFunction[P11, P12, P13, P1N, P21, P22, P23, P2N, P31, P32, P33, P3N]

P11[e_] := Part[e, 1, 1];
P12[e_] := Part[e, 1, 2];
P13[e_] := Part[e, 1, 3];
P1N[e_] := Part[e, 1, -1];

P21[e_] := Part[e, 2, 1];
P22[e_] := Part[e, 2, 2];
P23[e_] := Part[e, 2, 3];
P2N[e_] := Part[e, 2, -1];

P31[e_] := Part[e, 3, 1];
P32[e_] := Part[e, 3, 2];
P33[e_] := Part[e, 3, 3];
P3N[e_] := Part[e, 3, -1];

PublicFunction[PA1, PA2, PA3, PAN]

PA1[e_] := Part[e, All, 1];
PA2[e_] := Part[e, All, 2];
PA3[e_] := Part[e, All, 3];
PAN[e_] := Part[e, All, -1];
