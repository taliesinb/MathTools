PublicFunction[P1, P2, P3, PN]

P1 = First;
P2[e_] := Part[e, 2];
P3[e_] := Part[e, 3];
PN[e_] := Part[e, -1];

PublicFunction[P11, P12, P21, P22]

P11[e_] := Part[e, 1, 1];
P12[e_] := Part[e, 1, 2];
P21[e_] := Part[e, 2, 1];
P22[e_] := Part[e, 2, 2];

PublicFunction[PA1, PA2, PA3, PAN]

PA1[e_] := Part[e, All, 1];
PA2[e_] := Part[e, All, 2];
PA3[e_] := Part[e, All, 3];
PAN[e_] := Part[e, All, -1];
