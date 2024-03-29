(* TODO: turn these into literal macros via some Loader mechanism,
so they can appear on the LHS of Set! *)

PrivateFunction[P1, P2, P3, P4]

P1[e_] := Part[e, 1];
P2[e_] := Part[e, 2];
P3[e_] := Part[e, 3];
P4[e_] := Part[e, 4];

PrivateFunction[FF, FL, LL, LF]

FF[e_] := Part[e, 1, 1];
FL[e_] := Part[e, 1, -1];
LL[e_] := Part[e, -1, -1];
LF[e_] := Part[e, -1, 1];

PrivateFunction[Col1, Col2, Col3, ColL]

Col1[e_] := Part[e, All, 1];
Col2[e_] := Part[e, All, 2];
Col3[e_] := Part[e, All, 3];
ColL[e_] := Part[e, All, -1];