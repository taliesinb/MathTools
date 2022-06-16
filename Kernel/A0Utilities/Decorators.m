PackageExport["InvertedQ"]

SetRelatedSymbolGroup[InvertedQ, Inverted, StripInverted, InvertReverse]

SetUsage @ "
InvertedQ[e$] returns True if e$ has head Inverted.
"

InvertedQ[_Inverted] = True;
InvertedQ[_] = False;

(**************************************************************************************************)

PackageExport["Modulo"]

SetUsage @ "
Modulo[n$] represents a modulo dimension n$.
* Modulo threads over lists.
"

SetListable[Modulo];

declareBoxFormatting[
  Modulo[e_] :> MakeBoxes @ ModuloForm[e]
];


(**************************************************************************************************)

PackageExport["StripModulo"]

SetUsage @ "
StripModulo[e$] removes the head Modulo if present on e$.
* StripModulo threads over lists.
"

SetListable[StripModulo];

StripModulo = Case[
  Modulo[e_]  := e;
  e_          := e;
];

(**************************************************************************************************)

PackageExport["GetModulus"]

SetUsage @ "
GetModulus[e$] returns n$ when given Modulus[n$], else returns Infinity.
GetModulus[e$, else$] returns else$ instead of Infinity.
* GetModulus threads over lists.
"

SetListable[GetModulus];

GetModulus[e_] := GetModulus[e, Infinity];
GetModulus[Modulo[n_], _] := n;
GetModulus[list_List, else_] := Map[GetModulus[#, else]&, list];
GetModulus[_, else_] := else;

(**************************************************************************************************)

PackageExport["Inverted"]

SetUsage @ "
Inverted[elem$] represents the inversion of elem$.
* Inverted[a$ \[DirectedEdge] b$] represents the edge a$ \[DirectedEdge] b$ traversed in the reverse direction.
* %DirectedEdge[a$, b$, Inverted[c$]] evaluates to %DirectedEdge[b$, a$, c$].
* Inverted[Inverted[c$]] evaluates to c$.
* Inverted[c$] display as %Underbar[c$].
"

Inverted[Inverted[e_]] := e;
Inverted /: DirectedEdge[a_, b_, Inverted[c_]] := DirectedEdge[b, a, c];
Inverted[CardinalSet[cards_]] := CardinalSet[Inverted /@ cards];

declareBoxFormatting[
  Inverted[e_] :> InvertedBoxForm[e]
];

(**************************************************************************************************)

PackageExport["InvertReverse"]

SetUsage @ "
InvertReverse[list$] applies Inverted to elements of list$, then reverses the list.
"

InvertReverse[e_List] := Reverse @ Map[Inverted, e];

(**************************************************************************************************)

PackageExport["StripInverted"]

SetUsage @ "
StripInverted[e$] removes the head Inverted if present on e$.
* StripInverted does not map over lists.
"

StripInverted = Case[
  Inverted[e_] := e;
  e_          := e;
];

(**************************************************************************************************)

PackageExport["InvertedForm"]

SetUsage @ "
InvertedForm[e$] displays as Underbar[e$].
"

declareBoxFormatting[
  InvertedForm[e_] :> InvertedBoxForm[e]
];

(**************************************************************************************************)

PackageExport["AtLeast"]

SetUsage @ "
AtLeast[n$] is a symbolic expression indicating that at least n$ values should be obtained.
"

declareFormatting[
  AtLeast[n_] :> Row[{">", n}]
];
