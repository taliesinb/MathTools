SystemSymbol[Naturals, PositiveNaturals]

SetUsage @ "Naturals represents the natural numbers."
SetUsage @ "PositiveNaturals represents the positive natural numbers."

MakeBoxes[Naturals, StandardForm] := SBox["Naturals"];
MakeBoxes[Naturals, TraditionalForm] := SBox["Naturals"];

MakeBoxes[PositiveNaturals, StandardForm] := SBox["PositiveNaturals"];
MakeBoxes[PositiveNaturals, TraditionalForm] := SBox["PositiveNaturals"];

MakeBoxes[PositiveReals, StandardForm] := SBox["PositiveReals"];
MakeBoxes[PositiveReals, TraditionalForm] := SBox["PositiveReals"];

MakeBoxes[Primes, StandardForm] := SBox["Primes"];

(**************************************************************************************************)

PublicSymbol[PiSymbol, TauSymbol]

declareConstantSymbol[{PiSymbol, TauSymbol}]

(**************************************************************************************************)

PublicSymbol[NotApplicableSymbol, UnknownSymbol, EmptySetSymbol]

declareConstantSymbol[{NotApplicableSymbol, UnknownSymbol, EmptySetSymbol}];

(**************************************************************************************************)

PublicSymbol[TickSymbol]

declareConstantSymbol[TickSymbol];

(**************************************************************************************************)

PublicSymbol[UnitInterval]

declareConstantSymbol[UnitInterval];

(**************************************************************************************************)

(* Q: can i just use declareConstantSymbol here? *)

PublicSymbol[BlankSymbol]

declareBoxFormatting[
  BlankSymbol :> SBox["BlankSymbol"]
]

$TemplateKatexFunction["BlankSymbol"] = Function["\\blank"];

(**************************************************************************************************)

PublicSymbol[PlaceholderSquareSymbol]

declareBoxFormatting[
  PlaceholderSquareSymbol :> SBox["PlaceholderSquareSymbol"]
]

$TemplateKatexFunction["PlaceholderSquareSymbol"] = Function["□"];

(**************************************************************************************************)

PublicSymbol[IndeterminateSymbol]

declareConstantSymbolForm[IndeterminateSymbol];

(**************************************************************************************************)

PublicSymbol[BarTokenSymbol, FilledTokenSymbol, FilledSquareTokenSymbol, FilledRectangleTokenSymbol, EmptyTokenSymbol, EmptySquareTokenSymbol, EmptyRectangleTokenSymbol]

declareConstantSymbol[
  {BarTokenSymbol,
   FilledTokenSymbol, FilledSquareTokenSymbol, FilledRectangleTokenSymbol,
   EmptyTokenSymbol, EmptySquareTokenSymbol, EmptyRectangleTokenSymbol
}];
