Typeset`MakeBoxes[Rule[sym_Symbol /; KeyQ[$MakeBoxesStyleData, sym], LHS_] ? System`Dump`HeldOptionQ, fmt_, head_] := (Set[$MakeBoxesStyleData[sym], LHS]; {})

(* Scan[
  sym |-> (
    Typeset`MakeBoxes[Rule[sym_Symbol /; KeyExistsQ[$MakeBoxesStyleData, sym], LHS_] ? System`Dump`HeldOptionQ, fmt_, head_] := (Set[$MakeBoxesStyleData[sym], Print["Setting ", sym]; LHS]; {})
  ),
  Keys @ $MakeBoxesStyleData
];
 *)