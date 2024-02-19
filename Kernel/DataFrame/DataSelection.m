(**************************************************************************************************)

DefineStandardTraditionalForm[
  HoldPattern[DataSelection[inds_Integer]] ? HoldNoEntryQ :> dataIndicesBoxes[k, v, n]
];

dataIndicesBoxes[inds_] := RowBox[{
  "\"DataSelection\"", RowBox[{"[", tightColoredBoxes[IntStr @ Len @ indices, $LightOrange, 12], "]"}]
}];
