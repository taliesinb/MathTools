LoadShortcuts["Categories"]

(**************************************************************************************************)

NamedDiagram["Composition/Square"] := CommutativeSquare[
	{$OA, $OB, $OD, $OC},
	{LineMorphism[$Ak],
   LineMorphism[SpacedCompositionForm[$Al, $Ak, $Aj]],
   Reversed @ LineMorphism[$Aj],
   LineMorphism[$Al]},
  LabelRectification -> False,
  DiagramScaling -> .8
]

(**************************************************************************************************)

NamedDiagram["Composition/Triangle"] := CompositionTriangleDiagram[
	{$OX, $OY, $OZ},
	{$Af, $Ag, SpacedCompositionForm[$Ag,$Af]},
	DefaultMorphism -> LineMorphism
]

(**************************************************************************************************)

NamedDiagram["Composition/TriangleFunctorImage"] := CompositionTriangleDiagram[
  {$OX, $OY, $OZ},
  {LineMorphism[$Af,-1], LineMorphism[$Ag], LineMorphism[SpacedCompositionForm[$Ag,$Af]]},
  CloneOptions -> {"FullFunctor", "ExteriorLinkOptions" -> {ArrowheadSize -> 5}},
  GraphicsScale -> 250, CloningFunction -> {FramedForm, FramedForm}
]