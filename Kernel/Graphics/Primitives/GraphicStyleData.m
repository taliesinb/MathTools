PublicHead[GraphicsStyleData]

PrivateVariable[$GraphicsStyleData]

$GraphicsStyleData = <||>;

Typeset`MakeBoxes[GraphicsStyleData[assoc_Association, primitives_], form:StandardForm | TraditionalForm, type:Graphics|Graphics3D] :=
  graphicsStyleDataBoxes[assoc, primitives, form, type];

graphicsStyleDataBoxes[assoc_, primitives_, form_, type_] := InheritedBlock[{$GraphicsStyleData},
  $GraphicsStyleData = Merge[{$GraphicsStyleData, assoc}, mergeGraphicsStyleRules];
  Typeset`MakeBoxes[primitives, form, type]
];

mergeGraphicsStyleRules[{styles_}] := ToList @ styles;
mergeGraphicsStyleRules[{styles1_, styles2_}] := Join[ToList @ styles1, ToList @ styles2];

$customGraphicsHeadQ[GraphicsStyleData] = True;

Typeset`MakeBoxes[namedStyle_String /; KeyExistsQ[$GraphicsStyleData, namedStyle], form:StandardForm | TraditionalForm, type_] := With[
  {style = $GraphicsStyleData[namedStyle]},
  Typeset`MakeBoxes[
    Directive @ style,
    form, type
  ]
]

(* Caching: first, caching the makeboxes against the named style. maybe even do it in $GraphicsStyleData.

but for things that modify the current $MakeBoxesStyleData value, this will affect $MakeBoxesStyleData but that won't get changed.
so we need to isolate and memorize the changes to $MakeBoxesStyleData.

but also, it would be nice if we could refer back to particular properties of style contexts. E.g. EdgeColor :> Darker[$BrushColor].

Here, $FaceColor will be whatever the current value of $MakeBoxesStyleData["BrushColor"] is!

now, also, we want ways to refer to coordinates directly.

there are so many things we could do.

we could have whole new graphics contexts, like ModularGraphics, ModularGraphics3D, Graphics4D, etc.
*)