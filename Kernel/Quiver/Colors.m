PublicFunction[LookupCardinalColors]

SetUsage @ "
LookupCardinalColors[quiver$] returns the association of cardinals to colors for quiver$.
LookupCardinalColors[quiver$, c$] returns the color for cardinal c$.
* The annotation CardinalColors is returned if present.
* The cardinals are given in sorted order.
* If the graph has no tagged edges, <||> is returned.
* If c$ is an CardinalSet, the corresponding colors will be blended.
"

LookupCardinalColors[graph_Graph] := Scope[
  UnpackExtendedThemedOptions[graph, cardinalColorRules, cardinalColors, cardinalColorFunction];
  cardinals = CardinalList @ graph;
  Which[
    cardinals === None,
      <||>,
    ColorVectorQ[cardinalColors] && SameLengthQ[cardinalColors, cardinals],
      AssociationThread[cardinals, cardinalColors],
    AssociationQ[cardinalColors],
      cardinalColors,
    ColorQ[cardinalColors],
      ConstantAssociation[cardinals, cardinalColors],
    AssociationQ[cardinalColorFunction],
      AssociationThread[cardinals, Lookup[cardinalColorFunction, cardinals, $Gray]],
    cardinalColorFunction =!= None,
      AssociationMap[cardinalColorFunction, cardinals],
    RuleListQ @ cardinalColorRules,
      AssociationThread[
        cardinals,
        VectorReplace[cardinals, Append[cardinalColorRules, _ -> $Gray]]
      ],
    True,
      ChooseCardinalColors @ cardinals
  ]
];

(* if you look up a programmatically generated color for a cardinal not present in the cardinal list,
we can still make it work properly: used for glued graphs *)
LookupCardinalColors[graph_Graph, card_] /; LookupExtendedOption[graph, CardinalColorFunction] =!= None := Scope[
  UnpackExtendedThemedOptions[graph, cardinalColorFunction];
  If[ListQ[card],
    AssociationMap[cardinalColorFunction, card],
    cardinalColorFunction @ card
  ]
];

LookupCardinalColors[graph_Graph, card_] :=
  Lookup[LookupCardinalColors @ graph, card, Gray];

LookupCardinalColors[graph_Graph, CardinalSet[cards_List]] :=
  HumanBlend @ Sort @ Lookup[LookupCardinalColors @ graph, cards, Gray];

LookupCardinalColors[_] := $Failed;

(**************************************************************************************************)

PublicFunction[ChooseCardinalColors]

ChooseCardinalColors[None, ___] := <||>;

$xyzColors = <|"x" -> $Red, "y" -> $Green, "z" -> $Blue|>;
$xyColors = <|"x" -> $Red, "y" -> $Blue|>;
$rgbwxColors = <|
  "r" -> $Red, "g" -> $Green, "b" -> $Blue,
  "rb" -> $Purple, "rg" -> $Orange, "gb" -> $Teal,
  "w" -> $Gray, "x" -> $DarkGray
|>;

$colorFormed := $colorFormed = Map[Blank, $colorFormP];

ChooseCardinalColors[cardinals_List, palette_:Automatic] := Switch[Sort @ cardinals,
  {___, "f", ___},
    Append[ChooseCardinalColors[DeleteCases[cardinals, "f"]], "f" -> $Orange],
  {"x"},
    <|"x" -> $Red|>,
  {"x", "y"},
    $xyColors,
  {"x", "y", "z"},
    $xyzColors,
  {Repeated[$colorFormed]},
    AssociationThread[cardinals, StyleFormData /@ Part[cardinals, All, 0]],
  set_ /; SubsetQ[Keys @ $rgbwxColors, set],
    KeyTake[$rgbwxColors, cardinals],
  _,
    AssociationThread[cardinals, ToColorPalette[palette, Length @ cardinals]]
];