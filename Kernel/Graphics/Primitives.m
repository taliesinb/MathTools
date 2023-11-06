PublicFunction[ShapeFromString]

ShapeFromString[str_Str] := Scope[
  $f = 1;
  Prepend[{0,0}] @ Accumulate @ Map[
    $f * Lookup[$locs, StringTrim @ #, Print[#]]&,
    StringSplit[str]
  ]
];

$s2 = 1/Sqrt[2];
$s3 = Sqrt[3]/2;
$locs = Assoc[
  "" -> {0, 0},
  "+" :> ($f *= 2; {0,0}), "-" :> ($f /= 2; {0,0}),
  "n" -> {0, 1}, "e" -> {1, 0}, "s" -> {0, -1}, "w" -> {-1, 0},
  "ne" -> {$s2, $s2}, "se" -> {$s2, -$s2}, "sw" -> {-$s2, -$s2}, "nw" -> {-$s2, $s2},
  "120" -> {$s3, -1/2}, "240" -> {-$s3, -1/2},
  "*" :> ($f *= Sqrt[2]; {0, 0}),
  "/" :> ($f /= Sqrt[2]; {0, 0})
];

(**************************************************************************************************)

PublicGraphicsDirective[PatternShading3D]

DeclareGraphicsPrimitive[PatternShading3D, "Color", patternShading3DBoxes, {3}];

patternShading3DBoxes[PatternShading3D[color:$ColorPattern, density:$NumberP:5]] :=
  ToGraphics3DBoxes @ toSurfaceAppearance[
    "UseScreenSpace" -> 1, "FeatureColor" -> color,
    "Shape" -> "Line", "StepCount" -> 1, "Tiling" -> {1, 1}*density,
    "IsTwoTone" -> 1
  ];

Options[toSurfaceAppearance] = {
  "StepCount" -> 1, "Tiling" -> {5, 5},
  "FeatureColor" -> Black, "UseScreenSpace" -> 1, "IsTwoTone" -> 1,
  "LuminanceModifier" -> 0.0, "Shape" -> "Disk"
};

(* https://mathematica.stackexchange.com/questions/240690/regionplot3d-knotdata/240859#240859 *)

toSurfaceAppearance[opts:OptionsPattern[toSurfaceAppearance]] :=
  SurfaceAppearance["RampShading",
    Sequence @@ FilterRules[{opts, Options[toSurfaceAppearance]}, Except["Shape"]],
    "Arguments" -> {"HalftoneShading", 0.5, Red, OptionValue["Shape"]},
    EdgeForm[], Texture["HalftoneShading" <> OptionValue["Shape"]]]

(**************************************************************************************************)

PublicGraphicsPrimitive[AnnotatedCoordinate]

(* AnnotatedCoordinate gets transformed by MapPrimitiveCoordinates but doesn't actually render.
used to keep track of named coordinates as they are transformed for later processing. *)

DeclareGraphicsPrimitive[AnnotatedCoordinate, "Matrix", {}&];

(**************************************************************************************************)

(* CoordinateComplex is the generalization of Point, HorizontalLine, VerticalLine, etc. It's n'th argument
represents the n'th coordinate, and any lists are implicitly treated as a set of elements whose
cartesian product will be taken. Any CoordinateSegment[begin, end] will be treated as a segment on that
axis. *)

PublicFunction[CoordinateComplex]
PublicHead[CoordinateSegment]

CoordinateComplex[{___, {}, ___}] := {};

CoordinateComplex[coords_, patt_:All] :=
  KeyValueMap[ccMake, KeySelect[orderMatchQ[patt]] @ KeySort @ GroupPairs @ ApplyTuples[ccElement, ToList /@ coords]];

orderMatchQ[All] = True&;
orderMatchQ[list_List][i_] := MemberQ[list, Round @ i];
orderMatchQ[i_][j_] := i == j;

ccMake[0, p_] := Point[p];
ccElement[p__] :=
  0 -> {p};

ccMake[1, p_] := Line[p];
ccElement[p___, CoordinateSegment[l_, h_], q___] :=
  1 -> {
    {p, l, q},
    {p, h, q}
  };

ccMake[2, {p_}] := Rectangle @@ p;
ccMake[2, p_] := Rectangle @@@ p;
ccElement[CoordinateSegment[al_, ah_], CoordinateSegment[bl_, bh_]] :=
  2 -> {
    {al, bl},
    {ah, bh}
  };

ccMake[2., p_] := Polygon @ p;
ccElement[p___, CoordinateSegment[al_, ah_], q___, CoordinateSegment[bl_, bh_], r___] :=
  2. -> {
    {p, al, q, bl, r},
    {p, al, q, bh, r},
    {p, ah, q, bh, r},
    {p, ah, q, bl, r}
  };

ccMake[3, {p_}] := Cuboid @@ p;
ccMake[3, p_] := Cuboid @@@ p;
ccElement[p___, CoordinateSegment[al_, ah_], q___, CoordinateSegment[bl_, bh_], r___, CoordinateSegment[cl_, ch_], s___] :=
  3 -> {
    {p, al, q, bl, r, cl, s},
    {p, ah, q, bh, r, ch, s}
  };

(**************************************************************************************************)

