PublicHead[CenteredCuboid, CenteredRectangle]

CenteredCuboid[pos_] := CenteredCuboid[pos, 1];
CenteredRectangle[pos_] := CenteredRectangle[pos, 1];

With[{c3p = $Coord3P, c2p = $Coord2P, np = $NumberP},
Typeset`MakeBoxes[cc:CenteredCuboid[pos:c3p, sz:c3p|np], StandardForm | TraditionalForm, Graphics3D] :=
  With[{p1 = pos - sz/2, p2 = pos + sz/2}, InterpretationBox[CuboidBox[p1, p2], cc]];

Typeset`MakeBoxes[cr:CenteredRectangle[pos:c2p, sz:c2p|np], StandardForm | TraditionalForm, Graphics] :=
  With[{p1 = pos - sz/2, p2 = pos + sz/2}, InterpretationBox[RectangleBox[p1, p2], cr]]
];

(**************************************************************************************************)

PublicFunction[HorizontalLine, VerticalLine, DepthLine]

HorizontalLine[{xmin_, xmax_}, rest__] := Line[tupled[{{xmin, ##}, {xmax, ##}}&][rest]];

VerticalLine[x_, {ymin_, ymax_}, rest___] := Line[tupled[{{#1, ymin, ##2}, {#1, ymax, ##2}}&][x, rest]];

DepthLine[x_, y_, {zmin_, zmax_}] := Line[tupled[{{##, zmin}, {##, zmax}}&][x, y]];

tupled[f_][args:(Except[_List]..)] := f[args];
tupled[f_][args__] := ApplyTuples[f, ToList /@ {args}];

(**************************************************************************************************)

(* CoordinateComplex is the generalization of Point, HorizontalLine, VerticalLine, etc. It's n'th argument
represents the n'th coordinate, and any lists are implicitly treated as a set of elements whose
cartesian product will be taken. Any CoordinateSegment[begin, end] will be treated as a segment on that
axis. *)

PublicFunction[CoordinateComplex]
PublicHead[CoordinateSegment]

CoordinateComplex[{___, {}, ___}] := {};

CoordinateComplex[coords_] := KeyValueMap[ccMake, KeySort @ GroupPairs @ ApplyTuples[ccElement, ToList /@ coords]];

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
