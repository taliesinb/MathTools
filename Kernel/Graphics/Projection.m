PublicFunction[Graphics3DProjection]
PublicOption[ZFade]

Options[Graphics3DProjection] = JoinOptions[
  ZFade -> None, (* TODO: capture z values of all coordinates in a primitive, so that we can fade it properly *)
  Graphics
];

Graphics3DProjection[g:Graphics3D[primitives_, ___], opts:OptionsPattern[]] := Scope[

  UnpackOptions[$zFade];
  trans = ConstructGraphicsViewTransform @ g;
  $gvtAssoc = Last[trans];
  $xyz = ToXYZFunction @ trans;
  $style = {Black, Opacity[1]};
  $index = Bag[];

  $scale := $scale = PlotRangeScale @ g;

  $zfactor = 1; (* If[$gvtAssoc["ViewProjection"] === "Orthographic", -1, 1]; *)
  procPrim @ primitives;
  $index = KeySort @ Merge[BagPart[$index, All], Identity];
  {$zmin, $zmax} = MinMax @ FirstColumn @ Keys @ $index;

  primitives2D = KeyValueMap[If[$zFade =!= None, fromZandStyleFaded, fromZandStyle], $index];

  Graphics[primitives2D, FilterOptions @ opts]
];

(* todo: capture other common things in special positions like EdgeForm, FaceForm, etc *)
fromZandStyleFaded[{z_, {color_, style___}}, prim_] := Style[prim, Lighter[color, ($zmax - z) / ($zmax - $zmin) / 2], style];

fromZandStyle[{z_, {}}, prim_] := prim;
fromZandStyle[{z_, style_}, prim_] := Style[prim, Seq @@ style];

$gcCoords = $gcXYZ = None;

procPrim = Case[

  (* recurse through lists *)
  list_List                        := InheritedBlock[{$style}, Scan[%, list]];

  (* apply styles *)
  Style[e_, s___]                  := InheritedBlock[{$style}, Scan[procStyle, {s}]; % @ e];

  GraphicsComplex[coords_, prims_] := Scope[$gcXYZ = $xyz @ ($gcCoords = coords); procPrim @ prims];

  (* pass through wrappers *)
  w:$AnnotationP                   := % @ First @ w;

  (* TODO: Handle FilledCurve with custom recurser *)
  p_Point                          := insertScalar @ p;
  p:$ArcP                          := insertVector @ p;
  p:(_Ball|_Sphere)                := procSphere @@ p;

  (* convert Polyhedrons to their surface polygons *)
  (* convert complex regions to Polygons *)
  t_Tube                           := procTube @ t;
  p:$RegionP                       := procPolygon @ RegionPolygon @ p;
  p_Polyhedron                     := procPolygon @ (Polygon @@ p);
  p_Polygon                        := procPolygon @ p;

  i_Inset                          := procInset @ i;
  t_Text                           := procText @ t;

  (* assume unmatched things are style direcives *)
  e_                               := procStyle @ e;
,
  {mq -> CoordinateMatrixQ, msq -> CoordinateMatricesQ}
];

(**************************************************************************************************)

procInset = Case[

  Inset[g_, pos_, rest___] := Scope[
    {x, y, z} = $xyz @ pos;
    insert[z, Inset[g, {x, y}, rest]]
  ];

];

(**************************************************************************************************)

procTube = Case[

  Tube[spec_] := % @ Tube[spec, Scaled[.004]];

  Tube[spec_, Scaled[r_]] := % @ Tube[spec, r * $scale];

  t_Tube := procPolygon @ RegionPolygon @ t;

];

(**************************************************************************************************)

procSphere = Case[
  
  Seq[p_] := procSphere[p, 1];

  Seq[ind_ /; PositiveIntegerVectorQ[ind] || PositiveIntegerQ[ind], r_] :=
    %[Part[$gcCoords, ind], r];

  Seq[coords_ ? CoordinateMatrixQ, r_] :=
    Scan[%[#, r]&, coords];

  (* TODO: Calculate this more efficiently *)
  Seq[pos_, r_] := Scope[
    {x, y, z} = $xyz @ pos;
    insert[z, Disk[{x, y}, screenSpaceRadius[pos, r]]];
  ]

];

(**************************************************************************************************)

screenSpaceRadius[pos_, r_] := Scope[
  {cameraPoint, lookAtPoint} = $gvtAssoc["ViewVector"];
  relativePos = pos - cameraPoint;
  perp = Normalize @ Cross[relativePos, relativePos + {1., 3.1415, 9.101}];
  pos2 = pos + perp * r;
  {{x1, y1, z1}, {x2, y2, z2}} = $xyz @ {pos, pos2};
  EuclideanDistance[{x1, y1}, {x2, y2}]
];

(**************************************************************************************************)

procPolygon = Case[

  Polygon[indices_, coords_] := Scope[$gcXYZ = $xyz @ coords; % @ Polygon[indices]];

  (* TODO: we should drop the interior from Polyhedra, but not Polygons *)
  Polygon[p_List -> _]          := % @ Polygon[p];

  p_Polygon := insertVector[p];
];


procStyle := Case[
  c_ ? ColorQ             := Set[Part[$style, 1], c];
  o:Opacity[_]            := Set[Part[$style, 2], o];
  Opacity[o_, c_]         := % @ SetColorOpacity[c, o];
  Directive[{d___}]       := Scan[%, d];
  Directive[d___]         := Scan[%, d];
  s_ ? GraphicsDirectiveQ := AppendTo[$style, s];
];

(**************************************************************************************************)

insertScalar[obj_] := insertScalar[obj, xyz @ First @ obj];

(* insert single *)
insertScalar[obj_, xyz_] :=
  insert[Part[xyz, 3], ReplacePart[obj, 1 -> Take[xyz, 2]]];

(* insert multi *)
insertScalar[obj_, xyz_ ? CoordinateMatrixQ] := Scan[insertScalar[obj, #]&, xyz];

xyz[ind_] /; PositiveIntegerQ[ind] || PositiveIntegerVectorQ[ind] := ExtractIndices[$gcXYZ, ind];
xyz[coord_] := $xyz @ coord;

(**************************************************************************************************)

insertVector[obj_] := insertVector[obj, xyzMulti @ First @ obj];

(* insert single *)
insertVector[obj_, xyz_ ? CoordinateMatrixQ] :=
  insert[Mean @ Part[xyz, All, 3], ReplacePart[obj, 1 -> Take[xyz, All, 2]]];

(* insert multi *)
insertVector[obj_, xyz_] := Scan[insertVector[obj, #]&, xyz];

xyzMulti[ind_] /; PositiveIntegerVectorQ[ind] || PositiveIntegerVectorsQ[ind] := ExtractIndices[$gcXYZ, ind];
xyzMulti[coord_ ? CoordinateMatrixQ] := $xyz @ coord;
xyzMulti[coord_] := $xyz /@ coord;

(**************************************************************************************************)

insert[z_, obj_] /; $zFade === None := StuffBag[$index, {$zfactor * z, DeleteCases[$style, Black | Opacity[1]]} -> obj];
insert[z_, obj_] := StuffBag[$index, {$zfactor * z, $style} -> obj];

