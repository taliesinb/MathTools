PublicGraphicsPrimitive[ExtendedArrow]

(* SetUsage @ "
ExtendedArrow[path$] is a more featureful alternative to %Arrow.
* The following options that specify the style of the shaft are supported:
| %ArrowheadPosition | scaled arrowhead position between 0 and 1 along the path |
| %ArrowheadLength | size of arrowhead, defaulting to a fraction of arrow length |
| %ArrowThickness | thickness of the arrow shaft and head |
| %ArrowColor | color of arrow shaft and head |
| %ArrowOpacity | opacity of arrow shaft and head |
| %ArrowDashing | dashing of the arrow shaft |
| %Setback | setback distance of path endpoints |
| %ArrowPathOffset | offset the path of the arrow shaft |
* %ArrowPathOffset can be used to duplicate the path to make double, etc. lines, see its usage.
* additional options can customize the arrowhead:
| %ArrowheadPlane | how to orient the arrowhead primitives when in 3D |
| %ArrowheadShape | the named shape of the arrowhead, as in %NamedIcon |
| %ArrowheadColor | override the color of the arrowhead |
| %ArrowheadThickness | override the thickness of the arrowhead |
| %ArrowheadAnchor | where along the arrowhead it is anchored to arrowhead position |
| %ArrowheadOpacity | opacity of the arrowhead |
* colors can be specified as %SolidEdgeForm, which tints the edge and face of arrowhead differently.
"
 *)
Options[ExtendedArrow] = $extendedArrowOptions;

DeclareGraphicsPrimitive[ExtendedArrow, "Curve", extendedArrowBoxes, {2, 3}];

(**************************************************************************************************)

extendedArrowBoxes[ExtendedArrow[points_, opts___Rule]] := Scope[

  UnpackAssociationSymbols[
    {opts} -> $MakeBoxesStyleData,
    arrowheadPosition, arrowheadLength,
    arrowColor, arrowOpacity, arrowThickness, arrowDashing,
    arrowPathSetback, arrowPathOffset, setback, arrowheadColor
  ];
  UnpackAssociationSymbols[{opts} -> {}, arrowheadAnchor];

  (* turn initial curve into points and do simple preprocessing *)
  points //= CurveToPoints;
  If[points === {}, Return @ {}];
  is3d = CoordinateMatrix3DQ @ points;
  SetAutomatic[setback, arrowPathSetback]; (* legacy *)
  SetAutomatic[setback, 0];
  points = SetbackCoordinates[points, setback];
  center = Mean @ points;
  lineLength = LineLength[points];

  (* diplace and possibly duplicate the original curve *)
  {curvePoints, isMulti} = applyPathOffset[points, arrowPathOffset];

  (* calculate the arrowhead position and corrsponding anchor *)
  SetAutomatic[arrowheadPosition, 1.0];
  If[MatchQ[arrowheadPosition, Offset[_ ? NumericQ, _ ? NumericQ]],
    arrowheadPosition = L[arrowheadPosition] + (F[arrowheadPosition] / lineLength)];
  SetAutomatic[arrowheadAnchor, arrowheadPosition];

  (* ArrowPathOffset -> PerpendicularOffset[{..}] will generate multiple curves, we must
  put the arrowhead on the original pre-offset curve *)
  points = If[isMulti, points, curvePoints];
  {pos, dir} = VectorAlongLine[RemoveOffsets @ points, Scaled @ arrowheadPosition];

  (* prevent shaft from poking through if arrowhead is at endpoints *)
  If[arrowThickness > 1,
    offset = dir * arrowThickness/2;
    Which[
      arrowheadPosition == 1, points //= MapAt[Offset[-offset, #]&, -1],
      arrowheadPosition == 0, points //= MapAt[Offset[offset, #]&, 1],
      True, Null
    ];
  ];

  (* generate the arrowhead boxes *)
  SetAutomatic[arrowheadLength, lineLength * 0.05];
  SetAutomatic[arrowheadColor, Rep[arrowColor, SolidEdgeForm[c_] :> SolidEdgeForm[c, Auto]]];
  arrowhead = arrowheadBoxes[Arrowhead[pos, dir,
    ArrowheadPlane -> PlaneRightTowards[center],
    ArrowheadLength -> arrowheadLength,
    ArrowheadAnchor -> arrowheadAnchor,
    ArrowheadThickness -> arrowThickness,
    FilterOptions @ opts,
    ArrowheadColor -> arrowheadColor
  ]];

  (* final styled boxes *)
  line = Construct[If[is3d, Line3DBox, LineBox], curvePoints];
  styler = ShaftStyleBoxOperator[arrowColor, arrowOpacity, arrowThickness, arrowDashing];
  {styler @ line, arrowhead}
]

(**************************************************************************************************)

PrivateFunction[applyPathOffset]

PublicHead[AlignedOffset]

SetUsage @ "
ArrowPathOffset is an option to ExtendedArrow and takes the following settings:
| None | no adjustment |
| vec$ | apply a vector offset |
| %Offset[vec$] | apply an absolute offset |
| %Offset[{i$, f$}] | apply initial and final offsets |
| %AlignedOffset[x$] | offset a distance x$ along path |
| %AlignedOffset[{x$, y$}] | offset x$ along path and y$ perp |
| %AlignedOffset[{{$$}, {$$}}] | initial and final offsets |
| {$$} | generate multiple curves with different offsets |
"

applyPathOffset[path_, offset_] := Scope[
  $points = path; $isMulti = False;
  $alignMatrix := $alignMatrix = Transpose @ RotateToMatrix[coordNormDelta @@ FirstLast[$points]];
  List[iApplyPathOffset @ offset, $isMulti]
];

iApplyPathOffset = Case[

  None                      := $points;

  coord:$CoordP             := $points + Threaded[coord];

  Offset[o:$CoordP]         := SimplifyOffsets[OffsetOperator[o] /@ $points];
  Offset[{i:$CoordP, f:$CoordP}] := SimplifyOffsets @ MapFirstLast[OffsetOperator /@ {i, f}] @ $points;

  AlignedOffset[p:$NumberP] := % @ AlignedOffset @ {p, 0};
  AlignedOffset[p_List]     := % @ Offset @ Dot[p, $alignMatrix];

  list_List                 := Map[$isMulti = True; %, list];

  other_                    := (Message[ExtendedArrow::badoffset, other]; $points);

];

ExtendedArrow::badoffset = "Bad specification ArrowPathOffset -> ``.";

pathOrthogonalVector[points_ ? ContainsOffsetsQ] := pathOrthogonalVector[RemoveOffsets @ points];
pathOrthogonalVector[points_] := VectorRotate90CW @ N @ Normalize[(L[points] - F[points])];


