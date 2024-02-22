PrivateFunction[ImageSizePad]

SetUsage @ "
ImageSizePad[size$, padding$] expands the image size size$ by padding it according to padding$.
* padding$ can be any of the specifications supported by %ImagePadding.
"

ImageSizePad::badpadding = "`` is not a valid padding spec.";

ImageSizePad[imageSize_, paddingSpec_] := Scope[
  If[!MatchQ[imageSize, {_ ? NumericQ, _ ? NumericQ}], ReturnFailed[]];
  {pw, ph} = padding = processPadding[ImageSizePad, paddingSpec];
  {w, h} = imageSize;
  {w + Total[pw], h + Total[ph]}
];

processPadding[head_, paddingSpec_] := Scope[
  padding = Ceiling @ StandardizePadding @ paddingSpec;
  If[RealMatrixQ[padding] && Dims[padding] === {2, 2},
    padding,
    ReturnFailed[MessageName[head, "badpadding"], paddingSpec]
  ]
];

(**************************************************************************************************)

PrivateFunction[StandardizePadding]

SetUsage @ "
StandardizePadding[spec$] standardizes a padding specification spec$.
* StandardizePadding returns {{l$, r$}, {b$, t$}}.
* StandardizePadding accepts the following forms:
| None | no padding |
| n$ | pad by n$ on all sides |
| {h$, v$} | pad by h$ horizontally and v$ vertically |
| {{l$, r$}, {b$, t$}} | explicit padding |
| {Left -> l$, $$} | per-side padding |
* Sides can be Horizontal or Vertical to indicate both sides.
"

StandardizePadding = Case[
  All                              := All;
  None                             := {{0, 0}, {0, 0}};
  p:$NumberP                       := N @ {{p, p}, {p, p}};
  c:$Coord2P                       := N @ c;
  s:{$Coord2P, $Coord2P}           := N @ s;

  rule_Rule                        := % @ {rule};
  rules:{Rule[sideP, $NumberP]...} := N @ LookupSide[rules, {{Left, Right}, {Bottom, Top}}];

  _                                := $Failed;
,
  {sideP -> $SidePattern|Horizontal|Vertical|All}
];

(**************************************************************************************************)

PrivateFunction[StandardizePadding3D]

SystemSymbol[Under, Over]

Under::usage = "Under is like Bottom but for the Z coordinate.";
Over::usage = "Over is like Top but for the Z coordinate.";

SetUsage @ "
StandardizePadding3D[spec$] standardizes a 3D padding specification spec$.
* StandardizePadding3D returns {{l$, r$}, {b$, t$}, {u$, o$}}.
* StandardizePadding3D accepts the following forms:
| None | no padding |
| n$ | pad by n$ on all sides |
| {x$, y$, z$} | pad on both sides on the x$, $y and $z axes |
| {{l$, r$}, {b$, t$}, {$u, o$}} | explicit padding |
| {Left -> l$, $$} | per-side padding |
"

StandardizePadding3D = Case[
  All                              := All;
  None                             := {{0, 0}, {0, 0}, {0, 0}};
  p:$NumberP                       := N @ {{p, p}, {p, p}, {p, p}};
  c:$Coord3P                       := N @ c;
  s:{$Coord2P, $Coord2P, $Coord2P} := N @ s;

  rule_Rule                        := % @ {rule};
  rules:{Rule[sideP, $NumberP]...} := N @ LookupSide[rules, {{Left, Right}, {Bottom, Top}, {Under, Over}}];

  _                                := $Failed;
,
  {sideP -> $SidePattern|Horizontal|Vertical|Under|Over|All}
];

(**************************************************************************************************)

PrivateFunction[LookupSide]

LookupSide[rules_, sides_List] :=
  Map[LookupSide[rules, #]&, sides];

LookupSide[rules_, side_] :=
  Lookup[rules, side, Lookup[rules, toSideClass @ side, Lookup[rules, All, 0]]];

LookupSide[rules_, side_] :=
  Lookup[rules, side,
  Lookup[rules, toSideClass @ side,
  Lookup[rules, toMultiClassC @ side,
  Lookup[rules, toMultiClassA @ side,
  Lookup[rules, All, 0]]]]];

toMultiClassC = <|Bottom -> BottomLeft, Left -> TopLeft, Top -> TopRight, Right -> BottomRight|>;
toMultiClassA = <|Bottom -> BottomRight, Left -> BottomLeft, Top -> TopLeft, Right -> TopRight|>;

LookupSide[rules_][side_] :=
  LookupSide[rules, side];

toSideClass = Case[
  Left|Right := Horizontal;
  Bottom|Top := Vertical;
  other_     := Null;
]
