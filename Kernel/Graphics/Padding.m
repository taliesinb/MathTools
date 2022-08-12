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
  If[RealMatrixQ[padding] && Dimensions[padding] === {2, 2},
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
"

StandardizePadding = Case[
  All                   := All;
  None                  := {{0, 0}, {0, 0}};
  p_ ? NQ               := N @ {{p, p}, {p, p}};
  {pw_ ? NQ, ph_ ? NQ}  := N @ {{pw, pw}, {ph, ph}};
  spec:{{_ ? NQ, _ ? NQ}, {_ ? NQ, _ ? NQ}} := N @ spec;

  rule_Rule             := % @ {rule};
  rules:{Rule[sideP, _ ? NQ]...} :=
    N @ LookupSide[rules, {{Left, Right}, {Bottom, Top}}];

  _ := $Failed;

  {NQ -> NumericQ, sideP -> $SidePattern|Horizontal|Vertical|All}
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
