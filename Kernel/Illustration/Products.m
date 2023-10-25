PublicFunction[GraphProductsRow]

GraphProductsRow[{l1_ -> q1_, l2_ -> q2_}, prodSeq:Repeated[{__Rule}]] := Scope[
  q1 = RotateGraph @ q1;
  opts = Sequence[ArrowheadPosition -> 0.65, ImagePadding -> 15, Frame -> True];
  prods = {prodSeq};
  numProds = Length @ First @ List @ prodSeq;
  topRow = Prepend[""] @ ConstantArray[q2, numProds];
  prodRows = Catenate @ Map[
    Function[prods,
      {prodLabels, prodFns} = KeysValues @ prods;
      bottomRow = Prepend[""] @ Map[#[l1, l2]&, prodLabels];
      {
        Prepend[q1] @ Map[#[q1, q2, opts]&, prodFns],
        bottomRow
      }
    ],
    {prodSeq}
  ];
  rows = Prepend[prodRows, topRow];
  Grid[
    rows,
    Spacings -> {{0,0, {1, 1.5}}, {0,0, {1, 1.5}}}
  ]
];

(**************************************************************************************************)

PublicFunction[GraphProductTable]

Options[GraphProductTable] = JoinOptions[
  "Labels" -> {None, None},
  "UseCardinalSet" -> False,
  ArrowheadPosition -> 0.525,
  ImagePadding -> 15,
  ExtendedGraph,
  Grid
];

GraphProductTable[prodFn_, aList_, bList_, opts:OptionsPattern[]] := Scope[
  UnpackOptions[labels, useCardinalSet, arrowheadPosition];
  aList = RotateGraph /@ aList;
  entries = Outer[
    prodFn[#1, #2,
      PackingSpacing -> 1, "UseCardinalSet" -> useCardinalSet,
      FilterOptions[First @ PatternHead @ prodFn, opts],
      (* If[!useCardinalSet, ArrowheadShape -> None, Sequence @@ {}], *) MultiEdgeDistance -> 0.1,
      ArrowheadPosition -> arrowheadPosition, EdgeSetback -> .1, ImagePadding -> 15,
      Frame -> True
    ]&,
    aList,
    bList,
    1
  ];
  {aLabels, bLabels} = labels;
  table = PrependColumn[entries, aList];
  If[aLabels =!= None,
    blank = Splice[{"", ""}];
    table = PrependColumn[table, aLabels];
  ,
    blank = ""];
  PrependTo[table, Prepend[blank] @ bList];
  If[bLabels =!= None, PrependTo[table, Prepend[blank] @ bLabels]];
  Grid[table, FilterOptions @ opts, Spacings -> {{0,0,0.25,{1}, 0},{0,0,0.25,{1}, 0}}]
];

(**************************************************************************************************)

PublicFunction[GraphProductUnionSpacedRow]

productMeanPos[vertices_] := N @ Mean[List @@@ vertices];
GraphProductUnionSpacedRow[full_, items_, opts___Rule] := Scope[
  items = SortBy[items, ApplyThrough[{VertexList /* productMeanPos, VertexCount}]];
  items = ShowFaded[full, #, .85]& /@ items;
  SpacedRow[items, RiffleItem -> "\[Union]", opts, MaxWidth -> 6, RowSpacings -> 15]
];

(**************************************************************************************************)

PublicFunction[ConnectedComponentProductDecomposition]

Options[ConnectedComponentProductDecomposition] = JoinOptions[
  {MaxWidth -> 4, Spacings -> 15, Transposed -> False},
  ExtendedGraph
];

ConnectedComponentProductDecomposition[graphs_, terms_, userOpts:OptionsPattern[]] := Scope[
  If[graphs ~~~ l_Labeled,
    displayForm = toQuiverProductColumn @ Last @ graphs;
    graphs = First @ graphs;
  ,
    displayForm = {Automatic};
  ];
  UnpackOptions[maxWidth, spacings, transposed];
  opts = SeqDeleteOptions[{MaxWidth, Spacings, Transposed}][userOpts];
  base = GeneralQuiverProduct[graphs, terms, Automatic, opts,
    ImageSize -> 120, VertexSize -> 4, ArrowheadShape -> None,
    VertexOverlapResolution -> 0
  ];
  products = GeneralQuiverProduct[graphs, terms, All, opts,
    ImageSize -> 120, VertexSize -> 4, ArrowheadSize -> 12];
  imgSize = First @ LookupImageSize[base];
  dislayForm = VectorReplace[displayForm, {Automatic -> base, g_Graph :> ReplaceOptions[g, ImageSize -> imgSize]}];
  SpacedColumn[
    Sequence @@ dislayForm,
    LargeSymbolForm["="],
    GraphProductUnionSpacedRow[base, products,
      MaxWidth -> maxWidth, Spacings -> spacings, Transposed -> transposed],
    Spacings -> 45
  ]
];

toQuiverProductColumn = Case[
  RightFreeQuiverProductForm[a_, b_] :=
    {toSimpleQuiver @ a, LargeSymbolForm @ RightFreeQuiverProductForm[], toSimpleQuiver @ b};
  LeftFreeQuiverProductForm[a_, b_] :=
    {toSimpleQuiver @ a, LargeSymbolForm @ LeftFreeQuiverProductForm[], toSimpleQuiver @ b};
  LockedQuiverProductForm[a_, b_] :=
    {toSimpleQuiver @ a, LargeSymbolForm @ LockedQuiverProductForm[], toSimpleQuiver @ b};
  other_ := {OnFailed[toSimpleQuiver @ other, other]};
];

(**************************************************************************************************)

PublicFunction[QuiverProductTable]

QuiverProductTable[quivers_, termsLists_, args___] := Scope[
  makePlot = Labeled[
    GeneralQuiverProduct[quivers, #2, args, ImageSize -> {100, 100}, ArrowheadStyle -> $Gray],
    Row[{Style[#1, Bold], Invisible @ "X"}]
  ]&;
  SpacedColumn[
    SpacedRow[makePlot @@@ #, Spacings -> 50, LabelSpacing -> 15]& /@ termsLists,
    Spacings -> 50
  ]
];
