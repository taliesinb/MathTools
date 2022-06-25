PublicOption[AbstractCoordinateFunction, VertexCoordinateFunction, VertexNameFunction, IncludeRepresentationMatrices, AbstractCoordinateFilter]

SetUsage @ "AbstractCoordinateFunction is an option to QuiverLattice and QuiverGraph."
SetUsage @ "VertexCoordinateFunction is an option to QuiverLattice and QuiverGraph."
SetUsage @ "VertexNameFunction is an option to QuiverLattice and QuiverGraph."
SetUsage @ "AbstractCoordinateFilter is an option to QuiverLattice and QuiverGraph."

PrivateVariable[$baseLatticeUsage]

$baseLatticeUsage = StringTrim @ "
## Exploration options

* The following options control how states are explored:
| %MaxDepth | Infinity | maximum depth from the initial states |
| %MaxVertices | Infinity | maximum number of lattice vertices to obtain |
| %MaxEdges | Infinity | maximum number of lattice edges to obtain |
| %MaxNorm | Infinity | maximal allowed norm, larger norm vertices are dropped |
| %NormFunction | Automatic | function to compute norm from abstract vertex coordinates |
| %InitialStates | Automatic | where to start the exploration. |

* %MaxVertices -> AtLeast[n$] specifies that at least n$ vertices should be collected,
but the currently explored depth should be completed before termination.

* For %NormFunction -> Automatic, %MaxNorm -> n$ will use %ChessboardNorm, which has the \
effect of allowing up to n$ moves per cardinal.

* The default %InitialStates -> Automatic uses the 'natural' initial states for the \
given machine.
For a quiver representation, the 'central' vertex is chosen, valued by the identity matrix.
For %InitialStates -> All, all vertices of the fundamental quiver are used as initial states.

## Graph options

* The following options control aspects of the produced graph:
| %CombineMultiedges | True | combine multiedges into a single edge sharing cardinals |
| %SelfLoops | True | allow self-loops |
| %CardinalColors | Automatic | how to choose colors for cardinals |

* In addition, all the options of %ExtendedGraph are supported.

## Coordinatization options

* The following options control how vertices are named and coordinatized:
| %AbstractCoordinateFunction | Automatic | function to obtain abstract vertex coordinates from representation matrices |
| %VertexCoordinateFunction | Automatic | function to obtain graphical vertex coordinates from abstract coordinates |
| %VertexNameFunction | 'SpiralIndex' | function to rename vertices after coordinatization |

* %AbstractCoordinateFunction extracts abstract vertex coordinates from RepresentationElements, and accepts these settings:
| Automatic | pick coordinates based on the structure of the machine (default) |
| {p$1, $$, p$n} | obtain particular elements of the state |
| None | use the entire state as the coordinate |
| f$ | apply f$ to each %RepresentationElement |
| {All, f$} | apply f$ to the full state |

* %AbstractCoordinateFilter is a predicate that will be applied to vertices and should return True or False.
Only vertices returning True will be retained.

* %VertexCoordinateFunction determines the graphical coordinates, and accepts the following settings:
| Automatic | convert representation coords to spatial coords based on the structure of the machine (default) |
| None | automatic layout of the vertices based on setting of GraphLayout |
| f$ | apply f$ to the abstract coordinates produced by AbstractCoordinateFunction |

* %VertexNameFunction determines the final names given to vertices, and accepts these settings:
| 'SpiralIndex' | number vertices starting at 1 for the origin, proceeding clockwise then outward (default) |
| 'RasterIndex' | number vertices starting at the top left, proceeding right then down |
| 'Coordinates' | use abstract coordinates directly as names |
| 'Representation' | use the original RepresentationElement[$$] objects directly as names |
| 'RepresentationMatrix' | the underlying matrix of the RepresentationElement[$$] |
| None | use LatticeVertex[abscoords$, genvertex$] as names |

* %GraphLayout accepts these settings:
| None | use the layout provided by VertexCoordinateFunction |
| Automatic | use 'SpringElectricalEmbedding' |
| spec$ | use a custom specification accepted by Graph |

* %GraphLegend accepts these settings:
| None | no legend |
| Automatic | legend for cardinals |
| 'Quiver' | label with the generating quiver |
| 'PathRepresentation' | label with the generating quiver representation |

## Vertex data

The following vertex is available for use with %VertexColorFunction, %VertexLabels, etc:

| 'GeneratingVertex' | corresponding vertex of the fundamental quiver |
| 'AbstractCoordinates' | abstract coordinates for the vertex |
| 'RepresentationMatrix' | representation matrix for the vertex |
| 'Norm' | the norm of the vertex, if computed |
"

PublicFunction[InitialStates]

$baseGenerateLatticeOptions = JoinOptions[
  AbstractCoordinateFunction -> Automatic,
  AbstractCoordinateFilter -> None,
  VertexCoordinateFunction -> Automatic,
  VertexNameFunction -> "SpiralIndex",
  MaxNorm -> Infinity,
  MaxDepth -> Automatic,
  NormFunction -> Automatic,
  CardinalColors -> Automatic,
  MaxVertices -> Infinity, MaxEdges -> Infinity,
  DepthTermination -> Automatic, IncludeFrontier -> Automatic,
  IncludeRepresentationMatrices -> False,
  GraphLegend -> None,
  CombineMultiedges -> True,
  SelfLoops -> True,
  InitialStates -> Automatic,
  RandomSeeding -> None,
  CardinalList -> Automatic,
  InteriorSolid -> Automatic,
  $simpleGraphOptionRules
];

General::notquivrep = "First argument should be a PathRepresentationObject, or a quiver with canonically named cardinals.";
General::badvcoords = "VertexCoordinateFunction did not return vectors of consistent dimensions. The first vector was ``, produced from ``.";
General::badlatticename = "The specified name `` is not a known name for a lattice. Known names are: ``."
General::badlatticedepth = "The specified depth `` is not a positive integer."
General::badvertnaming = "Unknown setting `` for VertexNameFunction. Valid renaming rules are ``.";
General::renamenotquiv = "The vertex coordinates yielded a quiver with incompatible cardinal edges. Use LatticeGraph instead.";
General::badcardlist = "Provided CardinalList -> `` does not match length of original cardinals ``.";
General::normempty = "The setting of NormFunction and MaxNorm -> `` yielded an empty graph. The first few norms were: ``."
General::filterempty = "The setting of AbstractCoordinateFilter yielded an empty graph.";
General::nonnumnorm = "The setting of NormFunction yielded the non-numeric value `` on vertex ``.";
General::badgconst = "Could not construct the final graph.";

Options[iGenerateLattice] = $baseGenerateLatticeOptions;

$cardinalBasedRegionPattern = Path | Line[_, _] | HalfLine | InfiniteLine | Axes;

iGenerateLattice[head_, representation_, directedEdges_, opts:OptionsPattern[]] := Scope[

  If[StringQ[representation],
    latticeName = representation;

    If[MemberQ[$LatticeClassNames, latticeName],
      Return @ Map[
        iGenerateLattice[head, #, directedEdges, opts, PlotLabel -> Automatic]&,
        LatticeQuiverData @ latticeName
      ]];

    If[KeyExistsQ[$ParameterizedLatticeData, latticeName],
      Return @ iGenerateLattice[head, {latticeName}, directedEdges, opts]];

    representation = LatticeQuiverData[latticeName, "Representation"];
    If[!PathRepresentationObjectQ[representation],
      ReturnFailed[head::badlatticename, latticeName, commaString @ $LatticeNames]];
  ];

  If[RuleQ[representation],
    representation = PathRepresentation @@ representation;
    If[FailureQ[representation], ReturnFailed[]];
  ];

  If[QuiverQ[representation],
    representation = Quiet @ PathRepresentation[representation]];

  If[GroupoidObjectQ[representation],
    representation = AssociationMap[representation, {"CayleyFunction", "InitialStates"}];
  ];

  wasAutoCardinalList = False;
  If[AssociationQ[representation] && Sort[Keys @ representation] === {"CayleyFunction", "InitialStates"},

    UnpackAssociation[representation, cayleyFunction, initialStates];
    repInitialStates = ToList[initialStates];
    wasAutoCardinalList = True;
    trueCardinalList = Union[StripInverted /@ DeepCases[cayleyFunction, Labeled[_, c_ ? notInternalSymbolQ] :> c]];
    If[trueCardinalList === {}, trueCardinalList = Automatic];
  ,
    If[!PathRepresentationObjectQ[representation],
      ReturnFailed[head::notquivrep]];

    cayleyFunction = quiverStateToLatticeVertex @ representation["CayleyFunction", "Symmetric" -> True, "Labeled" -> True];
    baseRep = representation["Representation"];
    repInitialStates = List @ quiverStateToLatticeVertex @ representation["Identity"];

    quiver = representation["Quiver"];
    trueCardinalList = CardinalList @ quiver;
  ];

  UnpackOptionsAs[head, opts,
    maxNorm, normFunction,
    abstractCoordinateFunction, abstractCoordinateFilter, vertexCoordinateFunction,
    graphLayout,
    graphLegend, imageSize, vertexNameFunction, arrowheadStyle,
    maxDepth, maxVertices, maxEdges, depthTermination, includeFrontier,
    graphMetric, combineMultiedges,
    includeRepresentationMatrices,
    graphRegionHighlight, plotLabel,
    selfLoops, initialStates, randomSeeding,
    cardinals
  ];

  SetAutomatic[initialStates, repInitialStates];
  If[IntegerQ[initialStates],
    (* allow specifying a different vertex in the fundamental quiver. *)
    initialStates = ReplacePart[repInitialStates, {1, 2} -> initialStates]];

  SetAll[initialStates, LatticeVertex @@@ representation["AllIdentities"]];

  simpleOptions = TakeOptions[{opts}, $simpleGraphOptions];

  If[IntegerQ[maxDepth], SetAutomatic[includeFrontier, False]];
  SetAutomatic[maxDepth, Which[
    maxNorm =!= Infinity,
      If[IntegerQ[maxNorm], maxNorm, Ceiling[maxNorm] + 1] * Length[trueCardinalList],
    maxVertices =!= Infinity,
      Infinity,
    AssociationQ[representation],
      Infinity,
    True,
      maxNorm = Replace[latticeName, $defaultLatticeNorms];
      SetAutomatic[imageSize, Replace[latticeName, $defaultLatticeSizes]];
      If[maxNorm === None, maxNorm = 3, maxNorm * Length[trueCardinalList]]
  ]];

  If[MatchQ[maxVertices, AtLeast[_Integer]],
    maxVertices //= First;
    SetAutomatic[includeFrontier, False];
    SetAutomatic[depthTermination, "Complete"];
  ];

  SetAutomatic[depthTermination, "Immediate"];

  If[!MatchQ[maxDepth, (_Integer ? Positive) | Infinity],
    ReturnFailed[head::badlatticedepth, maxDepth];
  ];

  $stripLV = True;
  layoutDimension = Automatic;
  Switch[abstractCoordinateFunction,
    Automatic /; !AssociationQ[representation],
      {coordTypes, abstractCoordinateFunction, isRedundant} =
        FindCoordinatizationFunction[baseRep];
      If[graphLayout === Automatic && FreeQ[coordTypes, None],
        {is3D, proposedVertexCoordinateFunction} = chooseLatticeCoordinatization[coordTypes, isRedundant];
        SetAutomatic[vertexCoordinateFunction, proposedVertexCoordinateFunction];
      ];
      layoutDimension = If[is3D, 3, 2],
    Automatic /; AssociationQ[representation],
      abstractCoordinateFunction = Identity,
    None,
      abstractCoordinateFunction = Identity,
    _,
      abstractCoordinateFunction //= toACFunction;
      If[$stripLV, abstractCoordinateFunction = Normal /* abstractCoordinateFunction];
      lattice
  ];

  (* do the exploration *)
  {vertexList, indexEdgeList, reason} = MultiwaySystem[
    cayleyFunction, initialStates,
    {"VertexList", "IndexEdgeList", "TerminationReason"},
    DirectedEdges -> True, MaxDepth -> maxDepth, IncludeFrontier -> includeFrontier,
    DepthTermination -> depthTermination,
    MaxVertices -> maxVertices, MaxEdges -> (2 * maxEdges), SelfLoops -> selfLoops
  ];

  indexEdgeList = DeleteDuplicates @ indexEdgeList;
  If[IntegerQ[maxEdges] && Length[indexEdgeList] > maxEdges,
    indexEdgeList = Take[indexEdgeList, maxEdges];
    vertexPresentQ = ConstantAssociation[Catenate @ Map[Part[vertexList, {Part[#, 1], Part[#, 2]}]&, indexEdgeList], True];
    Scan[Set[vertexPresentQ[#], True]&, initialStates];
    vertexList = Pick[vertexList, Lookup[vertexPresentQ, vertexList, False]];
  ];

  If[randomSeeding =!= None && indexEdgeList =!= {},
    vertRange = Range @ Length[vertexList];
    BlockRandom[reordering = RandomSample[vertRange], RandomSeeding -> randomSeeding];
    vertexList //= PartOperator[reordering];
    reorderMap = AssociationThread[vertRange, reordering];
    indexEdgeList = MapAt[reorderMap, indexEdgeList, {All, 1 ;; 2}];
  ];

  (* rewrite the vertices via the coordinate function *)
  If[$stripLV && !MatchQ[First[vertexList, None], _LatticeVertex], $stripLV = False];
  abstractVertexList = MapAt[abstractCoordinateFunction, vertexList, If[$stripLV, {All, 1}, {All}]];
  ivertex = First[abstractVertexList, None];

  (* rewrite the indexed edges to use the explicit vertices *)
  edgeList = DeleteDuplicates[indexEdgeList] /.
    DirectedEdge[i_, j_, c___] :> DirectedEdge[Part[abstractVertexList, i], Part[abstractVertexList, j], c];
  If[abstractCoordinateFunction =!= Identity,
    edgeList //= DeleteDuplicates];

  If[ContainsQ[graphRegionHighlight, $cardinalBasedRegionPattern] || graphMetric =!= Automatic,
    If[directedEdges === False, AppendTo[simpleOptions, ArrowheadShape -> None]];
    directedEdges = True;
  ];

  If[directedEdges === False, edgeList = UndirectedEdge @@@ edgeList];

  (* the coordinitization might have collapsed some vertices *)
  abstractVertexList //= DeleteDuplicates;

  (* remove any vertices that exceed the norm *)
  norms = None;
  If[maxNorm =!= Infinity || abstractCoordinateFilter =!= None,
    If[maxNorm =!= Infinity,
      SetAutomatic[normFunction, ChessboardNorm];
      normFunction //= toNormFunction;
      normOffset = If[IntegerQ[maxNorm], 0, maxNorm - Ceiling[maxNorm //= Ceiling]];
      norms = Map[normFunction, FirstColumn[abstractVertexList] + normOffset];
      If[!VectorQ[norms, NumericQ],
        badIndex = SelectFirstIndex[norms, NumericQ /* Not];
        ReturnFailed[head::nonnumnorm, Part[norms, badIndex], InputForm @ Part[abstractVertexList, badIndex]]];
      vertexIndices = SelectIndices[norms, LessEqualThan[maxNorm]];
      If[vertexIndices === {},
        someNorms = AssociationThread @@ Take[{abstractVertexList, norms}, All, UpTo[4]];
        ReturnFailed[head::normempty, maxNorm, someNorms]];
      norms = Part[norms, vertexIndices];
    ,
      vertexIndices = Range @ Length @ abstractVertexList;
    ];
    If[abstractCoordinateFilter =!= None,
      vertexIndices = Intersection[vertexIndices, SelectIndices[abstractVertexList, First /* abstractCoordinateFilter]];
      If[vertexIndices === {},
        ReturnFailed[head::filterempty]];
      If[norms =!= None, norms = Part[norms, vertexIndices]];
    ];
    {abstractVertexList, edgeList} = VertexEdgeList @ Subgraph[
      Graph[abstractVertexList, edgeList],
      Part[abstractVertexList, vertexIndices]
    ];
    vertexList = Part[vertexList, vertexIndices];
  ];

  (* apply the final layout, if any *)
  SetAutomatic[vertexCoordinateFunction, None];
  If[vertexCoordinateFunction =!= None,
    If[AssociationQ[vertexCoordinateFunction], vertexCoordinateFunction //= procComplexVCF];
    If[$stripLV, vertexCoordinateFunction = First /* vertexCoordinateFunction];
    vertexCoordinates = Map[vertexCoordinateFunction, abstractVertexList];
    If[!MatrixQ[vertexCoordinates], ReturnFailed[head::badvcoords, First @ vertexCoordinates, First @ abstractVertexList]];
    layoutDimension = Switch[Length @ First @ vertexCoordinates, 2, 2, 3, 3, _, Automatic];
  ,
    vertexCoordinates = Automatic;
  ];

  (* apply the final vertex and edge relabeling *)
  If[AssociationQ[representation] && vertexNameFunction === "SpiralIndex", vertexNameFunction = "Index"];
  renamingRule = toRenamingRule[vertexNameFunction, abstractVertexList, vertexList];
  If[FailureQ[renamingRule], ReturnFailed[head::badvertnaming, vertexNameFunction, commaString @ $validRenamingRules]];
  {ivertex, finalVertexList} = {ivertex, abstractVertexList} /. renamingRule;
  If[edgeList =!= {}, edgeList = MapAt[Replace[renamingRule], edgeList, {All, 1;;2}]];

  If[PermutedRangeQ[finalVertexList],
    (* if we renamed to integers 1..n, reorder to make sure they occur in the natural order *)
    ordering = Ordering @ finalVertexList;
    finalVertexList = ToPacked @ Part[finalVertexList, ordering];
    vertexList = Part[vertexList, ordering];
    If[ListQ[norms], norms = Part[norms, ordering]];
    abstractVertexList = Part[abstractVertexList, ordering];
    If[ListQ[vertexCoordinates], vertexCoordinates = Part[vertexCoordinates, ordering]];
    edgeList //= Sort;
  ];

  vertexCoordinates = ToPackedReal @ vertexCoordinates;

  If[plotLabel === Automatic && StringQ[latticeName],
    simpleOptions //= ReplaceOptions[PlotLabel -> ToTitleString[latticeName]]];

  simpleOptions = DeleteOptions[simpleOptions, {VertexCoordinateFunction, Cardinals}];
  If[wasAutoCardinalList, trueCardinalList = Automatic];
  SetAutomatic[trueCardinalList, CardinalList @ edgeList];

  If[cardinals =!= Automatic,
    If[Length[cardinals] =!= Length[trueCardinalList],
      ReturnFailed[head::badcardlist, cardinals, trueCardinalList]];
    If[PathRepresentationObjectQ @ representation,
      representation = RenameCardinals[representation, cardinals];
      quiver = representation["Quiver"];
    ]];

  $quiverLabel := Quiver[quiver, ImageSize -> {50, 80},
    ArrowheadStyle -> arrowheadStyle, ArrowheadShape -> "Line",
    GraphLegend -> Placed[Automatic, Left]];

  $representation = representation;

  graphLegend //= makeQLatticeGraphLegend;

  baseOpts = deleteAutomaticOptions[
    ImageSize -> imageSize,
    GraphLayout -> graphLayout,
    VertexCoordinates -> vertexCoordinates
  ];

  If[head === LatticeGraph,
    graph = ExtendedGraph[
      finalVertexList, edgeList,
      GraphLegend -> graphLegend,
      GraphOrigin -> ivertex,
      Cardinals -> trueCardinalList,
      Sequence @@ simpleOptions,
      baseOpts,
      ArrowheadStyle -> None, BaselinePosition -> Center
    ];
    If[FailureQ[graph], ReturnFailed[head::badgconst]];
  ,
    imageSize //= toStandardImageSize;
    graph = ExtendedGraph[
      finalVertexList, edgeList,
      GraphLegend -> graphLegend,
      GraphOrigin -> ivertex, Cardinals -> trueCardinalList,
      Sequence @@ simpleOptions, BaselinePosition -> Center,
      baseOpts
    ];
    If[FailureQ[graph], ReturnFailed[head::renamenotquiv]];
  ];

  If[combineMultiedges, graph //= CombineMultiedges];
  If[cardinals =!= Automatic,
    graph = RenameCardinals[graph, cardinals]];

  isLV = MatchQ[First @ vertexList, LatticeVertex[_, _]];
  AttachVertexAnnotations[graph, <|
    If[isLV, "GeneratingVertex" -> vertexList[[All, 2]], {}],
    "AbstractCoordinates" -> If[isLV, abstractVertexList[[All, 1]], abstractVertexList],
    If[isLV && includeRepresentationMatrices, "RepresentationMatrix" -> vertexList[[All, 1]], {}],
    If[norms =!= None, "Norm" -> norms, {}]
  |>]
];

(* this exists to allow graph themes to take over, but i should fix
the problem in graph themes intead *)
deleteAutomaticOptions[l___, opt_ -> Automatic, r___] := deleteAutomaticOptions[l, r];
deleteAutomaticOptions[m___] := m;

makeQLatticeGraphLegend = Case[
  "Quiver" :=
    $quiverLabel;

  "Cardinals" :=
    "Cardinals";

  "PathRepresentation" :=
    Row[{Spacer[30], PathRepresentationPlot @ $representation}];

  Placed[spec_, place_] :=
    Placed[% @ spec, place];

  Labeled[spec_, rest__] :=
    Labeled[% @ spec, rest];

  Automatic :=
    Automatic;

  _ := None
];


$defaultLatticeNorms = {
  "TruncatedTrihexagonal" -> 5,
  name_String /; $LatticeData[name, "Dimension"] === 3 -> None,
  "Square" -> 2,
  _String -> 3,
  _ -> Infinity
};

$defaultLatticeSizes = {"Line" -> {200, 50}, "Square" -> {180, 180}, _ -> {200, 200}};

(**************************************************************************************************)

General::badparamlatticename = "The specified name `` is not a known name for a parameterized lattice. Known names are: ``."
General::badparamlatticeargs = "The provided arguments `` were not valid for parameterized lattice ``."
General::badparamlattiecount = "The parameterized lattice `` takes up to `` arguments, but `` were provided.";

iGenerateLattice[head_, {latticeName_String, args___}, directedEdges_, opts:OptionsPattern[]] /; !MatchQ[{args}, {__String}] && !MemberQ[$LatticeClassNames, latticeName] := Scope[

  paramLatticedata = $ParameterizedLatticeData[latticeName];
  If[MissingQ[paramLatticedata],
    ReturnFailed[head::badparamlatticename, latticeName, commaString @ $ParameterizedLatticeNames]];

  UnpackAssociation[paramLatticedata, factory, parameters];
  UnpackOptions[maxDepth];

  arguments = {args};
  options = {opts};

  argCount = Length @ arguments;
  If[argCount > Length[parameters] - 1,
    ReturnFailed[head::badparamlattiecount, latticeName, Length[parameters] - 1, argCount]];

  paramKeys = Keys @ parameters;
  arguments = Join[parameters, AssociationThread[Take[paramKeys, argCount], arguments]];
  If[maxDepth =!= Automatic, arguments["MaxDepth"] = maxDepth];

  result = factory[arguments, Association @ opts];
  If[GraphQ[result],
    If[head === LatticeGraph, result = ExtendedGraph[result, ArrowheadShape -> None]];
    Return @ result;
  ];

  If[!MatchQ[result, {_, {___Rule}}],
    ReturnFailed[head::badparamlatticeargs, KeyDrop["MaxDepth"] @ arguments, latticeName]];

  {representation, customOptions} = FirstRest @ result;
  customOptions //= Flatten;
  If[KeyExistsQ[customOptions, MaxDepth],
    options //= DeleteOptions[MaxDepth]];

  iGenerateLattice[head, representation, directedEdges, Sequence @@ options, Sequence @@ customOptions]
];

(**************************************************************************************************)

(**************************************************************************************************)

quiverStateToLatticeVertex[e_] := e /. PathValue[a_, b_] :> LatticeVertex[b, a];

(**************************************************************************************************)

procComplexVCF[assoc_] :=
  Apply @ Construct[Function,
    $Total @ KeyValueMap[{i, vec} |-> toVCFEntry[Slot[i], vec], assoc]
  ] /. $Total -> Total;

toVCFEntry[s_, vecs_List ? MatrixQ] := toVCFEntry[s, Association @ MapIndex1[#2 -> #1&, vecs]];
toVCFEntry[s_, vecs_Association] := Construct[Switch, s, Sequence @@ KeyValueMap[Splice[{#1, #2}]&, vecs], True, 0];
toVCFEntry[s_, vec_List] := s * vec;

$abc = Transpose @ N @ {{Sqrt[3]/2, -(1/2)}, {0, 1}, {-(Sqrt[3]/2), -(1/2)}};

vecAngle[{0., 0.}] := 0;
vecAngle[{a_, b_}] := ArcTan[a, b];

vecSorter[v_] /; Length[v] >= 4 := vecSorter @ Take[v, 3];
vecSorter[v_] /; Length[v] == 3 := {Norm[v], vecAngle @ Dot[$abc, v]};
vecSorter[v_] /; Length[v] == 2 := {Norm[v], vecAngle @ v};
vecSorter[v_] /; Length[v] == 1 := {Norm[v], v};

$validRenamingRules = {"SpiralIndex", "RasterIndex", "Representation", "RepresentationMatrix", "Coordinates", "Index"};

toRenamingRule[name_String -> n_Integer, vertices_, origVertices_] :=
  Map[PartOperator[n], toRenamingRule[name, vertices, origVertices]];

toRenamingRule[name_String -> f_, vertices_, origVertices_] :=
  Map[f, toRenamingRule[name, vertices, origVertices]];

toRenamingRule["SpiralIndex", vertices_, origVertices_] :=
  AssociationThread[vertices, Ordering @ Ordering @ Map[vecSorter, N @ vertices[[All, 1]]]];

toRenamingRule["RasterIndex", vertices_, origVertices_] :=
  AssociationThread[vertices, Ordering @ Ordering @ ({-#2, #1}& @@@ N[vertexCoordinates])];

toRenamingRule["Representation", vertices_, origVertices_] :=
  AssociationThread[vertices, origVertices[[All, 1]]];

toRenamingRule["RepresentationMatrix", vertices_, origVertices_] :=
  AssociationThread[vertices, origVertices[[All, 1, 1]]];

toRenamingRule["Coordinates" -> n_Integer, _, _] :=
  LatticeVertex[v_, _] :> Part[v, n];

toRenamingRule["Coordinates" -> f_, _, _] :=
  LatticeVertex[v_, _] :> f[n];

toRenamingRule["Coordinates", _, _] :=
  LatticeVertex[v_, _] :> v;

toRenamingRule["Index", vertices_, _] :=
  AssociationRange[vertices];

toRenamingRule[None, _, _] := {};

toRenamingRule[_, _, _] := $Failed;

(**************************************************************************************************)

toNormFunction = MatchValues[
  (i:({__Integer}|_Integer) -> f_) := PartOperator[i] /* f;
  list_List := ApplyThrough[% /@ list] /* Max;
  i_Integer := PartOperator[i];
  "Euclidean" := RootMeanSquare;
  f_ := f;
];

(**************************************************************************************************)

toACFunction = MatchValues[
  l_List    := Extract[l];
  {All, f_} := ($stripLV ^= False; f);
  f_        := f;
];

(**************************************************************************************************)

$LatticeQuiverAutocomplete = Join[$LatticeNames, $ParameterizedLatticeNames, $LatticeClassNames];

(**************************************************************************************************)

PublicFunction[LatticeGraph]

SetUsage @ "
LatticeGraph['name$'] generates part of the lattice graph for the named lattice 'name$'.
LatticeGraph[{'name$', p$1, $$}] generates a parameterized lattice graph with parameters p$i.
LatticeGraph[machine$] generates part of the lattice graph from a particular machine.
LatticeGraph[$$, depth$] generates a graph to a given depth.
* machine$ can be a group, groupoid, quiver representation, or an association with the \
keys 'CayleyFunction' and 'InitialStates'.
<*$namedLatticeUsage*>
<*$baseLatticeUsage*>
* To obtain cardinal tags on the edges, use the function %LatticeQuiver.
"

DeclareArgumentCount[LatticeGraph, {1, 2}];

Options[LatticeGraph] = JoinOptions[
  DirectedEdges -> False,
  $baseGenerateLatticeOptions
];

declareFunctionAutocomplete[LatticeGraph, {$LatticeQuiverAutocomplete, 0}];

declareSyntaxInfo[LatticeGraph, {_, _., OptionsPattern[]}];

LatticeGraph[spec_, opts:OptionsPattern[]] :=
  iGenerateLattice[LatticeGraph, spec, OptionValue @ DirectedEdges, FilterOptions @ opts];

(* this avoids an obscure bug that triggers when you do ?LatticeGraph, that I should report *)
SetAttributes[LatticeGraph, ReadProtected];

LatticeGraph[spec_, depth_, opts:OptionsPattern[]] :=
  LatticeGraph[spec, opts, MaxDepth -> depth];

(**************************************************************************************************)

PublicFunction[LatticeQuiver]

SetUsage @ "
LatticeQuiver['name$'] generates part of the lattice graph for the named lattice 'name$'.
LatticeQuiver[{'name$', p$1, $$}] generates a parameterized lattice quiver with parameters p$i.
LatticeQuiver[machine$] generates part of the lattice graph from a particular machine.
LatticeQuiver[$$, depth$] generates a graph to a given depth.
* machine$ can be a group, groupoid, quiver representation, or an association with the \
keys 'CayleyFunction' and 'InitialStates'.
* The returned graph is an undirected graph, unless %DirectedEdges -> True is specified.
* LatticeQuiver behaves like %LatticeGraph, but returns a quiver instead of an ordinary graph.
<*$namedLatticeUsage*>
<*$baseLatticeUsage*>
"

DeclareArgumentCount[LatticeQuiver, {1, 2}];

Options[LatticeQuiver] = $baseGenerateLatticeOptions;

declareFunctionAutocomplete[LatticeQuiver, {$LatticeQuiverAutocomplete, 0}];

declareSyntaxInfo[LatticeQuiver, {_, _., OptionsPattern[]}];

LatticeQuiver[spec_, opts:OptionsPattern[]] :=
  iGenerateLattice[LatticeQuiver, spec, True, FilterOptions @ opts];

LatticeQuiver[spec_, depth_, opts:OptionsPattern[]] :=
  LatticeQuiver[spec, opts, MaxDepth -> depth];


(**************************************************************************************************)

PublicFunction[LatticeQuiverData]

SetUsage @ "
LatticeQuiverData['name$'] gives information about the quiver lattice given by 'name$'.
LatticeQuiverData['name$', 'prop$'] gives the specific property 'prop$'.
LatticeQuiverData['class$'] returns a list of names that fall into a particular class.
LatticeQuiverData[] returns a list of the names of all known lattices.
* The following properties are present:
| 'Names' | the full list of names of the lattice |
| 'Representation' | the PathRepresentation[$$] object that generates the lattice |
| 'Dimension' | the dimension of its natural coordinitization |
* The following classes are supported:
| '2D' | lattices whose dimension is 2 |
| '3D' | lattices whose dimension is 3 |
* Custom quiver lattices can be defined using DefineLatticeQuiver.
"

DeclareArgumentCount[LatticeQuiverData, {1, 2}];

$latticeQuiverProperties = {
  "Names", "Representation", "Dimension"
};

declareFunctionAutocomplete[LatticeQuiverData, {$LatticeQuiverAutocomplete, $latticeQuiverProperties}];

pickNamesWithPropertyEqualTo[prop_, value_] :=
  KeyValueMap[If[Lookup[#2, prop] === value, #1, Nothing]&, $LatticeData];

LatticeQuiverData["2D"] := pickNamesWithPropertyEqualTo["Dimension", 2];
LatticeQuiverData["3D"] := pickNamesWithPropertyEqualTo["Dimension", 3];
LatticeQuiverData[name_String] := Lookup[$LatticeData, $latticeNameAliases @ name, None];
LatticeQuiverData[name_String, prop_String] := Part[$LatticeData, Key @ $latticeNameAliases @ name, prop];
LatticeQuiverData[] := $LatticeNames;

LatticeQuiverData[{name_String, args___}, "Representation"] := Scope[
  Last @ $ParameterizedLatticeData[name][args]
];

(**************************************************************************************************)

PublicFunction[RationalAngle]

RationalAngle[{0|0., 0|0.}] := Indeterminate;
RationalAngle[{a_, b_}] := N[ArcTan[a, b]] / Tau;
