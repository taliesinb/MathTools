Package["GraphTools`"]


PackageImport["GeneralUtilities`"]


PackageExport["AbstractCoordinateFunction"]
PackageExport["VertexCoordinateFunction"]
PackageExport["VertexNameFunction"]
PackageExport["IncludeRepresentationMatrices"]

SetUsage @ "AbstractCoordinateFunction is an option to QuiverLattice and QuiverGraph."
SetUsage @ "VertexCoordinateFunction is an option to QuiverLattice and QuiverGraph."
SetUsage @ "VertexNameFunction is an option to QuiverLattice and QuiverGraph."

$baseLatticeUsage = "
* The following options are supported:
| AbstractCoordinateFunction | Automatic | function to obtain abstract vertex coordinates from representation matrices |
| VertexCoordinateFunction | Automatic | function to obtain graphical vertex coordinates from abstract coordinates |
| VertexNameFunction | 'SpiralIndex' | function to rename vertices after abstract coordinatization |
| GraphLayout | None | the overall layout method to use for vertices and edges |
| LayoutDimension | Automatic | number of dimensions of the graph layout |
| VertexLabels | None | whether to plot vertices with their labels |
| ImageSize | Automatic | size to plot the graph |
| MaxVertices | Infinity | maximum number of lattice vertices to obtain |
| MaxEdges | Infinity | maximum number of lattice edges to obtain |
| MaxNorm | Infinity | drop vertices with larger norm |
| NormFunction | Automatic | function to compute norm from abstract vertex coordinates |
| GraphRegionHighlight | None | regions of the graph to highlight |
| GraphLegend | Automatic | legend to attach to the entire graph |
| IncludeRepresentationMatrices | False | whether to attach the original representations as vertex annotations |

* AbstractCoordinateFunction extracts abstract vertex coordinates from RepresentationElements, and accepts these settings:
| Automatic | pick coordinates based on the structure of the group (default) |
| None | use the entire RepresentationElement as the coordinate |
| f$ | apply f$ to the contents of each RepresentationElement (a matrix) |

* VertexCoordinateFunction determines the graphical coordinates, and accepts the following settings:
| Automatic | convert representation coords to spatial coords based on the structure of the group (default) |
| None | use Graph's automatic layout of the vertices |
| f$ | apply f$ to the representation coordinates produced by AbstractCoordinateFunction |

* VertexNameFunction determines the final names given to vertices, and accepts these settings:
| 'SpiralIndex' | number vertices starting at 1 for the origin, proceeding clockwise then outward (default) |
| 'RasterIndex' | number vertices starting at the top left, proceeding right then down |
| 'Coordinates' | use abstract coordinates directly as names |
| 'Representation' | use the original RepresentationElement[$$] objects directly as names |
| None | use LatticeVertex[abstract$, type$] as names |

* GraphLayout accepts these settings:
| None | use the layout provided by VertexCoordinateFunction |
| Automatic | use 'SpringElectricalEmbedding' |
| '2D' | use a two-dimensional automatic layout |
| '3D' | use a three-dimensional automatic layout |
| spec$ | use a custom specification accepted by Graph |

* VertexLabels determines how to label vertices, and accepts these settings:
| None | do not label vertices |
| Automatic | label vertices with their names |
| 'VertexCoordinates' | label vertices with their abstract vertex coordinates |
| Tooltip | label vertices with their names via a tooltip |

* EdgeLabels determines how to label edges, and accepts these settings:
| None | do not label edges |
| Automatic | label edges with their cardinals |

* GraphLegend accepts these settings:
| None | no legend |
| Automatic | use a legend indicating the cardinals |
| expr$ | use a custom legend given by expr$ |
"

PackageExport["InitialStates"]

$baseGenerateLatticeOptions = JoinOptions[{
  AbstractCoordinateFunction -> Automatic,
  VertexCoordinateFunction -> Automatic,
  VertexNameFunction -> "SpiralIndex",
  GraphLegend -> Automatic,
  MaxNorm -> Infinity,
  NormFunction -> Automatic,
  CardinalColors -> Automatic,
  MaxVertices -> Infinity, MaxEdges -> Infinity,
  DepthTermination -> Automatic, IncludeFrontier -> Automatic,
  IncludeRepresentationMatrices -> False,
  CombineMultiedges -> True,
  SelfLoops -> True,
  InitialStates -> Automatic
  },
  $simpleGraphOptionRules
];

General::notquivrep = "First argument should be a QuiverRepresentationObject, or a quiver with canonically named cardinals.";
General::badvcoords = "VertexCoordinateFunction did not return vectors of consistent dimensions.";
General::badlatticename = "The specified name `` is not a known name for a lattice. Known names are: ``."
General::badlatticedepth = "The specified depth `` is not a positive integer."
General::badvertnaming = "Unknown setting `` for VertexNameFunction. Valid renaming rules are ``.";
General::renamenotquiv = "The vertex coordinates yielded a quiver with incompatible cardinal edges. Use LatticeGraph instead.";
General::normempty = "The setting of NormFunction and MaxNorm -> `` yielded an empty graph. The first few norms were: ``."
General::nonnumnorm = "The setting of NormFunction yielded the non-numeric value `` on vertex ``.";

Options[iGenerateLattice] = $baseGenerateLatticeOptions;

$cardinalBasedRegionPattern = Path | Line[_, _] | HalfLine | InfiniteLine | Axes;

iGenerateLattice[head_, representation_, maxDepth_, directedEdges_, opts:OptionsPattern[]] := Scope[

  depth = maxDepth;

  If[StringQ[representation],
    latticeName = representation;

    If[MemberQ[$LatticeClassNames, latticeName],
      Return @ Map[
        iGenerateLattice[head, #, maxDepth, directedEdges, opts, PlotLabel -> Automatic]&,
        LatticeQuiverData @ latticeName
      ]];

    If[KeyExistsQ[$ParameterizedLatticeData, latticeName],
      Return @ iGenerateLattice[head, {latticeName}, maxDepth, directedEdges, opts]];

    representation = LatticeQuiverData[latticeName, "Representation"];
    If[!QuiverRepresentationObjectQ[representation],
      ReturnFailed[head::badlatticename, latticeName, commaString @ $LatticeNames]];
  ];

  If[RuleQ[representation],
    representation = QuiverRepresentation @@ representation;
    If[FailureQ[representation], ReturnFailed[]];
  ];

  If[QuiverQ[representation],
    representation = Quiet @ QuiverRepresentation[representation]];

  If[GroupoidObjectQ[representation],
    representation = AssociationMap[representation, {"CayleyFunction", "InitialStates"}];
  ];

  If[AssociationQ[representation] && Sort[Keys @ representation] === {"CayleyFunction", "InitialStates"},

    UnpackAssociation[representation, cayleyFunction, initialStates];
    repInitialStates = Developer`ToList[initialStates];
    cardinalList = Union[StripNegated /@ DeepCases[cayleyFunction, Labeled[_, c_] :> c]];
    If[cardinalList === {}, cardinalList = Automatic];
  ,
    If[!QuiverRepresentationObjectQ[representation],
      ReturnFailed[head::notquivrep]];

    cayleyFunction = quiverStateToLatticeVertex @ representation["CayleyFunction", "Symmetric" -> True, "Labeled" -> True];
    baseRep = representation["Representation"];
    repInitialStates = List @ quiverStateToLatticeVertex @ representation["Identity"];

    quiver = representation["Quiver"];
    cardinalList = CardinalList @ quiver;
  ];

  UnpackOptionsAs[head, opts,
    maxNorm, normFunction,
    abstractCoordinateFunction, vertexCoordinateFunction,
    graphLayout,
    graphLegend, imageSize, vertexNameFunction, arrowheadStyle,
    maxVertices, maxEdges, depthTermination, includeFrontier,
    graphMetric, combineMultiedges,
    includeRepresentationMatrices,
    graphRegionHighlight, plotLabel,
    selfLoops, initialStates
  ];

  SetAutomatic[initialStates, repInitialStates];

  simpleOptions = TakeOptions[{opts}, $simpleGraphOptions];

  SetAutomatic[depth, Which[
    maxNorm =!= Infinity, maxNorm * Length[cardinalList],
    maxVertices =!= Infinity, Infinity,
    AssociationQ[representation],
      Infinity,
    True,
      maxNorm = If[MemberQ[$sparseLatticeNames, latticeName], 5, 3];
      maxNorm * Length[cardinalList]
  ]];

  If[MatchQ[maxVertices, AtLeast[_Integer]],
    maxVertices //= First;
    SetAutomatic[includeFrontier, False];
    SetAutomatic[depthTermination, "Complete"];
  ];

  SetAutomatic[depthTermination, "Immediate"];

  If[!MatchQ[depth, (_Integer ? Positive) | Infinity],
    ReturnFailed[head::badlatticedepth, depth];
  ];

  $quiverLabel := Quiver[quiver, ImageSize -> 50,
    ArrowheadStyle -> arrowheadStyle,
    GraphLegend -> Placed[Automatic, Left]];

  Switch[graphLegend,
    "Quiver",
      graphLegend = Placed[$quiverLabel, Right],
    "QuiverRepresentation",
      generators = representation["Generators"];
      labeledGenerators = makeLabeledGenerators[generators, ChooseCardinalColors @ Keys @ generators];
      graphLegend = Placed[Row[{$quiverLabel, "  ", labeledGenerators}], Right],
    _,
      None
  ];

  layoutDimension = Automatic; vertexLayout = Automatic;
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
      abstractCoordinateFunction = Normal /* abstractCoordinateFunction;
      lattice
  ];

  (* do the exploration *)
  {vertexList, indexEdgeList, reason} = StateTransitionGraph[cayleyFunction, initialStates,
    {"VertexList", "IndexEdgeList", "TerminationReason"},
    DirectedEdges -> True,
    MaxDepth -> depth,
    IncludeFrontier -> includeFrontier, DepthTermination -> depthTermination,
    MaxVertices -> maxVertices, MaxEdges -> maxEdges, SelfLoops -> selfLoops
  ];

  (* rewrite the vertices via the coordinate function *)
  abstractVertexList = MapAt[abstractCoordinateFunction, vertexList, {All, 1}];
  ivertex = First @ abstractVertexList;

  (* rewrite the indexed edges to use the explicit vertices *)
  edgeList = DeleteDuplicates[indexEdgeList] /.
    DirectedEdge[i_, j_, c_] :> DirectedEdge[Part[abstractVertexList, i], Part[abstractVertexList, j], c];
  If[abstractCoordinateFunction =!= Identity,
    edgeList //= DeleteDuplicates];

  If[ContainsQ[graphRegionHighlight, $cardinalBasedRegionPattern],
    If[directedEdges === False, AppendTo[simpleOptions, ArrowheadShape -> None]];
    directedEdges = True;
  ];

  If[directedEdges === False, edgeList = UndirectedEdge @@@ edgeList];

  (* the coordinitization might have collapsed some vertices *)
  abstractVertexList //= DeleteDuplicates;

  (* remove any vertices that exceed the norm *)
  If[maxNorm =!= Infinity,
    SetAutomatic[normFunction, ChessboardNorm];
    normFunction //= toNormFunction;
    norms = Map[First /* normFunction, abstractVertexList];
    If[!VectorQ[norms, NumericQ],
      badIndex = SelectFirstIndex[norms, NumericQ /* Not];
      ReturnFailed[head::nonnumnorm, Part[norms, badIndex], InputForm @ Part[abstractVertexList, badIndex]]];
    vertexIndices = SelectIndices[norms, LessEqualThan[maxNorm]];
    If[vertexIndices === {},
      someNorms = AssociationThread @@ Take[{abstractVertexList, norms}, All, UpTo[4]];
      ReturnFailed[head::normempty, maxNorm, someNorms]];
    {abstractVertexList, edgeList} = VertexEdgeList @ Subgraph[
      Graph[abstractVertexList, edgeList],
      Part[abstractVertexList, vertexIndices]
    ];
    vertexList = Part[vertexList, vertexIndices];
    norms = Part[norms, vertexIndices];
  ,
    norms = None;
  ];

  (* apply the final layout, if any *)
  SetAutomatic[vertexCoordinateFunction, None];
  If[vertexCoordinateFunction =!= None,
    If[AssociationQ[vertexCoordinateFunction], vertexCoordinateFunction //= procComplexVCF];
    vertexCoordinates = Map[First /* vertexCoordinateFunction, abstractVertexList];
    If[!MatrixQ[vertexCoordinates], ReturnFailed[head::badvcoords]];
    layoutDimension = Switch[Length @ First @ vertexCoordinates, 2, 2, 3, 3, _, Automatic];
  ,
    vertexCoordinates = Automatic;
    vertexLayout = "SpringElectricalEmbedding";
  ];

  (* apply the final vertex and edge relabeling *)
  If[AssociationQ[representation] && vertexNameFunction === "SpiralIndex", vertexNameFunction = "Index"];
  renamingRule = toRenamingRule[vertexNameFunction, abstractVertexList, vertexList];
  If[FailureQ[renamingRule], ReturnFailed[head::badvertnaming, vertexNameFunction, commaString @ $validRenamingRules]];
  {ivertex, finalVertexList, edgeList} = {ivertex, abstractVertexList, edgeList} /. renamingRule;
  If[RangeQ[finalVertexList],
    (* if we renamed to integers 1..n, reorder to make sure they occur in the natural order *)
    ordering = Ordering @ finalVertexList;
    finalVertexList = ToPacked @ Part[finalVertexList, ordering];
    vertexList = Part[vertexList, ordering];
    abstractVertexList = Part[abstractVertexList, ordering];
    If[ListQ[vertexCoordinates], vertexCoordinates = Part[vertexCoordinates, ordering]];
    edgeList //= Sort;
  ];

  vertexCoordinates = ToPackedReal @ vertexCoordinates;

  If[plotLabel === Automatic && StringQ[latticeName],
    simpleOptions //= ReplaceOptions[PlotLabel -> toTitleString[latticeName]]];

  SetAutomatic[cardinalList, CardinalList @ edgeList];
  If[head === LatticeGraph,
    graph = ExtendedGraph[
      finalVertexList, edgeList,
      GraphLegend -> graphLegend,
      ImageSize -> imageSize,
      GraphLayout -> graphLayout, VertexCoordinates -> vertexCoordinates,
      GraphOrigin -> ivertex, Cardinals -> cardinalList,
      Sequence @@ simpleOptions,
      ArrowheadStyle -> None
    ]
  ,
    imageSize //= toStandardImageSize;
    graph = Quiver[finalVertexList, edgeList,
      GraphLayout -> graphLayout, VertexCoordinates -> vertexCoordinates,
      GraphLegend -> graphLegend, ImageSize -> imageSize,
      GraphOrigin -> ivertex, Cardinals -> cardinalList,
      Sequence @@ simpleOptions
    ];
    If[FailureQ[graph], ReturnFailed[head::renamenotquiv]];
  ];

  If[combineMultiedges, graph //= CombineMultiedges];

  AttachVertexAnnotations[graph, <|
    If[Length[First @ vertexList] == 2, "GeneratingVertex" -> vertexList[[All, 2]], {}],
    "AbstractCoordinates" -> abstractVertexList[[All, 1]],
    If[includeRepresentationMatrices, "RepresentationMatrix" -> vertexList[[All, 1]], {}],
    If[norms =!= None, "Norm" -> norms, {}]
  |>]
];

$sparseLatticeNames = {"TruncatedTrihexagonal"};

toTitleString[s_String] :=
  ToLowerCase @ StringReplace[s, RegularExpression["([a-z])([A-Z])"] :> "$1 $2"];

(**************************************************************************************************)

General::badparamlatticename = "The specified name `` is not a known name for a parameterized lattice. Known names are: ``."
General::badparamlatticeargs = "The provided arguments `` were not valid for parameterized lattice ``."
General::badparamlattiecount = "The parameterized lattice `` takes up to `` arguments, but `` were provided.";

iGenerateLattice[head_, {latticeName_String, args__}, maxDepth_, directedEdges_, opts:OptionsPattern[]] := Scope[

  paramLatticedata = $ParameterizedLatticeData[latticeName];
  If[MissingQ[paramLatticedata],
    ReturnFailed[head::badparamlatticename, latticeName, commaString @ $ParameterizedLatticeNames]];

  UnpackAssociation[paramLatticedata, factory, parameters];

  arguments = {args};

  argCount = Length @ arguments;
  If[argCount > Length[parameters] - 1,
    ReturnFailed[head::badparamlattiecount, latticeName, Length[parameters] - 1, argCount]];

  paramKeys = Keys @ parameters;
  arguments = Join[parameters, AssociationThread[Take[paramKeys, argCount], arguments]];
  If[maxDepth =!= Automatic, arguments["MaxDepth"] = maxDepth];

  result = factory[arguments];
  If[!ListQ[result], ReturnFailed[head::badparamlatticeargs, KeyDrop["MaxDepth"] @ arguments, latticeName]];

  {representation, customOptions} = FirstRest @ result;
  customOptions //= Flatten;
  newMaxDepth = Lookup[customOptions, MaxDepth];
  If[!MissingQ[newMaxDepth], maxDepth = newMaxDepth];
  customOptions = DeleteOptions[customOptions, MaxDepth];

  iGenerateLattice[head, representation, maxDepth, directedEdges, opts, Sequence @@ customOptions]
];

(**************************************************************************************************)

(**************************************************************************************************)

quiverStateToLatticeVertex[e_] := e /. QuiverElement[a_, b_] :> LatticeVertex[b, a];

(**************************************************************************************************)

procComplexVCF[assoc_] :=
  Apply @ Construct[Function,
    Total @ KeyValueMap[{i, vec} |-> Slot[i] * vec, assoc]
  ];

$abc = Transpose @ N @ {{Sqrt[3]/2, -(1/2)}, {0, 1}, {-(Sqrt[3]/2), -(1/2)}};

vecAngle[{0., 0.}] := 0;
vecAngle[{a_, b_}] := ArcTan[a, b];

vecSorter[v_] := Norm[v];
vecSorter[v_] /; Length[v] == 3 := {Norm[v], vecAngle @ Dot[$abc, v]};
vecSorter[v_] /; Length[v] == 2 := {Norm[v], vecAngle @ v};
vecSorter[v_] /; Length[v] == 1 := {Norm[v], v};

$validRenamingRules = {"SpiralIndex", "RasterIndex", "Representation", "Coordinates", "Index"};

toRenamingRule["SpiralIndex", vertices_, origVertices_] :=
  AssociationThread[vertices, Ordering @ Ordering @ Map[vecSorter, N @ vertices[[All, 1]]]];

toRenamingRule["RasterIndex", vertices_, origVertices_] :=
  AssociationThread[vertices, Ordering @ Ordering @ ({-#2, #1}& @@@ N[vertexCoordinates])];

toRenamingRule["Representation", _, _] :=
  AssociationThread[vertices, origVertices[[All, 1]]];

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
  f_ := f;
];

(**************************************************************************************************)

toACFunction = MatchValues[
  l_List := Extract[l];
  f_ := f;
];

(**************************************************************************************************)

$LatticeQuiverAutocomplete = Join[$LatticeNames, $ParameterizedLatticeNames, $LatticeClassNames];

(**************************************************************************************************)

PackageExport["LatticeGraph"]

SetUsage @ "
LatticeGraph[qrep$, d$] generates the lattice graph for a quiver representation qrep$ to depth d$.
LatticeGraph['name$', d$] generates the lattice graph for the named lattice 'name$' to depth d$.
LatticeGraph[spec$] uses a default depth of 6.
<*$namedLatticeUsage*>
* To obtain cardinal tags on the edges, use the function LatticeQuiver.
"

DeclareArgumentCount[LatticeGraph, {1, 2}];

Options[LatticeGraph] = JoinOptions[
  {DirectedEdges -> False},
  $baseGenerateLatticeOptions
];

declareFunctionAutocomplete[LatticeGraph, {$LatticeQuiverAutocomplete, 0}];

declareSyntaxInfo[LatticeGraph, {_, _., OptionsPattern[]}];

LatticeGraph[name_, opts:OptionsPattern[]] :=
  LatticeGraph[name, Automatic, opts];

(* this avoids an obscure bug that triggers when you do ?LatticeGraph, that I should report *)
SetAttributes[LatticeGraph, ReadProtected];

LatticeGraph[spec_, depth_, opts:OptionsPattern[]] := Scope[
  UnpackOptions[directedEdges];
  iGenerateLattice[LatticeGraph, spec, depth, directedEdges, FilterOptions @ opts]
];

(**************************************************************************************************)

PackageExport["LatticeQuiver"]

SetUsage @ "
LatticeQuiver[qrep$, d$] generates the lattice quivr graph for a quiver representation qrep$ to depth d$.
LatticeQuiver['name$', d$] generates the lattice quiver graph for the named lattice 'name$' to depth d$.
LatticeQuiver[spec$] uses a default depth of 6.
<*$namedLatticeUsage*>
* LatticeQuiver behaves like LatticeGraph, but returns a quiver instead of an ordinary graph.
"

DeclareArgumentCount[LatticeQuiver, {1, 2}];

Options[LatticeQuiver] = $baseGenerateLatticeOptions;

declareFunctionAutocomplete[LatticeQuiver, {$LatticeQuiverAutocomplete, 0}];

declareSyntaxInfo[LatticeQuiver, {_, _., OptionsPattern[]}];

LatticeQuiver[name_, opts:OptionsPattern[]] :=
  LatticeQuiver[name, Automatic, opts];

LatticeQuiver[spec_, depth_, opts:OptionsPattern[]] :=
  iGenerateLattice[LatticeQuiver, spec, depth, True, opts];


(**************************************************************************************************)

PackageExport["LatticeQuiverData"]

SetUsage @ "
LatticeQuiverData['name$'] gives information about the quiver lattice given by 'name$'.
LatticeQuiverData['name$', 'prop$'] gives the specific property 'prop$'.
LatticeQuiverData['class$'] returns a list of names that fall into a particular class.
LatticeQuiverData[] returns a list of the names of all known lattices.
* The following properties are present:
| 'Names' | the full list of names of the lattice |
| 'Representation' | the QuiverRepresentation[$$] object that generates the lattice |
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
