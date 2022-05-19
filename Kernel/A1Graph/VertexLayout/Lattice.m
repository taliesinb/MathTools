PackageExport["LatticeLayout"]
PackageExport["BasisVectors"]
PackageExport["ScaleFactor"]

Options[LatticeLayout] = {BasisVectors -> Automatic, ScaleFactor -> 1};

LatticeLayout[opts:OptionsPattern[]][data_] := Scope[
  
  UnpackOptions[basisVectors, scaleFactor];

  UnpackAssociation[data, indexGraph, graph];

  {vertexCoordinates, visitedEdges} = LatticeQuiverCoordinates[graph, basisVectors];

  vertexCoordinates *= scaleFactor;

  edgePairs = EdgePairs @ indexGraph;

  wasVisited = ConstantArray[True, EdgeCount @ indexGraph];
  Part[wasVisited, visitedEdges] = True;

  edgeCoordinateLists = MapThread[makeLatticeEdge, {edgePairs, wasVisited}];

  {vertexCoordinates, edgeCoordinateLists}
]

makeLatticeEdge[pair_, True] := Part[vertexCoordinates, pair];
makeLatticeEdge[pair_, False] := Scope[
  {a, b} = Part[vertexCoordinates, pair];
  d = 0.25 * Normalize[b - a];
  e = rot90 @ d;
  ae = a + e;
  be = b + e;
  corn = 4;
  DiscretizeCurve[{a, ae - e/corn, ae, ae + d/corn, be - d/corn, be, be + e/corn, b}]
];

rot90[{x_, y_}] := {y, -x};
rot270[{x_, y_}] := {-y, x};