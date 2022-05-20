PackageScope["$LatticeData"]
PackageScope["$LatticeNames"]
PackageScope["$LatticeClassNames"]
PackageScope["$latticeNameAliases"]

$LatticeData = <||>
$LatticeNames = {};
$LatticeClassNames = {"2D", "3D"};

PackageExport["DefineLatticeQuiver"]

SetUsage @ "
DefineLatticeQuiver[fields$] defines a new lattice described by fields$ and add its to \
the internal table consulted by LatticeQuiverData.
* The following fields are required:
| 'Names' -> {'name$1', 'name$2', $$} | a single name, and possible aliases, for the lattice |
| 'Quiver' :> code$ | code to produce the fundamental quiver |
| 'Group' :> code$ | code to produce the group (or representation of the group) |
| 'Dimension' -> n$ | the dimension of the lattice vectors created by the lattice |
* The code in 'Representation' :> code$ will evaluated on demand and cached internally.
* The name 'name$1' will be used as the canonical name of the lattice, with the others \
acting merely as aliases.
"

$requiredLatticeQuiverFields = {
  "Names", "Group", "Quiver", "Dimension"
};

DefineLatticeQuiver::missingfields = "Required fields are ``."
DefineLatticeQuiver::badfield = "Error defining ``: Value for field \"``\" is not a valid ``."
DefineLatticeQuiver::badrep = "Error defining ``: could not build representation.";

DefineLatticeQuiver[rawFields__] := Scope[

  fields = Association[rawFields];
  If[!AssociationQ[fields] || Apply[And, KeyExistsQ[fields, $requiredLatticeQuiverFields]],
    ReturnFailed["missingfields", commaString @ $requiredLatticeQuiverFields]];

  {canonicalName, aliases} = FirstRest @ Lookup[fields, "Names"];

  UnpackAssociation[fields, quiver, group];
  If[!QuiverQ[quiver], ReturnFailed["badfield", canonicalName, "Quiver", "quiver"]];
  If[!GroupQ[group], ReturnFailed["badfield", canonicalName, "Group", "group"]];

  representation = PathRepresentation[quiver, group];
  If[!PathRepresentationObjectQ[representation],
    ReturnFailed["badrep", canonicalName]];

  fields["Representation"] = representation;
  AppendTo[$LatticeNames, canonicalName];
  Do[$latticeNameAliases[alias] ^= canonicalName, {alias, aliases}];

  AssociateTo[$LatticeData, canonicalName -> fields];
];

$latticeNameAliases[name_] := name;

(**************************************************************************************************)

DefineLatticeQuiver[
  "Names" -> {"Line"},
  "Quiver" -> BouquetQuiver @ "x",
  "Group" -> InfiniteAbelianGroup[1],
  "Dimension" -> 1
];

(**************************************************************************************************)

DefineLatticeQuiver[
  "Names" -> {"Square", "Quadrille"},
  "Quiver" -> BouquetQuiver @ "xy",
  "Group" -> InfiniteAbelianGroup[2],
  "Dimension" -> 2
];

(**************************************************************************************************)

DefineLatticeQuiver[
  "Names" -> {"Triangular", "Deltille"},
  "Quiver" -> BouquetQuiver @ "abc",
  "Group" -> InfiniteAbelianGroup[3, "Redundant"],
  "Dimension" -> 2
];

(**************************************************************************************************)

DefineLatticeQuiver[
  "Names" -> {"Hexagonal", "Hextille"},
  "Quiver" -> Quiver[Labeled[1 -> 2, "a" | "b" | "c"], VertexCoordinates -> {{0, 0}, {1, 0}}],
  "Group" -> InfiniteAbelianGroup[3, "Redundant"],
  "Dimension" -> 2
];

(**************************************************************************************************)

DefineLatticeQuiver[
  "Names" -> {"HexagonalDoubleCover"},
  "Quiver" -> BouquetQuiver @ "abc",
  "Group" -> InfiniteDihedralGroup[3, "Redundant"],
  "Dimension" -> 2
];

(**************************************************************************************************)

DefineLatticeQuiver[
  "Names" -> {"Rhombille"},
  "Quiver" -> Quiver[Labeled[{1 -> 2, 2 -> 3}, "a" | "b" | "c"], MultiEdgeDistance -> 0.3, VertexCoordinates -> {{0, 0}, {1, 0}, {2, 0}}],
  "Group" -> InfiniteAbelianGroup[3, "Redundant"],
  "Dimension" -> 2
];

(**************************************************************************************************)

$sq3 = Sqrt[3];
$isq3 = 1/$sq3;
$DedecagonTranslationGroup = TranslationGroup[
  {{0, 2}, {1, $sq3}, {$sq3, 1}, {2, 0}, {$sq3, -1}, {1, -$sq3}} / 2
];

DefineLatticeQuiver[
  "Names" -> {"Rhombitrihexagonal", "Rhombihexadeltille"},
  "Quiver" -> Quiver @ <|
    1 -> {6 -> 4, 1 -> 3},
    2 -> {3 -> 2, 5 -> 6},
    3 -> {1 -> 5, 2 -> 4},
    4 -> {6 -> 1, 4 -> 3},
    5 -> {2 -> 6, 3 -> 5},
    6 -> {1 -> 2, 5 -> 4}
  |>,
  "Group" -> $DedecagonTranslationGroup,
  "Dimension" -> 2
];

(**************************************************************************************************)

DefineLatticeQuiver[
  "Names" -> {"Trihexagonal", "Hexadeltille"},
  "Quiver" -> Quiver[<|
    "a" -> {1 -> 3, 3 -> 1},
    "b" -> {1 -> 2, 2 -> 1},
    "c" -> {2 -> 3, 3 -> 2}
  |>, MultiEdgeDistance -> .12, VertexCoordinates -> CirclePoints[3]],
  "Group" -> InfiniteAbelianGroup[3, "Redundant"],
  "Dimension" -> 2
];

(**************************************************************************************************)

DefineLatticeQuiver[
  "Names" -> {"SnubTrihexagonal", "SnubHextille"},
  "Quiver" -> Quiver @ <|
    "a" -> {1 -> 6, 3 -> 4, 4 -> 1, 6 -> 2, 5 -> 3},
    "b" -> {1 -> 2, 5 -> 4, 2 -> 5, 3 -> 1, 4 -> 6},
    "c" -> {3 -> 2, 5 -> 6, 1 -> 5, 2 -> 4, 6 -> 3}
  |>,
  "Group" -> InfiniteAbelianGroup[3, "Redundant"],
  "Dimension" -> 2
];

(**************************************************************************************************)

DefineLatticeQuiver[
  "Names" -> {"TruncatedSquare", "TruncatedQuadrille"},
  "Quiver" -> Quiver[<|
    "a" -> {1 -> 2}, "b" -> {4 -> 3},
    "c" -> {3 -> 1, 2 -> 4},
    "d" -> {1 -> 4, 3 -> 2}
  |>, GraphLayout -> "Tutte"],
  "Group" -> TranslationGroup[{{1, 0}, {0, 1}, {1, 1}/2, {-1, 1}/2}],
  "Dimension" -> 2
];

(**************************************************************************************************)

DefineLatticeQuiver[
  "Names" -> {"TruncatedTrihexagonal", "TruncatedHexadeltille"},
  "Quiver" -> Quiver[<|
    "a" -> {3 -> 2, 8 -> 9, 12 -> 5, 11 -> 6},
    "b" -> {4 -> 3, 9 -> 10},
    "c" -> {5 -> 4, 10 -> 11, 1 -> 8, 2 -> 7},
    "d" -> {6 -> 5, 11 -> 12},
    "e" -> {7 -> 6, 12 -> 1, 3 -> 10, 4 -> 9},
    "f" -> {1 -> 2, 8 -> 7}
  |>, VertexCoordinates -> CirclePoints[12]],
  "Group" -> $DedecagonTranslationGroup,
  "Dimension" -> 2
];

(**************************************************************************************************)

DefineLatticeQuiver[
  "Names" -> {"SkewTrihexagonal"},
  "Quiver" -> Quiver @ {
     Labeled[1 -> 2, "a" | "b"],
     Labeled[2 -> 3, "b" | "c"],
     Labeled[3 -> 1, "c" | "a"]
  },
  "Group" -> InfiniteAbelianGroup[3, "Redundant"],
  "Dimension" -> 2
];

(**************************************************************************************************)

DefineLatticeQuiver[
  "Names" -> {"Cubic"},
  "Quiver" -> BouquetQuiver @ "xyz",
  "Group" -> InfiniteAbelianGroup[3],
  "Dimension" -> 3
];

(**************************************************************************************************)

$z = Sqrt[8/3];
$HCPGroup = TranslationGroup[{
  {1, -$sq3, 0}, {2, 0, 0}, {1, $sq3, 0},
  {1, -$isq3, $z}, {0, 2$isq3, $z}, {-1, -$isq3, $z}
} / 2];

DefineLatticeQuiver[
  "Names" -> {"HexagonalClosePacked"},
  "Quiver" -> Quiver @ <|
    "a" | "b" | "c" -> {1 -> 1, 2 -> 2},
    "d" | "e" | "f" -> {1 <-> 2}
  |>,
  "Group" -> $HCPGroup,
  "Dimension" -> 3
];

(**************************************************************************************************)

$FCCGroup = TranslationGroup[{
  {1, -$sq3, 0}, {2, 0, 0}, {1, $sq3, 0},
  {1, -$isq3, $z}, {0, 2$isq3, $z}, {-1, -$isq3, $z},
  {-1, $isq3, $z}, {0, -2$isq3, $z}, {1, $isq3, $z}
} / 2];

DefineLatticeQuiver[
  "Names" -> {"FaceCenteredCubic"},
  "Quiver" -> Quiver @ <|
    "a" | "b" | "c" -> {1 -> 1, 2 -> 2},
    "d" | "e" | "f" -> {1 -> 2},
    "g" | "h" | "i" -> {2 -> 1}
  |>,
  "Group" -> $FCCGroup,
  "Dimension" -> 3
];

(**************************************************************************************************)

PackageScope["$namedLatticeUsage"]

$namedLatticeUsage = StringTrim @ "
## Named lattices

* The only one-dimensional lattice is the 'Line' lattice.

* Named two-dimensional lattices (and their corresponding tilings) include:
| 'Square' | square tiling, aka quadrille |
| 'TruncatedSquare' | truncated square tiling, aka truncated quadrille |
| 'Triangular' | triangular tiling, aka deltille |
| 'Hexagonal' | hexagonal tiling, aka hextille |
| 'Rhombille' | rhombille tiling |
| 'Rhombitrihexagonal' | rhombitrihexagonal tiling, aka rhombihexadeltille |
| 'Trihexagonal' | trihexagonal tiling, aka hexadeltille |
| 'SnubTrihexagonal' | snub trihexagonal tiling, aka snub hextille |
| 'TruncatedTrihexagonal' | truncated trihexagonal tiling, aka truncated hexadeltille |

* Named three-dimensional Bravais lattices include:
| 'Cubic' | cubic lattice, aka primitive cubic |
| 'FaceCenteredCubic' | face centered cubic, aka FCC, cubic close-packed |
| 'HexagonalClosePacked' | hexagonal closed packed, aka HCP |

* Named parameterized lattices include:
| 'SquareTorus' | w$, h$ | square lattice on a w$ \[Times] h$ torus |
| 'TriangularTorus' | w$, h$ | triangular lattice on w$ \[Times] h$ torus |
| 'HexagonalTorus' | w$, h$ | hexagonal lattice on w$ \[Times] h$ torus |
| 'TrihexagonalTorus' | w$, h$ | trihexagonal lattice on w$ \[Times] h$ torus |
| 'RhombilleTorus' | w$, h$ | rhombille lattice on w$ \[Times] h$ torus |
| 'Lamplighter' | n$ | Cayley graph of lamplighter group on n$ lamps |
| 'Cube' | n$ | cube with n$ subdivided vertices on each face |
* All parameterized lattices have default parameters.
* Tori accept a single value h$, in which case w$ will be taken to be infinite.
* If %LayoutDimension -> 2 is given for tori, they will be plotted on a modulus plane.
";