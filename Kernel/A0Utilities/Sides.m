PrivateVariable[$SideToCoords, $CoordsToSide]

$SideToCoords = <|
  Left        -> {-1,  0},
  Right       -> { 1,  0},
  Top         -> { 0,  1},
  Above       -> { 0,  1},
  Bottom      -> { 0, -1},
  Below       -> { 0, -1},
  BottomLeft  -> {-1, -1},
  BottomRight -> { 1, -1},
  TopLeft     -> {-1,  1},
  TopRight    -> { 1,  1},
  Center      -> { 0,  0}
|>

$CoordsToSide = InvertAssociation @ KDrop[$SideToCoords, {Below, Above}];

(**************************************************************************************************)

PrivateVariable[$SideToUnitCoords]

$SideToUnitCoords = Map[(# + 1)/2.&, $SideToCoords]

(**************************************************************************************************)

PrivateVariable[$FlipSideRules]

$FlipSideRules = {
  Left -> Right,
  Right -> Left,
  Top -> Bottom,
  Bottom -> Top,
  BottomLeft  -> TopRight,
  BottomRight -> TopLeft,
  TopLeft     -> BottomRight,
  TopRight    -> BottomLeft
};

(**************************************************************************************************)

PrivateVariable[$SideToRadians]

$SideToRadians = <|
  Left        ->  4/4 * Pi,
  TopLeft     ->  3/4 * Pi,
  Top         ->  2/4 * Pi,
  TopRight    ->  1/4 * Pi,
  Right       ->  0,
  BottomRight -> -1/4 * Pi,
  Bottom      -> -2/4 * Pi,
  BottomLeft  -> -3/4 * Pi
|>;

(**************************************************************************************************)

PrivateFunction[ApplyFlip]

SetUsage @ "
ApplyFlip[pos$, {flipx$, flipy$}] flips a pair of coordinates based on booleans flipx$ and flipy$.
ApplyFlip[pos$, flip$, trans$] swaps the coordinates if trans$ is True before flipping.
ApplyFlip[side$, flip$] transforms a symbolic side e.g. Top, Bottom, TopRight, Center, etc. and returns another symbol.
* ApplyFlip also works on Above and Below and returns these when given.
"

ApplyFlip[{x_, y_}, {flipX_, flipY_}, transpose_:False] :=
  If[transpose, Rev, Id] @ {If[flipX, -1, 1] * x, If[flipY, -1, 1] * y};

ApplyFlip[sym_Symbol, args___] := $CoordsToSide @ Sign @ ApplyFlip[$SideToCoords @ sym, args];

ApplyFlip[ab:(Above|Below), args___] := ApplyFlip[ab /. {Above -> Top, Below -> Bottom}, args] /. {Top -> Above, Bottom -> Below};
