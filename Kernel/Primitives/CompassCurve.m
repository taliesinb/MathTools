PublicGraphicsPrimitive[CompassCurve]

DeclareAtomicCurvePrimitive[CompassCurve, compassCurvePoints];

SignPrimitive["Vector | Pair", CompassCurve];

(**************************************************************************************************)

compassCurvePoints[CompassCurve[src:$Coord2P, spec_]] :=
  ToPackedReal @ parsePath[src, spec];

CompassCurve::badjoin = "Unknown join spec ``."
compassCurvePoints[CompassCurve[{src:$Coord2P, dst:$Coord2P}, spec_]] := Scope[
  $stop = stop;
  {srcSpec, joinSpec, dstSpec} = If[StringCount[spec, ";"] === 2,
    SSplit[spec, ";", All],
    {spec, "", ""}
  ];
  srcPath = parsePath[src, srcSpec];
  dstPath = Rev @ parsePath[dst, dstSpec];
  {x1, y1} = L @ srcPath;
  {x2, y2} = F @ dstPath;
  joinPath = Switch[joinSpec,
    "", {},
    "V", {{x1, y2}},
    "H", {{x2, y1}},
    _, Message[CompassCurve::badjoin, joinSpec]; {}
  ];
  ToPackedReal @ Join[srcPath, joinPath, dstPath]
];

parsePath[p_, str_] := FoldList[#1 + parseWord[#2]&, p, SSplit @ str];

parseWord[word_Str /; SMatchQ[word, DigitCharacter ~~ "/" ~~ DigitCharacter]] :=
  AngleVector[(ToExpression[word] + 1/4) * Tau];

parseWord[word_Str] := Scope[$d = {0, 0}; Scan[parseLetter, Chars @ word]; $d];

$letters = <|
  "N" -> { 0,  1},   "E" -> { 1,  0},   "S" -> { 0, -1},   "W" -> {-1,  0},
  "n" -> { 0,  1}/2, "e" -> { 1,  0}/2, "s" -> { 0, -1}/2, "w" -> {-1,  0}/2
|>;

parseLetter = Case[
  "#" := ApplyTo[$d, Normalize];
  s_ /; KeyQ[$letters, s] := AddTo[$d, $letters[s]];
]