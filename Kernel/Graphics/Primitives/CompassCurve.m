PublicHead[CompassCurve]

DeclareAtomicCurvePrimitive[CompassCurve, compassCurvePoints];

SignPrimitive["Vector", CompassCurve];

(**************************************************************************************************)

compassCurvePoints[CompassCurve[start:$Coord2P, spec_]] :=
  ToPackedReal @ FoldList[#1 + parseWord[#2]&, start, StringSplit @ spec];

parseWord[word_String /; StringMatchQ[word, DigitCharacter ~~ "/" ~~ DigitCharacter]] :=
  AngleVector[(ToExpression[word] + 1/4) * Tau];

parseWord[word_String] := Scope[$d = {0, 0}; Scan[parseLetter, Characters @ word]; $d];

$letters = <|
  "N" -> { 0,  1},   "E" -> { 1,  0},   "S" -> { 0, -1},   "W" -> {-1,  0},
  "n" -> { 0,  1}/2, "e" -> { 1,  0}/2, "s" -> { 0, -1}/2, "w" -> {-1,  0}/2
|>;

parseLetter = Case[
  "#" := ApplyTo[$d, Normalize];
  s_ /; KeyExistsQ[$letters, s] := AddTo[$d, $letters[s]];
]