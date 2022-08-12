PublicHead[DecoratedPath]

SetUsage @ "
DecoratedPath[coords$, decorations$] represents a path with decorated elements.
* decorations$ should be a list of the following elements:

"

Typeset`MakeBoxes[DecoratedPath[x_, r_], _, Graphics] := Null;