PublicGraphicsPrimitive[AnnotatedCoordinates]

(* AnnotatedCoordinates gets transformed by MapPrimitiveCoordinates but doesn't actually render.
used to keep track of named coordinates as they are transformed for later processing. *)

DeclareGraphicsPrimitive[AnnotatedCoordinates, "Matrix", {}&];


