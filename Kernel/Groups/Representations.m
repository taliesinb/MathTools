PublicObject[LinearRepresentationObject]

SetUsage @ "
LinearRepresentationObject[$$] represents a linear group representation.
* It has the following properties, accessible via rep$['prop$']:
| 'CayleyGraph' | Cayley graph for the representation |
| 'Dimension' | size of the representation matrices |
| 'Generators' | a list of RepresentationElement objects |
| 'Group' | group being represented |
| 'GroupOrder' | order of the group being represented |
| 'Identity' | the RepresentationElement for the identity |
| 'Type' | the type of representation, as a string  |
"

constructLinearGroupRepresentation[data_] := Scope[
  data = data;
  group = data["Group"];
  matrices = Normal /@ data["Generators"];
  matrices = ExpandUnitRoots[matrices];
  mod = Match[
    First @ data["Generators"],
    RepresentationElement[_, m_] :> m,
    None
  ];
  If[mod =!= None && Length[data["Identity"]] === 1,
    data["Identity"] = Append[data["Identity"], ToPacked @ mod]];
  type = Which[
    TranslationGroupQ[group] || AllTrue[matrices, TranslationMatrixQ], "Translation",
    AnyTrue[matrices, ComplexMatrixQ], Which[
      AllTrue[matrices, UnitaryMatrixQ], "Unitary",
      AllTrue[matrices, HermitianMatrixQ], "Hermitian",
      True, "Complex"
    ],
    AllTrue[matrices, UpperUnitriangularMatrixQ], "Unitriangular",
    AllTrue[matrices, UpperTriangularMatrixQ], "Triangular",
    AllTrue[matrices, PermutationMatrixQ], "Permutation",
    AbelianGroupQ[group] || AllTrue[matrices, TranslationMatrixQ], "Abelian",
    True, "Mixed"
  ];
  ConstructNoEntry[LinearRepresentationObject, Append[data, "Type" -> type]]
];

$representationIcon =
 Framed[Style["R", FontSize -> 20], FrameStyle -> Gray,
  ImageSize -> {35, 35}, Alignment -> Center]

LinearRepresentationObject /: MakeBoxes[object:LinearRepresentationObject[data_Association] ? HoldNoEntryQ, format_] := ModuleScope[
  UnpackAssociation[data, group, groupOrder, generators, dimension, type];
  BoxForm`ArrangeSummaryBox[
    LinearRepresentationObject, object, $representationIcon; None,
    (* Always displayed *)
    {
     {summaryItem["Group", group], SpanFromLeft},
     {summaryItem["Order", groupOrder], summaryItem["Type", type]},
     {summaryItem["Generators", Length[generators]], summaryItem["Dimension", dimension]}
     },
    (* Displayed on request *)
    {{Row[KeyValueMap[Labeled[#2, #1]&, generators], "  "], SpanFromLeft}},
    format,
    "Interpretable" -> Automatic
  ]
];

declareObjectPropertyDispatch[LinearRepresentationObject, representationProperty];

PrivateFunction[representationProperty]

(**************************************************************************************************)

PublicFunction[RepresentationObjectQ]

SetUsage @ "
RepresentationObjectQ[rep$] returns True if rep$ is a valid LinearRepresentationObject[$$].
"

RepresentationObjectQ[_LinearRepresentationObject ? HoldNoEntryQ] := True;
RepresentationObjectQ[_] := False;

(**************************************************************************************************)

PublicHead[RepresentationElement]

SetUsage @ "
RepresentationElement[matrix$] is the matrix representation of a group element.
RepresentationElement[matrix$, mod$] is a matrix representation applied modulo mod$.
* RepresentationElements are produced by a LinearRepresentationObject[$$].
* RepresentationElement will format as a compact matrix.
* rep$1[rep$2] will return the RepresentationElement for g$1 \[CircleDot] g$2.
"

ToInverseFunction[RepresentationElement[matrix_]] :=
  Quiet @ Check[RepresentationElement[Inverse[matrix]], None]

ToInverseFunction[RepresentationElement[matrix_, mod_]] := Scope[
  inv = Quiet @ Check[Inverse @ matrix, None];
  If[inv === None, None, RepresentationElement[inv, mod]]
];

ModForm /: RepresentationElement[ModForm[matrix_, mod_]] :=
  RepresentationElement[matrix, mod];

RepresentationElement /: Normal[RepresentationElement[matrix_, ___]] :=
  If[PackedArrayQ[matrix], matrix, ExpandUnitRoots @ matrix];

declareFormatting[
  RepresentationElement[matrix_ ? MatrixQ] :>
    renderRepresentationMatrix[matrix, $isTraditionalForm],
  RepresentationElement[matrix_ ? MatrixQ, mod_ ? MatrixQ] :>
    RepresentationElement @ MapThread[ModForm, {matrix, mod}, 2],
  RepresentationElement[matrix_ ? MatrixQ, mod_Integer] :>
    ModForm[RepresentationElement[matrix], mod],
  RepresentationElement[matrix_ ? MatrixQ, mod_Symbol] :>
    RepresentationElement[matrix]
];

splitImag[e_] := If[ContainsQ[e, _Complex], fmtComplexRow[Re @ e, Im @ e], e];

fmtComplexRow[0, im_] := Row[{im, $imagStr}];
fmtComplexRow[re_, im_] := Row[{re, "+", im, $imagStr}];

RepresentationElement[elem1_][RepresentationElement[elem2_]] := With[
  {res = Dot[elem1, elem2]},
  RepresentationElement @ If[PackedArrayQ[res], res, Expand @ res]
];

RepresentationElement[m1_, sym_Symbol][RepresentationElement[m2_, _]] :=
  RepresentationElement[
    Inner[sym, m1, m2],
    sym
  ];

RepresentationElement[elem1_, mod_][RepresentationElement[elem2_, _]] := With[
  {res = Mod[Expand @ Dot[elem1, elem2], mod]},
  RepresentationElement[If[PackedArrayQ[res], res, Expand @ res], mod]
];

(**************************************************************************************************)

PublicFunction[ToLinearRepresentation]

SetUsage @ "
ToLinearRepresentation[obj$] attempts to convert obj$ to a %LinearRepresentationObject.
* If obj$ is already a %LinearRepresentationObject, it is returned unchanged.
* If obj$ is a group or %PathRepresentation, its base representation is returned.
* If obj$ is a %RootSystem, its %TranslationGroup representation is returned.
* Otherwise, $Failed is returned.
"

ToLinearRepresentation = Case[
  r_LinearRepresentationObject ? HoldNoEntryQ := r;
  cq_PathRepresentationObject ? HoldNoEntryQ  := cq["Representation"];
  rs_RootSystemObject ? HoldNoEntryQ          := LinearGroupRepresentation @ TranslationGroup @ rs;
  g_ ? GroupQ                                                := LinearGroupRepresentation @ g;
  _                                                          := $Failed;
];


PrivateFunction[toRepresentation]

toRepresentation["Abelian", n_] := LinearGroupRepresentation @ InfiniteAbelianGroup[n];
toRepresentation["Redundant", n_] := LinearGroupRepresentation @ InfiniteAbelianGroup[n, "Redundant"];
toRepresentation[spec_, _] :=  ToLinearRepresentation @ spec;

(**************************************************************************************************)

PublicFunction[CustomLinearRepresentation]

SetUsage @ "
CustomLinearRepresentation[{matrix$1, $$, matrix$n}] takes a list of matrices and \
returns a LinearRepresentationObject[$$].
CustomLinearRepresentation[matrices$, group$] specifies that the representation is \
of the group group$.
CustomLinearRepresentation[matrices$, group$, mod$] constructs representations modulo mod$.
"

DeclareArgumentCount[CustomLinearRepresentation, {1, 2}];

declareSyntaxInfo[CustomLinearRepresentation, {_, _., _.}];

CustomLinearRepresentation::notmat = "First argument should be a list or association of matrices."
CustomLinearRepresentation::badrepmat = "Matrices have inconsistent dimensions: ``."

CustomLinearRepresentation[matrices_, group_:None, mod_:None] := Scope[
  Which[
    VectorQ[matrices, MatrixQ], matrices //= RangeAssociation,
    AssociationQ[matrices] && AllTrue[matrices, MatrixQ], Null,
    True, ReturnFailed["notmat"]
  ];
  matrices = ToPacked /@ Normal /@ matrices;
  dims = Dimensions @ Values @ matrices;
  If[!MatchQ[dims, {_, _, _}], ReturnFailed["badintcode", Dimensions /@ matrices]];
  dim = Part[dims, 2];
  generators = If[mod === None,
    RepresentationElement /@ matrices,
    RepresentationElement[#, mod]& /@ matrices
  ];
  order = If[group === None, Infinity, GroupOrder @ group];
  repData = <|
    "Group" -> group,
    "GroupOrder" -> order,
    "Generators" -> generators,
    "Dimension" -> dim,
    "Identity" -> identity
  |>;
  constructLinearGroupRepresentation[repData]
];


(**************************************************************************************************)

PublicFunction[RepresentationGenerators]

SetUsage @ "
RepresentationGenerators[obj$] returns a list of RepresentationElement objects \
for the generators of a group, LinearRepresentationObject, or PathRepresentationObject.
"

DeclareArgumentCount[RepresentationGenerators, 1];

RepresentationGenerators[obj_] := Scope[
  rep = CoerceToRep[1];
  rep["Generators"]
]

(**************************************************************************************************)

PublicFunction[LinearGroupRepresentation]

SetUsage @ "
LinearGroupRepresentation[group$] returns a LinearRepresentationObject of a group group$.
"

DeclareArgumentCount[LinearGroupRepresentation, 1];

General::badintcode = "Internal code returned inconsistent matrix dimensions: ``."

declareSyntaxInfo[LinearGroupRepresentation, {_}];

LinearGroupRepresentation[group_] := Scope[
  If[!GroupQ[group], ReturnFailed["notgroup", group]];
  matrices = ToPacked /@ Normal /@ makeGenerators[group];
  dims = Dimensions[Values[matrices] /. ModForm[m_, _] :> m];
  If[!MatchQ[dims, {_, _, _}], ReturnFailed["badintcode", Dimensions /@ matrices]];
  dim = Part[dims, 2];
  generators = RepresentationElement /@ matrices;
  id = makeIdentityRepresentation[group];
  SetNone[id, IdentityMatrix @ dim];
  repData = <|
    "Group" -> group,
    "GroupOrder" -> GroupOrder[group],
    "Generators" -> generators,
    "Dimension" -> dim,
    "Identity" -> RepresentationElement @ id
  |>;
  constructLinearGroupRepresentation[repData]
]

(**************************************************************************************************)

PublicFunction[TransformGenerators]

SetUsage @ "
TransformGenerators[representation$, transformation$] transforms the generators of a LinearRepresentationObject[$$], \
returning a new LinearRepresentationObject[$$].
* transformation$ should be a function taking an association of n$ matrices, where n$ is the number of existing \
generators, and returning a new association of matrices.
"

DeclareArgumentCount[TransformGenerators, 2];

TransformGenerators::badtrans = "The transformation returned an object of dimensions ``, instead of a list of square matrices.";

declareSyntaxInfo[TransformGenerators, {_, _}];

TransformGenerators[rep_, trans_] := Scope[
  rep = CoerceToRep[1];
  data = getObjectData[rep];
  gens = First /@ data["Generators"];
  newGens = trans @ gens;
  If[!AssociationQ[newGens], ReturnFailed["badtrans", dims]];
  dims = Dimensions @ Values @ newGens;
  If[!MatchQ[dims, {_, z_, z_}], ReturnFailed["badtrans", dims]];
  data["Generators"] = RepresentationElement /@ newGens;
  constructLinearGroupRepresentation[data]
];

