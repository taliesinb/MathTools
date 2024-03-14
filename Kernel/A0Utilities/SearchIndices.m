PublicFunction[CreateMetaIndex]

SetUsage @ "
CreateMetaIndex[items$, {f$1, f$2, $$}] creates a meta index that uses f$i to create the i$'th index.
CreateMetaIndex[$$, preFn$] applies preFn$ to items before they are indexed.
* the resulting %MetaIndexObject[$$] can be searched via %MetaIndexObject[$$][query$] and will return the corresponding item$.
* each index is searched in turn until a hit is returned, or %None if none are found.
* if f$i returns a list of atoms, a literal sub-index is created via %CreateLiteralIndex.
* if f$i returns a list of lists, a sequence sub-index is created via %CreateSequenceIndex.
"

CreateMetaIndex[originalItems_List, fns_List, pre_:Identity] := Scope[

  If[originalItems === {},  ReturnFailed["noItems"]];

  items = Check[Map[pre, originalItems], $Failed];
  If[!ListQ[items] || MemberQ[items, $Failed], ReturnFailed["preFailed"]];

  fnsIndices = Map[fn |-> Which[
      keyItems = Check[fn /@ items, $Failed];
      keyItems === $Failed,     ReturnFailed["indexingFailed", fn],
      VectorQ[keyItems, AtomQ], {fn, CreateLiteralIndex @ keyItems},
      VectorQ[keyItems, ListQ], {fn, CreateSequenceIndex @ keyItems},
      True,                     ReturnFailed["indexingUnsupported", fn]
    ],
    fns
  ];

  MetaIndexObject[originalItems, fnsIndices]
];

CreateMetaIndex::preFailed = "Preprocessing function failed or produced messages.";
CreateMetaIndex::noItems = "Cannot create an index for empty list.";
CreateMetaIndex::indexingUnsupported = "Indexing function `` didn't return an indexable list.";
CreateMetaIndex::indexingFailed =  "Indexing function `` failed or issued messages.";

_CreateMetaIndex := BadArguments[];

(**************************************************************************************************)

PublicObject[MetaIndexObject]

MetaIndexObject[items_, fnsIndices_][query_] := Scope[
  MapApply[
    {fn, index} |-> If[IntQ[res = index[fn[query]]], Return[Part[items, res], Block]],
    fnsIndices
  ];
  None
];

MetaIndexObject[items_, fnsIndices_][query_] /; $verbose := Scope[
  VPrint["Searching for ", MsgForm @ query];
  VBlock @ MapApply[
    {fn, index} |-> If[
      VPrint["Searching index via ", MsgForm @ fn];
      VBlock[IntQ[res = index[fn[query]]]],
        VPrint["Found at ", res, ": ", MsgForm @ Part[items, res]];
        Return[Part[items, res], Block]],
    fnsIndices
  ];
  VPrint["Not found."];
  None
];

(**************************************************************************************************)

MakeBoxes[MetaIndexObject[items_List, fnIndexPairs_List], StandardForm] :=
  metaIndexObjectBoxes[Len @ items, Len @ fnIndexPairs];

metaIndexObjectBoxes[items_, indices_] :=
  skeletonBox["MetaIndexObject", $LightOrange, {Row[{items, " items"}], Row[{indices, " indices"}]}];

(**************************************************************************************************)

PublicFunction[CreateLiteralIndex]

SetUsage @ "
CreateLiteralIndex[items$] creates an index against a list of token sequences.
* a %LiteralIndexObject[$$] is returned that can be queried via %LiteralIndexObject[$$][query$].
* an exact match must occur.
"

CreateLiteralIndex[list_] := LiteralIndexObject @ Map[First] @ Select[SingleQ] @ UAssoc @ PositionIndex[list];

(**************************************************************************************************)

PublicHead[LiteralIndexObject]

LiteralIndexObject[assoc_][query_] := Lookup[assoc, query, None];

MakeBoxes[LiteralIndexObject[dict_Association], StandardForm] :=
  literalIndexObjectBoxes[Len @ dict];

literalIndexObjectBoxes[len_] :=
  skeletonBox["LiteralIndexObject", $LightOrange, {Row[{len, " items"}]}];

(**************************************************************************************************)

PublicFunction[CreateSequenceIndex]

SetUsage @ "
CreateSequenceIndex[sequences$] creates an index against a list of token sequences.
CreateSequenceIndex[$$, frac$] ignores tokens that occur in more than frac$ of sequences.
* a %SequenceIndexObject[$$] object that can be queried via %SequenceIndexObject[$$][query$].
* results are first sorted by weighted sum, where weights are IDF of matched tokens.
* the top 5 items are then re-weighted by fraction of the query that occurred in order in the result.
* the top result is then returned.
"

CreateSequenceIndex[sequences_List, cutoff_:0.25] := ModuleScope[
  If[!VectorQ[sequences, ListQ], ReturnFailed[]];
  cutoff = Max[Ceiling[Length[sequences] * cutoff], 50];
  elemCounts = Data`UnorderedAssociation @ Counts @ Flatten @ sequences;
  fluffElems = Keys @ Select[elemCounts, GreaterEqualThan[cutoff]];
  fluffElems //= Discard[StringMatchQ @ PunctuationCharacter];
  elemCounts = KeyDrop[elemCounts, fluffElems];
  elems = Keys @ elemCounts;
  elemDict = Data`UnorderedAssociation @ AssociationRange @ elems;
  sequences = Lookup[elemDict, #, Nothing]& /@ sequences;
  elemIndices = Lookup[PositionIndex[sequences, 2], Range @ Length @ elems];
  invElemCounts = Lookup[1.0 / elemCounts, elems];
  SequenceIndexObject[elemDict, invElemCounts, elemIndices, sequences]
];

_CreateSequenceIndex := BadArguments[];

(**************************************************************************************************)

MakeBoxes[SequenceIndexObject[dict_Association, icounts_List, index_List, seqs_List], StandardForm] :=
  sequenceIndexObjectBoxes[dict, icounts, index, seqs];

sequenceIndexObjectBoxes[dict_, icounts_, index_, seqs_] :=
  skeletonBox["SequenceIndexObject", $LightOrange, {
    Row[{Len @ dict,    " tokens"}],
    Row[{Len @ icounts, " token IDFs"}],
    Row[{Len @ seqs,    " items"}],
    Row[{Len @ index,   " index entries"}]
  }];

(**************************************************************************************************)

PublicObject[SequenceIndexObject]

SequenceIndexObject[wordToID_, wordIDCounts_, wordIDToDocIDs_, docIDToWordIDs_][queryWords_List] := Scope[

  If[$verbose,
    wordIDtoWord = AssociationInvert @ wordToID;
    docIDtoStr = Function[StringRiffle[Lookup[wordIDtoWord, Part[docIDToWordIDs, #]], " "]];
  ];

  rawQueryWordIDs = Lookup[wordToID, queryWords, Nothing];
  queryWordIDs = Lookup[wordToID, DeleteCases["\""] @ queryWords, Nothing];
  If[queryWordIDs === {}, VPrint["Query is empty."]; Return @ None];
  docIDs = Union @@ Part[wordIDToDocIDs, queryWordIDs];
  If[docIDs === {}, Return @ None];

  docScores = Map[
    docWordIDs |-> Total[Part[wordIDCounts, Intersection[docWordIDs, queryWordIDs]]] - Length[docWordIDs]/1000.,
    Part[docIDToWordIDs, docIDs]
  ];

  topScoreInds = If[Length[docScores] >= 10, Ordering[docScores, -10], Range[Length @ docScores]];
  bestDocScores = Part[docScores, topScoreInds];
  bestDocIDs = Part[docIDs, topScoreInds];

  sequenceMatchScores = MapThread[
    {docID, score} |-> SequenceMatchScore[Part[docIDToWordIDs, docID], rawQueryWordIDs],
    {bestDocIDs, bestDocScores}
  ];
  bestDocScores *= sequenceMatchScores;
  VPrint[Grid[Trans[docIDtoStr /@ bestDocIDs, sequenceMatchScores, bestDocScores], Alignment -> Left]];

  bestDocID = Part[bestDocIDs, MaximumIndex @ bestDocScores];
  bestDocID
];

(**************************************************************************************************)

PublicFunction[SequenceMatchScore]

(* fraction of sequence that is fully matched *)
SequenceMatchScore[a_List, b_List] := Divide[
  N @ Total @ Cases[SequenceAlignment[a, b], i:{__Int | __Str} :> Length[i]^2],
  Max[Length @ a, Length @ b]^2
];
