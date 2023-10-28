PrivateHead[GraphCache]

SetUsage @ "
GraphCache[sym$] represents a cache of computed properties of a graph that stores cached properties \
in sym$.
";

SetHoldAllComplete[GraphCache];

(**************************************************************************************************)

PrivateSpecialFunction[declareGraphCacheFriendly]

declareGraphCacheFriendly[sym_] := (
  SetValid[sym];
  SetNoEntry[sym];
);
declareGraphCacheFriendly[syms__] := Scan[declareGraphCacheFriendly, {syms}];

(**************************************************************************************************)

PrivateFunction[CreateGraphCache]

SetHoldRest[CreateGraphCache];

CreateGraphCache[graph_Graph, symbol_Symbol] := (
  symbol = UAssociation[];
  GraphCache[graph, symbol]
);

(**************************************************************************************************)

MakeBoxes[GraphCache[_, sym_Symbol], StandardForm] :=
  RBox["GraphCache", "[", "{", RowBox @ Flatten @ Riffle[ToBoxes /@ Keys @ sym, ","], "}", "]"];

GraphCache /: Print[GraphCache[graph_, sym_]] := Print[Keys @ sym];

(* for ordinary functions, evaluate them on the raw graph *)
GraphCache /: f_Symbol[GraphCache[graph_, sym_], args___] /; HasDownEvaluationsQ[f] && NotValidQ[f] :=
  f[graph, args];

(* for cache-friendly functions, which have the entryq flag set if they are not in the process of evaluating,
first check the cache, and if not present, mark them as being evaluated, compute the result by passing in the GraphCache,
then cache the result *)
GraphCache /: f_Symbol[gc:GraphCache[_, sym_], args___] /; ValidQ[f] && NoEntryQ[f] :=
  Lookup[sym, Key @ {f, args}, evaluateWithoutRecursion[sym, f, gc, args]];

SetHoldFirst[evaluateWithoutRecursion];
evaluateWithoutRecursion[sym_, f_, gc_, args___] := Block[{res},
  SetNoEntry[f, False];
  sym[{f, args}] = res = f[gc, args];
  SetNoEntry[f];
  res
];
