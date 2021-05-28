PackageExport["GraphVertexAssignments"]

SetUsage @ "
GraphVertexAssignments[graph$, integers$] assigns values from integers$ to the vertices of graph in all possible ways,
returning lists of assignments in the vertex order. The default value of 0 is implied.
The automorphism group of the graph will be used to return only one representative of each assignment.
"

GraphVertexAssignments[graph_, values_] := Scope[
  n = VertexCount[graph];
  values = PadRight[values, n];
  perms = Permutations[values];
  group = Quiet @ Check[GraphAutomorphismGroup[graph], $Failed];
  If[FailureQ[group], Return[perms]];
  DeleteDuplicates[perms, MemberQ[Permute[#2, group], #1]&]
]

