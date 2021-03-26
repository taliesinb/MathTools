Package["GraphTools`"]

PackageImport["GeneralUtilities`"]


PackageExport["LatticeVertex"]

SetUsage @ "
LatticeVertex[coords$] represents a vertex in a lattice graph with lattice coordinates coords$.
LatticeVertex[coords$, type$] represents a vertex in a lattice graph with type type$.
"
formatInteger[a_] := Style[If[Negative[a], UnderBar[Abs[a]], a], FontFamily -> "Avenir"];
formatLVertex[args:{__Integer}] := Row[formatInteger /@ args, "\[ThinSpace]"];
formatLVertex[args_] := args;
formatLVertex[args_, type_] := Row[{formatLVertex[args], " (", type, ")"}];

Format[LatticeVertex[args_List], StandardForm] := formatLVertex[args];
Format[LatticeVertex[args_List], TraditionalForm] := formatLVertex[args];
Format[LatticeVertex[args_List, type_], StandardForm] := formatLVertex[args, type];
Format[LatticeVertex[args_List, type_], TraditionalForm] := formatLVertex[args, type];


PackageExport["ProductVertex"]

SetUsage @ "
ProductVertex[a$, b$] represents a product of two vertices a$ and b$.
"

Format[ProductVertex[a_, b_], StandardForm] := CircleTimes[a, b];
Format[ProductVertex[a_, b_], TraditionalForm] := CircleTimes[a, b];


PackageExport["ProductVertices"]

ProductVertices[a_, b_] := ProductVertex @@@ Tuples[{toRange[a], toRange[b]}];

toRange[n_Integer] := Range[-n, n];
toRange[m_Integer ;; n_Integer] := Range[m, n];
toRange[m_Integer ;; n_Integer ;; s_Integer] := Range[m, n, s];


PackageExport["ContractedVertex"]

SetUsage @ "
ContractedVertex[{v$1, v$2, $$}] represents a vertex formed by contracting several vertices v$i.
ContractedVertex[vlist$, name$] represents a vertex formed by contraction with new name name$.
"

Format[ContractedVertex[v_]] := CirclePlus @@ v;
Format[ContractedVertex[v_, name_]] := Subscript[name, "+"];


PackageExport["SumVertex"]

SetUsage @ "
SumVertex[v$, g$] represents a vertex v$ from graph g$ in a sum of graphs.
"

SumVertex[i_][e_] := SumVertex[e, i];

Format[SumVertex[v_, g_]] := Subscript[v, g];
