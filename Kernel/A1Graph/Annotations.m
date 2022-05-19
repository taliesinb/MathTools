PackageExport["VertexAnnotations"]
PackageExport["EdgeAnnotations"]
PackageExport["CustomGraphAnnotation"]

(**************************************************************************************************)

PackageExport["DeleteVertexAnnotations"]

DeleteVertexAnnotations[graph_Graph] :=
  AnnotationDelete[graph, VertexAnnotations];

DeleteVertexAnnotations[other_] := other;

(**************************************************************************************************)

PackageExport["LookupVertexAnnotations"]

LookupVertexAnnotations[graph_, key_, part_] :=
  Part[LookupVertexAnnotations[graph, key], part];

LookupVertexAnnotations[graph_, key_] :=
  Lookup[Replace[LookupAnnotation[graph, VertexAnnotations, None], None -> <||>], key, None];

LookupVertexAnnotations[graph_, All] :=
  Replace[LookupAnnotation[graph, VertexAnnotations, None], None -> <||>];

(**************************************************************************************************)

PackageExport["AttachVertexAnnotations"]

AttachVertexAnnotations[graph_, annotations_] := Scope[
  CheckIsGraph[1];
  joinAnnotation[graph, VertexAnnotations, annotations]
];

(**************************************************************************************************)

PackageExport["VertexAnnotationPresentQ"]

VertexAnnotationPresentQ[graph_, key_] :=
  KeyExistsQ[Replace[LookupAnnotation[graph, VertexAnnotations, None], None -> <||>], key]

(**************************************************************************************************)

PackageExport["DeleteEdgeAnnotations"]

DeleteEdgeAnnotations[graph_Graph] :=
  AnnotationDelete[graph, EdgeAnnotations];

DeleteEdgeAnnotations[other_] := other;

(**************************************************************************************************)

PackageExport["LookupEdgeAnnotations"]

LookupEdgeAnnotations[graph_, key_] :=
  Lookup[Replace[LookupAnnotation[graph, EdgeAnnotations, None], None -> <||>], key, None];

LookupEdgeAnnotations[graph_, All] :=
  Replace[LookupAnnotation[graph, EdgeAnnotations, None], None -> <||>];

(**************************************************************************************************)

PackageExport["AttachEdgeAnnotations"]

AttachEdgeAnnotations[graph_, annotations_] := Scope[
  CheckIsGraph[1];
  joinAnnotation[graph, EdgeAnnotations, annotations]
];

(**************************************************************************************************)

PackageExport["EdgeAnnotationPresentQ"]

EdgeAnnotationPresentQ[graph_, key_] :=
  KeyExistsQ[Replace[LookupAnnotation[graph, EdgeAnnotations, None], None -> <||>], key]

(**************************************************************************************************)

PackageScope["joinAnnotation"]

joinAnnotation[graph_, key_, newAnnotations_] := Scope[
  oldAnnotations = LookupAnnotation[graph, key, None];
  SetNone[oldAnnotations, <||>];
  Annotate[graph, key -> Join[oldAnnotations, newAnnotations]]
];

(**************************************************************************************************)

PackageExport["GraphVertexData"]
PackageScope["setupGraphVertexData"]

defineLiteralMacro[setupGraphVertexData,
  setupGraphVertexData[graph_, extra___Rule] := (
    $graphVertexIndex = AssociationRange @ VertexList @ graph;
    $graphVertexData = LookupVertexAnnotations[graph, All];
    AssociateTo[$graphVertexData, {extra}];
  )
];

getPart[list_List, i_Integer] /; 1 <= i <= Length[list] := Part[list, i];
getPart[_, _] := None;

getVertexElem[IndexedVertex[i_], data_] := getPart[data, i];
getVertexElem[vertex_, data_] := getPart[data, Lookup[$graphVertexIndex, vertex, 0]];

GraphVertexData[] := $graphVertexData;

GraphVertexData[vertex_, key_] :=
  getVertexElem[vertex, Lookup[$graphVertexData, key, None]];

(**************************************************************************************************)

PackageExport["GraphEdgeData"]
PackageScope["setupGraphEdgeData"]

defineLiteralMacro[setupGraphEdgeData,
  setupGraphEdgeData[graph_, extra___Rule] := (
    $graphEdgeIndex = AssociationRange @ EdgeList @ graph;
    $graphEdgeData = LookupEdgeAnnotations[graph, All];
    AssociateTo[$graphEdgeData, {extra}];
  )
];

getEdgeElem[IndexedEdge[i_], data_] := getPart[data, i];
getEdgeElem[edge_, data_] := getPart[data, Lookup[$graphEdgeIndex, edge, 0]];

GraphEdgeData[] := $graphEdgeData;

GraphEdgeData[edge_, key_] :=
  getEdgeElem[edge, Lookup[$graphEdgeData, key, None]];