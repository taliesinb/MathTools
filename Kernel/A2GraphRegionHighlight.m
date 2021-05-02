Package["GraphTools`"]

PackageImport["GeneralUtilities`"]


(**************************************************************************************************)

PackageExport["HighlightGraphRegion"]

SetUsage @ "
HighlightGraphRegion[graph$, highlights$] highlights regions of graph$ according to highlights$.
<*$GraphRegionHighlightUsage*>
* HighlightGraphRegion returns a Graph in which the option GraphRegionHighlight has been set to \
highlghts$. Any pre-existing highlights are preserved.
"

DeclareArgumentCount[HighlightGraphRegion, 2];

declareSyntaxInfo[HighlightGraphRegion, {_, _, OptionsPattern[]}];

HighlightGraphRegion[graph_, highlights_] := Scope[
  oldHighlights = AnnotationValue[graph, GraphRegionHighlight];
  oldHighlights = If[FailureQ[oldHighlights], {}, Developer`ToList @ oldHighlights];
  Annotate[graph, GraphRegionHighlight -> Join[oldHighlights, Developer`ToList @ highlights]]
];

(**************************************************************************************************)

PackageExport["GraphRegionHighlightGraphics"]

GraphRegionHighlightGraphics[graph_, regionSpec_] := Scope[

  graph = CheckGraphArg[1];

  GraphPlotScope[graph,
    graphics = resolveGraphRegionHighlightGraphics[regionSpec];
    plotRange = $GraphPlotRange;
    imageSize = $GraphPlotSize;
  ];

  If[FailureQ[graphics], ReturnFailed[]];

  Graphics[graphics, PlotRange -> plotRange, ImageSize -> imageSize, Framed -> False]
];

(**************************************************************************************************)

PackageScope["resolveGraphRegionHighlightGraphics"]

resolveGraphRegionHighlightGraphics[None | {}] :=
  {};

resolveGraphRegionHighlightGraphics[elem_] :=
  resolveGraphRegionHighlightGraphics[{elem}];

resolveGraphRegionHighlightGraphics[list_List] := Scope[
  $highlightsBag = Internal`Bag[];
  Scan[processHighlightSpec, list];
  Internal`BagPart[$highlightsBag, All]
];

(********************************************)
(** highlight processing code              **)
(********************************************)

sowHighlight[g_] := Internal`StuffBag[$highlightsBag, g];

$baseHighlightStyle := $baseHighlightStyle = Opacity[1.0, First @ $LightColorPalette];

Options[CustomHighlightedOptions] = {
  Background -> Automatic
};

GraphRegionHighlight::badelem = "Unknown highlight element ``.";

processHighlightSpec[other_] := Message[GraphRegionHighlight::badelem, Shallow[other]];

processHighlightSpec[Framed] :=
  sowHighlight @ {EdgeForm[{Red, Dashed}], FaceForm[None], Rectangle @@ (Transpose @ $GraphPlotRange)}

processHighlightSpec[expr_ ? GraphRegionElementQ] :=
  processHighlightSpec @ Highlighted @ expr;

processHighlightSpec[Highlighted[elems_, color:$ColorPattern:Automatic]] := Scope[

  {vertices, edges, negations} = processRegionSpec[elems];
  If[edges =!= {}, vertices = Complement[vertices, AllVertices @ Part[$IndexGraphEdgeList, edges]]];

  r = $GraphMaxSafeVertexSizeScaled * 1.5;

  style = color;
  SetAutomatic[style, $baseHighlightStyle];

  If[vertices =!= {},
    sowHighlight[{style, PointSize[r], Point @ Part[$GraphVertexCoordinates, vertices]}]];

  If[edges =!= {},
    sowHighlight[{style, JoinForm["Round"], CapForm["Round"], Thickness[r],
      edgeCoords = Part[$GraphEdgeCoordinateLists, edges];
      If[False && negations =!= {}, edgeCoords //= MapAt[Reverse, List /@ negations]];
      toJoinedCurve @ edgeCoords
    }]
  ];
];

toJoinedCurve[{a_}] := Line[a];

flipToMatch[a_, b_] := If[First[a] === First[b], Reverse @ a, a];
flipCoordinateLists[line_] := flipToMatch @@@ Partition[line, 2, 1, 1];

toJoinedCurve[list_List] := toSingleLine /@ Split[flipCoordinateLists @ list, Last[#1] == First[#2]&];

toSingleLine[edgeCoords_] := Line[Append[edgeCoords[[All, 1]], edgeCoords[[-1, 2]]]];

(* joinedCurveGroup[{a_}] := Line[a];
joinedCurveGroup[segments_] := JoinedCurve[Line /@ segments];
 *)