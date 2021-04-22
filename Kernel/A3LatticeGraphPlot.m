Package["GraphTools`"]


PackageImport["GeneralUtilities`"]


PackageExport["LatticeGraphPlot"]

LatticeGraphPlot[graph_Graph] /; (AnnotationValue[graph, GraphPlottingFunction] =!= LatticeGraphPlot || !GraphQ[$Graph]) :=
  ExtendedGraphPlot @ Annotate[graph, GraphPlottingFunction -> LatticeGraphPlot];

LatticeGraphPlot[___] := $Failed;

LatticeGraphPlot[graph_ ? GraphQ] := Scope[


];