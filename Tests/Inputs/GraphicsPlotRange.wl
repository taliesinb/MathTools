(* ::Package:: *)

(* ::Section:: *)
(*graphics*)


(* ::Subsection:: *)
(*primitives*)


GraphicsPlotRange[Graphics[Disk[{1,1},9], PlotRange -> {{-1, 1}, {-2, 2}}]]
GraphicsPlotRange[Graphics[Disk[{1,1},9], PlotRange -> {{-1, 1}, {-2, 2}}], PlotRangePadding -> 0.1]


GraphicsPlotRange[Graphics[Disk[{1,1},2]]]
GraphicsPlotRange[Graphics[Disk[{1,1},2]], PlotRangePadding -> 0.1]


GraphicsPlotRange[Graphics[Point[{1,1}]]]


$matrix2D = {{-1,-1},{1,1},{2,0}};
GraphicsPlotRange[Graphics[Circle[$matrix2D, 1]]]
GraphicsPlotRange[Graphics[Disk[$matrix2D, 1]]]
GraphicsPlotRange[Graphics[Ball[$matrix2D, 1]]]
GraphicsPlotRange[Graphics[Point[$matrix2D]]]
GraphicsPlotRange[Graphics[Line[$matrix2D]]]
GraphicsPlotRange[Graphics[BezierCurve[$matrix2D]]]
GraphicsPlotRange[Graphics[Arrow[$matrix2D]]]
GraphicsPlotRange[Graphics[Polygon[$matrix2D]]]


$matrices2D = {$matrix2D, {{2,0},{3,0},{2,1}}};
GraphicsPlotRange[Graphics[Line[$matrices2D]]]
GraphicsPlotRange[Graphics[Arrow[$matrices2D]]]
GraphicsPlotRange[Graphics[Polygon[$matrices2D]]]


(* ::Subsection:: *)
(*custom primitives*)


GraphicsPlotRange[ExtendedArrow[{{1,1},{2,2}}]]


GraphicsPlotRange[Graphics[NamedIcon[{0,0},{1,0},"Square"]]]


GraphicsPlotRange @ Graphics[{Point[{0,0}], NamedIcon[{1,1},{1,0},"Square"]}]


GraphicsPlotRange @ Graphics[{Point[{0,0}], NamedIcon[{1,1},{1,0},"Square", GraphicsScale -> 50, ImageSize -> 50]}]


(* ::Subsection:: *)
(*insets*)


GraphicsPlotRange[Graphics[Text["ABC",{1,1}]]]


GraphicsPlotRange[Graphics[Inset[Graphics[Disk[]],{1,1}]]]


(* ::Section:: *)
(*Graphics3D*)


GraphicsPlotRange[Graphics3D[Sphere[{1,1,1},9], PlotRange -> {{-1, 1}, {-2, 2}, {-3, 3}}]]
GraphicsPlotRange[Graphics3D[Sphere[{1,1,1},9], PlotRange -> {{-1, 1}, {-2, 2}, {-3, 3}}], PlotRangePadding -> 0.1]


GraphicsPlotRange[Graphics3D[Point[{1,1,1}]]]


$matrix3D = {{-1,-1,0},{1,1,1},{2,0,2}};
GraphicsPlotRange[Graphics3D[Sphere[$matrix3D,2]]]
GraphicsPlotRange[Graphics3D[Ball[$matrix3D,2]]]
GraphicsPlotRange[Graphics3D[Point[$matrix3D]]]
GraphicsPlotRange[Graphics3D[Line[$matrix3D]]]
GraphicsPlotRange[Graphics3D[BezierCurve[$matrix3D]]]
GraphicsPlotRange[Graphics3D[Arrow[$matrix3D]]]
GraphicsPlotRange[Graphics3D[Polygon[$matrix3D]]]


$matrices3D = {$matrix3D, {{2,0,1},{3,0,0},{2,1,3}}};
GraphicsPlotRange[Graphics3D[Line[$matrices3D]]]
GraphicsPlotRange[Graphics3D[Arrow[$matrices3D]]]
GraphicsPlotRange[Graphics3D[Polygon[$matrices3D]]]


(* ::Subsection:: *)
(*insets*)


GraphicsPlotRange[Graphics3D[Text["ABC"]]]
GraphicsPlotRange[Graphics3D[Text["ABC",{1,1,1}]]]


GraphicsPlotRange[Graphics3D[Inset[Graphics[Disk[]]]]]
GraphicsPlotRange[Graphics3D[Inset[Graphics[Disk[]],{1,1,1}]]]


(* ::Section:: *)
(*GraphicsBox, Graphics3DBox*)


(* ::Subsection:: *)
(*primitives*)


GraphicsPlotRange[GraphicsBox[DiskBox[{1,1},9], PlotRange -> {{-1, 1}, {-2, 2}}]]
GraphicsPlotRange[GraphicsBox[DiskBox[{1,1},9], PlotRange -> {{-1, 1}, {-2, 2}}], PlotRangePadding -> 0.1]
GraphicsPlotRange[Graphics3DBox[DiskBox[{1,1},5], PlotRange -> {{-1, 1}, {-2, 2}, {-3, 3}}]]
GraphicsPlotRange[Graphics3DBox[DiskBox[{1,1},5], PlotRange -> {{-1, 1}, {-2, 2}, {-3, 3}}], PlotRangePadding -> 0.1]


GraphicsPlotRange[GraphicsBox[DiskBox[{1,1},2]]]
GraphicsPlotRange[GraphicsBox[DiskBox[{1,1},2]], PlotRangePadding -> 0.1]
GraphicsPlotRange[Graphics3DBox[SphereBox[{1,1,1},2]]]
GraphicsPlotRange[Graphics3DBox[SphereBox[{1,1,1},2]], PlotRangePadding -> 0.1]


GraphicsPlotRange[GraphicsBox[PointBox[{1,1}]]]
GraphicsPlotRange[Graphics3DBox[Point3DBox[{1,1,1}]]]


$matrix2D = {{-1,-1},{1,1},{2,0}};
$matrix3D = {{-1,-1,0},{1,1,1},{2,0,2}};
GraphicsPlotRange[GraphicsBox[CircleBox[$m2d, 1]] /. $m2d -> $matrix2D]
GraphicsPlotRange[Graphics3DBox[SphereBox[$m3d, 1]] /. $m3d -> $matrix3D]


$matrices2D = {$matrix2D, {{2,0},{3,0},{2,1}}};
$matrices3D = {$matrix3D, {{2,0,1},{3,0,0},{2,1,3}}};
GraphicsPlotRange[GraphicsBox[LineBox[$m2d]] /. $m2d -> $matrices2D]
GraphicsPlotRange[GraphicsBox[ArrowBox[$m2d]] /. $m2d -> $matrices2D]
GraphicsPlotRange[GraphicsBox[PolygonBox[$m2d]] /. $m2d -> $matrices2D]
GraphicsPlotRange[Graphics3DBox[Line3DBox[$m3d]] /. $m3d -> $matrices3D]
GraphicsPlotRange[Graphics3DBox[Arrow3DBox[$m3d]] /. $m3d -> $matrices3D]
GraphicsPlotRange[Graphics3DBox[Polygon3DBox[$m3d]] /. $m3d -> $matrices3D]


(* ::Subsection:: *)
(*insets*)


GraphicsPlotRange[GraphicsBox[TextBox["ABC"]]]
GraphicsPlotRange[GraphicsBox[TextBox["ABC",{1,1}]]]
GraphicsPlotRange[GraphicsBox[InsetBox[GraphicsBox[DiskBox[{0,0},1]]]]]
GraphicsPlotRange[GraphicsBox[InsetBox[GraphicsBox[DiskBox[{0,0},1]],{1,1}]]]


GraphicsPlotRange[Graphics3DBox[Text3DBox["ABC"]]]
GraphicsPlotRange[Graphics3DBox[Text3DBox["ABC",{1,1,1}]]]
GraphicsPlotRange[Graphics3DBox[Inset3DBox[GraphicsBox[DiskBox[{0,0},1]]]]]
GraphicsPlotRange[Graphics3DBox[Inset3DBox[GraphicsBox[DiskBox[{0,0},1]],{1,1,1}]]]


(* ::Section:: *)
(*Graph*)


GraphicsPlotRange @ Graph[{0,1}, {0 -> 1}, VertexCoordinates -> {{0, 0}, {1, 1}}]


GraphicsPlotRange @ Graph[{0,1,2,4,5,6,7,8}, {}, GraphLayout -> "CircularEmbedding"]


GraphicsPlotRange @ Graph[{0,1,2}, {0 -> 1, 0 -> 2}, GraphLayout -> "LayeredEmbedding"]


GraphicsPlotRange @ GridQuiver[2,4]


GraphicsPlotRange @ LineQuiver[4]


(* ::Section:: *)
(*FixedGraphics*)


GraphicsPlotRange @ FixedGraphics[{Point[{0, 0}]}]


GraphicsPlotRange @ FixedGraphics[{Disk[{0,0},1]}]


GraphicsPlotRange @ FixedGraphics[{Circle[{0,0},1]}]


GraphicsPlotRange @ FixedGraphics[{Text["ABC",{0,0}]}]


(* compare with 
FixedGraphics[{Point[{{-0.5, -0.25}, {0.5, 0.25}}], NamedIcon[{0,0},{1,0}, "RightArrow", ImageSize -> 100]}] *)
GraphicsPlotRange @ FixedGraphics[{NamedIcon[{0,0},{1,0}, "RightArrow", ImageSize -> 100]}]
