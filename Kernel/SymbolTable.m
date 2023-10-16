(* this table serves two roles: it speeds up loading by resolving a whole bunch of known-good symbols
without resorting to Symbol, and it also helps the SublimeText script get primed with its categorization
of all system symbols for the purposes of syntax highlighting. so these lists aren't exhaustive, except
in one or two cases where there is no good programmatic way of gathering a particular symbol group (e.g. GraphicsDirective).

when generating syntax definitions, we supplement this table using the separate KernelSymbolTable.m, which the script
generates one per Mathematica release (and which consults this table for help), and also by scanning QG for all
the typed Package` declarations that categorize QG's own symbols.
*)

{
{"System`", "Package"} -> {
Package`Package,
Package`PackageExport, Package`PackageScope,  Package`PackageImport,
Package`SystemSymbol,  Package`SystemMacro,   Package`SystemVariable,  Package`SystemFunction,  Package`SystemHead,
Package`PublicSymbol,  Package`PublicMacro,   Package`PublicVariable , Package`PublicFunction,  Package`PublicHead,
Package`PrivateSymbol, Package`PrivateMacro,  Package`PrivateVariable, Package`PrivateFunction, Package`PrivateHead,

Package`PublicOption,  Package`PublicObject, Package`PublicScopedOption,
Package`PrivateMutatingFunction, Package`PublicMutatingFunction, Package`PublicDebuggingFunction,
Package`PublicTypesettingForm, Package`PublicTypesettingFormBox,
Package`PublicTypesettingBoxFunction, Package`PrivateTypesettingBoxFunction,
Package`SystemGraphicsDirective, Package`PublicGraphicsPrimitive, Package`PublicGraphicsDirective,
Package`PrivateSpecialFunction, Package`PublicSpecialFunction,
Package`PublicScopingFunction
},

{"System`", "Symbol"} -> {
True, False, None, Automatic, Inherited, All, Full, Indeterminate, Null,
Flat, OneIdentity, HoldFirst, HoldRest, HoldAll, HoldAllComplete,
Left, Right, Above, Below, Before, After, Center, Top, Bottom, Tiny, Small, Medium, Large,
SpanFromLeft, SpanFromAbove, SpanFromBoth, Baseline,
E, Pi, StartOfString, EndOfString, StartOfLine, EndOfLine, NumberString,
Hold[Infinity],
EndOfFile, Overflow, Underflow,
Horizontal, Vertical, Into, Next, Previous
},

{"System`", "Head"} -> {
Symbol, Integer, String, Complex, Real, Rational, List, Association,
Rule, RuleDelayed, TwoWayRule, DirectedEdge, UndirectedEdge, Missing,
RegularExpression, StringExpression, Interval,
Span, UpTo,
Threaded,
File, URL, C,
Slot, SlotSequence,
Hold[TemplateSlot, TemplateSlotSequence], Typed, TypeSpecifier,
HoldPattern, Condition, Alternatives, Pattern, PatternTest, Verbatim, Blank, BlankSequence, BlankNullSequence, Longest, Shortest, Except, KeyValuePattern, Optional, Repeated,
CompoundExpression, Placed,
SameAs, EqualTo, UnequalTo, LessThan, GreaterThan, LessEqualThan, GreaterEqualThan
},

{"System`", "Object"} -> {
Graph,
NumericArray, SparseArray,
Image, Image3D,
XMLElement, XMLObject,
Cell, CellGroup, CellGroupData, TextData, BoxData,
Hold[HTTPRequest, HTTPResponse]
},

{"System`", "Function"} -> {
Map, Scan, MapAt, MapIndexed, MapThread, Apply, Fold, FoldList, FixedPoint, Riffle,
RightComposition, Composition, Function, Identity, Construct,
Tuples, Subsets, Permutations,
Normal,
Rasterize,
Sort, Reverse, SortBy, GroupBy, GatherBy, Ordering, Count, Counts, CountsBy, DeleteDuplicates, DeleteDuplicatesBy,
Head, First, Last, Rest, Most, Part, Extract, Select, SelectFirst, Cases, FirstCase, Pick, Gather, Split, Partition, DeleteCases, Transpose, RotateLeft, RotateRight,
Position, FirstPosition,
Length, Dimensions, Prepend, Append, Take, Drop, Join, Catenate, Flatten, Union, Intersection, Complement, Range, Insert, Delete,
Replace, ReplacePart, ReplaceAll, ReplaceRepeated,
StringJoin, StringTake, StringDrop, StringCases, StringLength, TextString, StringTrim, StringReplace, StringRiffle, Characters, StringSplit, StringInsert, StringDelete, StringPadLeft, StringPadRight,
Keys, KeyTake, KeyDrop, KeySort, Values, Key, AssociationMap, AssociationThread, Lookup, KeyMap, KeyValueMap, Thread,
PositionIndex, Merge,
Options, OptionsPattern, OptionValue,
AllTrue, AnyTrue, NoneTrue,
StringQ, AssociationQ, ListQ, IntegerQ, FailureQ, VectorQ, MatrixQ, ArrayQ, NumberQ, GraphQ, NumericQ, BooleanQ,
FreeQ, MemberQ, MatchQ, SameQ, UnsameQ, Equal, TrueQ, MissingQ,
StringMatchQ, StringFreeQ, StringContainsQ, StringStartsQ, StringEndsQ, DuplicateFreeQ,
And, Or, Not, Greater, Less, LessEqual, GreaterEqual, Between, Positive, Negative, NonNegative,
If, While, Which, Do, Switch, Table, ConstantArray,
IdentityMatrix, UnitVector, ArrayFlatten, ArrayReshape, Inverse, RotationMatrix,
MessageName, Quiet, Check, General, Assert,
Sqrt, Power, Abs, Dot, Cross, Times, Plus, Minus, Subtract, Divide, Min, Max, Mod, MinMax, Floor, Ceiling, Round,
N, Sin, Cos, Tan, Tanh, ArcTan, Re, Im, Exp, Log, Log10,
Total, Mean, Median, Norm, Normalize, Clip, EuclideanDistance,
Interpolation,
EdgeList, VertexList, IndexGraph, VertexCount, EdgeCount, AdjacencyMatrix, Subgraph, PathGraph, GraphPlot, Graph3D,
SymbolName, Names
},

{"System`", "MutatingFunction"} -> {
AppendTo, PrependTo, AssociateTo, ApplyTo,
SetAttributes, Protect, Unprotect, Clear, ClearAll, Set, SetDelayed, SetOptions,
AddTo, SubtractFrom, TimesBy, DivideBy, KeyDropFrom, Increment, Decrement, PreIncrement, PreDecrement
},

{"GeneralUtilities`", "MutatingFunction"} -> {
GeneralUtilities`CacheTo,
GeneralUtilities`JoinTo, GeneralUtilities`UnionTo,
GeneralUtilities`KeyAddTo, GeneralUtilities`KeyAppendTo, GeneralUtilities`KeyJoinTo, GeneralUtilities`KeyPrependTo, GeneralUtilities`KeyUnionTo,
GeneralUtilities`SetRelatedSymbolGroup,
GeneralUtilities`SetHoldFirst, GeneralUtilities`SetHoldAll, GeneralUtilities`SetHoldAllComplete, GeneralUtilities`SetHoldRest, GeneralUtilities`SetListable
},

{"System`", "SpecialFunction"} -> {
Return, Throw, Catch, RuleCondition,
Splice, Sequence,
Hold, HoldComplete, Unevaluated, Hold @ Nothing,
Failure, $Failed, $Aborted, $TimedOut, Hold[$Context, $ContextPath],
MakeBoxes, ToBoxes, ToExpression, MakeExpression,
Break, Continue, Goto, Label,
DynamicModule,
ImportString, Import, Get, Read,  ReadString,  ReadLine, ReadList, BinaryRead, BinaryReadList, ReadByteArray,
ExportString, Export, Put, Write, WriteString, WriteLine, BinaryWrite, BinarySerialize, BinaryDeserialize,
Skip, Find, StreamPosition, SetStreamPosition, Streams,
OpenRead, OpenWrite, OpenAppend, Close, CreateFile,
NotebookGet, NotebookRead, NotebookImport, NotebookSave, NotebookFind, NotebookOpen, NotebookClose,
CreateNotebook, NotebookPut, NotebookWrite,
EvaluationNotebook, FrontEndExecute,
PreviousCell, NextCell, ParentCell, Cells, SelectedCells, SelectionMove, NotebookSelection,
CurrentValue, Message,
SyntaxInformation, Attributes, DownValues, OwnValues, UpValues, SubValues, FormatValues, DefaultValues
},

{"System`", "DebuggingFunction"} -> {
Print, CellPrint, System`PrintIF, System`PrintPF,
In, InString, Out,
EchoTiming, EchoFunction, EchoLabel, Echo, System`EchoIF, System`EchoPF, System`EchoFF, System`EchoGPF,
System`Capture,
CopyToClipboard,
URLFetch, URLRead,
Abort, AbortProtect,
Hold[Z1,Z2,Z3,Z4,Z5,Z6,Z7,Z8,Z9]
},

{"GeneralUtilities`", "DebuggingFunction"} -> {
GeneralUtilities`EchoHold, GeneralUtilities`EchoHoldSet, GeneralUtilities`EchoHoldTag, GeneralUtilities`Tap
},

{"System`", "Option"} -> {
TextAlignment, BaselinePosition, Alignment, AlignmentPoint, Spacings, Dividers, AspectRatio,
ImageSize, ImagePadding, ImageMargins, ContentPadding, FrameMargins, PlotRange, PlotRangePadding, PlotRangeClipping, BaseStyle, ColorFunction, ColorFunctionScaling,
ViewCenter, ViewVector, ViewPoint, ViewMatrix, ViewProjection, ViewAngle,
Frame, FrameTicks, Ticks, FrameStyle, FontFamily, FontWeight, FontSize, FontColor,
InterpolationOrder,
EdgeStyle, VertexStyle, EdgeShapeFunction, VertexShapeFunction, GraphLayout, DirectedEdges,
Lighting, ColorRules, PlotStyle, FillingStyle, MeshStyle, Epilog, Prolog
},

{"System`", "TypesettingForm"} -> {
Style,
Annotation, Labeled, Legended, Hold[Callout], Hyperlink, Tooltip, Interpretation,
Grid, Row, Column, Pane, Spacer, Framed, Item,
Dynamic, EventHandler,
Deploy, Defer,
Invisible, Magnify, Overlay,
MouseAppearance, Mouseover, StatusArea,
Button, ButtonBar, Checkbox, CheckboxBar, Slider, Slider2D, ProgressIndicator, RadioButton, RadioButtonBar, FlipView,
Subscript, Subsuperscript, Superscript, Underscript, Overscript, OverDot, UnderBar, OverBar, Element,
TraditionalForm, StandardForm, InputForm, StringForm
},

{"System`", "TypesettingBoxFunction"} -> {
GraphicsBox, Graphics3DBox, DynamicBox, DynamicModuleBox,
RowBox, GridBox, SubscriptBox, SuperscriptBox, SubsuperscriptBox,
StyleBox, TooltipBox, FrameBox, RawBoxes, AdjustmentBox, ErrorBox
},

{"System`", "GraphicsDirective"} -> {
Directive,
RGBColor, GrayLevel, Hue, CMYKColor, XYZColor, LABColor, LCHColor, LUVColor, Lighter, Darker, Opacity,
Hold[Thin, Thick], Thickness, AbsoluteThickness,
PointSize, AbsolutePointSize,
Offset, Scaled, ImageScaled,
EdgeForm, FaceForm, Texture,
SurfaceAppearance, CapForm, JoinForm, MaterialShading, StippleShading, Specularity, Glow,
Hold[Transparent, Red, Green, Blue, Black, White, Gray, Cyan, Magenta, Yellow, Brown, Orange, Pink, Purple, LightRed, LightGreen, LightBlue, LightGray, LightCyan, LightMagenta, LightYellow, LightBrown, LightOrange, LightPink, LightPurple,
Dashing, AbsoluteDashing, Dashed, Dotted, DotDashed],
Blurring, Haloing, DropShadowing,
Italic, Bold, Plain, Underlined, Struckthrough
},

{"System`", "GraphicsPrimitive"} -> {
Rotate, Translate, Scale, GeometricTransformation,
(* TranslationTransform, ScalingTransform, RotationTransform, TransformationMatrix, AffineTransform, *)
Graphics, Graphics3D,
Inset, Text, GraphicsGroup, GraphicsComplex, Raster,
Line, Circle, Rectangle, Triangle, Disk, DiskSegment, Point, Polygon, Arrow, Arrowheads,
BezierCurve, BSplineCurve, JoinedCurve, FilledCurve,
HalfLine, InfiniteLine, InfinitePlane,
Sphere, Tube, Cube, Cuboid, Cylinder, Cone, CapsuleShape
},

{"System`", "GraphicsBoxFunction"} -> {
GraphicsGroupBox, GraphicsComplexBox, RasterBox,
LineBox, CircleBox, RectangleBox, DiskBox, PointBox, PolygonBox, ArrowBox,
BezierCurveBox, BSplineCurveBox, JoinedCurveBox, FilledCurveBox,
InsetBox, RotationBox, GeometricTransformationBox, GeometricTransformation3DBox, TextBox, Text3DBox,
Arrow3DBox, Axis3DBox, AxisBox, BezierCurve3DBox,
BSplineCurve3DBox, BSplineSurface3DBox,
SphereBox, TubeBox, CuboidBox, CylinderBox, ConeBox,
ConicHullRegion3DBox, ConicHullRegionBox, GraphicsComplex3DBox,
GraphicsGroup3DBox, HexahedronBox, Inset3DBox, Line3DBox,
Point3DBox, Polygon3DBox, PolyhedronBox, PrismBox, PyramidBox, Raster3DBox,
TubeBezierCurveBox, TubeBSplineCurveBox, TetrahedronBox
},

{"System`", "ScopingFunction"} -> {
With, Block, Module
},

{"GeneralUtilities`", "ScopingFunction"} -> {
GeneralUtilities`Scope
},

{"Internal`", "ScopingFunction"} -> {
Internal`InheritedBlock
},

{"GeneralUtilites`", "SpecialFunction"} -> {
GeneralUtilities`ReturnFailed, GeneralUtilities`ThrowFailure, GeneralUtilities`CatchFailure,
GeneralUtilities`DefineMacro, GeneralUtilities`DefineLiteralMacro, GeneralUtilities`Seq
},

{"GeneralUtilities`", "Function"} -> {
GeneralUtilities`ContainsQ, GeneralUtilities`ScanIndexed,
GeneralUtilities`DeclareArgumentCount, GeneralUtilities`Match, GeneralUtilities`MatchValues,
GeneralUtilities`UnpackOptions, GeneralUtilities`KeyValueScan,
GeneralUtilities`SafePart, GeneralUtilities`SafeDrop, GeneralUtilities`SafeTake,
GeneralUtilities`DeepCases, GeneralUtilities`DeepUniqueCases,
GeneralUtilities`FilterOptions, GeneralUtilities`IndexOf,
GeneralUtilities`RuleQ, GeneralUtilities`ElementQ,
GeneralUtilities`Discard, GeneralUtilities`SelectDiscard,
GeneralUtilities`KeysValues, GeneralUtilities`DeleteNone, GeneralUtilities`SelectFirstIndex
},

{"GeneralUtilities`", "TypesettingForm"} -> {
GeneralUtilities`PrettyForm, GeneralUtilities`HoldPrettyForm
},

{"Internal`", "ScopingFunction"} -> {
Internal`WithLocalSettings
},

{"Internal`", "MutatingFunction"} -> {
Internal`StuffBag
},

(*some of these look useful so putting them here for now *)
{"Internal`", "Function"} -> {
Internal`BagPart, Internal`Bag, Internal`BagLength,
Internal`NonNegativeIntegerQ, Internal`NonNegativeMachineIntegerQ, Internal`NonPositiveIntegerQ,
Internal`PositiveIntegerQ, Internal`PositiveMachineIntegerQ, Internal`RealValuedNumberQ, Internal`RealValuedNumericQ,
Internal`NonPositiveMachineIntegerQ, Internal`RepetitionFromMultiplicity,
Internal`Reciprocal, Internal`OutermostToInnermost, Internal`PatternPresentQ, Internal`PatternFreeQ, Internal`SyntacticNegativeQ, Internal`HasComplex
},

{"Developer`", "Function"} -> {
Developer`AssociationVectorQ, Developer`Base64StringQ, Developer`DecodeBase64, Developer`DecodeBase64ToByteArray, Developer`EmptyQ,
Developer`NotEmptyQ, Developer`HoldAtomQ, Developer`HoldSymbolQ, Developer`ListOrAssociationQ, Developer`MachineComplexQ,
Developer`MachineIntegerQ, Developer`MachineRealQ, Developer`PackedArrayQ, Developer`PackedArrayForm, Developer`RealQ, Developer`StringOrStringVectorQ,
Developer`StringVectorQ, Developer`SymbolQ, Developer`ToList, Developer`ToPackedArray, Developer`FromPackedArray
},

{"Developer`", "SpecialFunction"} -> {
Developer`ReadRawJSONFile, Developer`ReadRawJSONString, Developer`ReadRawJSONStream,
Developer`WriteRawJSONFile, Developer`WriteRawJSONString, Developer`WriteRawJSONStream,
Developer`CellInformation
},

{"System`Private`", "Function"} -> {
System`Private`MightEvaluateWhenAppliedQ
}
}