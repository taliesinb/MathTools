(* this table serves two roles: it speeds up loading by resolving a whole bunch of known-good symbols
without resorting to Symbol, and it also helps the SublimeText script get primed with its categorization
of all system symbols for the purposes of syntax highlighting. so these lists aren't exhaustive, except
in one or two cases where there is no good programmatic way of gathering a particular symbol group (e.g. GraphicsDirective).

when generating syntax definitions, we supplement this table using the separate Data/Wolfram/SystemSymbolTable.mx, which the script
generates one per Mathematica release (and which consults this table for help), and also by scanning QG for all
the typed Package` declarations that categorize QG's own symbols.
*)

{

{"System`", "Package"} -> {

 Package`SystemSymbol,  Package`SystemMacro,   Package`SystemVariable,  Package`SystemFunction,  Package`SystemHead,
 Package`PublicSymbol,  Package`PublicMacro,   Package`PublicVariable , Package`PublicFunction,  Package`PublicHead,
Package`PrivateSymbol, Package`PrivateMacro,  Package`PrivateVariable, Package`PrivateFunction, Package`PrivateHead,

Package`PublicMessageFunction,
Package`PrivateMessageFunction,

 Package`PublicSpecialFunction,  Package`PublicSpecialVariable,  Package`PublicStringPattern,  Package`PublicMutatingFunction,  Package`PublicDebuggingFunction,
Package`PrivateSpecialFunction, Package`PrivateSpecialVariable, Package`PrivateStringPattern, Package`PrivateMutatingFunction, Package`PrivateDebuggingFunction,

Package`PublicExprPattern,
Package`PrivateExprPattern,

 Package`PublicIOFunction,
Package`PrivateIOFunction,

 Package`PublicTypesettingBoxFunction,
Package`PrivateTypesettingBoxFunction,

Package`PublicGraphicsDirective,
Package`PublicGraphicsPrimitive,
Package`PublicObject,
Package`PublicOption,
Package`PublicScopedOption,
Package`PublicTypesettingForm,
Package`PublicTypesettingFormBox,
Package`PublicScopingFunction,

 Package`PublicCacheFunction,
Package`PrivateCacheFunction,
Package`CacheVariable
},

{"System`", "Symbol"} -> {
True, False, None, Automatic, Inherited, All, Full, Indeterminate, Null,
Flat, OneIdentity, HoldFirst, HoldRest, HoldAll, HoldAllComplete,
Left, Right, Above, Below, Before, After, Center, Top, Bottom, Tiny, Small, Medium, Large,
SpanFromLeft, SpanFromAbove, SpanFromBoth, Baseline,
E, Pi,
Hold[Infinity],
Expression, EndOfFile, Overflow, Underflow,
Horizontal, Vertical, Into, Next, Previous,
Smaller, Larger
},

{"System`", "StringPattern"} -> {
WordBoundary, Whitespace, NumberString,
WhitespaceCharacter, DigitCharacter, LetterCharacter, WordCharacter, PunctuationCharacter, HexadecimalCharacter,
StartOfLine, EndOfLine, StartOfString, EndOfString,
StringExpression, RegularExpression
},

{"System`", "Head"} -> {
Symbol, Integer, String, Complex, Real, Rational, List, Association,
Rule, RuleDelayed, TwoWayRule, DirectedEdge, UndirectedEdge, Missing,
Interval,
Span, UpTo,
Threaded,
File, URL, C,
Slot, SlotSequence,
Hold[TemplateSlot, TemplateSlotSequence], Typed, TypeSpecifier,
HoldPattern, Condition, Alternatives, Pattern, PatternTest, Verbatim, Blank, BlankSequence, BlankNullSequence, Longest, Shortest, Except, KeyValuePattern, Optional, Repeated, RepeatedNull,
CompoundExpression, Placed,
SameAs, EqualTo, UnequalTo, LessThan, GreaterThan, LessEqualThan, GreaterEqualThan, Notebook, FrontEndToken
},

{"System`", "Object"} -> {
Graph,
NumericArray, SparseArray,
Image, Image3D, AnimatedImage,
XMLElement, XMLObject,
Cell, CellGroup, CellGroupData, TextData, BoxData,
Hold[HTTPRequest, HTTPResponse],
Hold[DataStructure, Region],
NotebookObject, CompressedData
},

{"System`", "Function"} -> {
Map, MapApply, Scan, MapAt, MapIndexed, MapThread, Apply, Fold, FoldList, FixedPoint, Riffle,
RightComposition, Composition, Function, Identity, Construct,
Tuples, Subsets, Permutations,
Normal,
Sort, SortBy, Reverse, ReverseSort, ReverseSortBy, GroupBy, GatherBy, Ordering, Count, Counts, CountsBy, DeleteDuplicates, DeleteDuplicatesBy,
Head, First, Last, Rest, Most, Part, Extract, Select, SelectFirst, Cases, FirstCase, Pick, Gather, Split, Partition, DeleteCases, Transpose, RotateLeft, RotateRight,
Position, FirstPosition,
Length, Dimensions, Prepend, Append, Take, Drop, Join, Catenate, Flatten, Union, Intersection, Complement, Range, Insert, Delete,
ArrayDepth, Accumulate,
Replace, ReplacePart, ReplaceAll, ReplaceList, ReplaceRepeated,
StringJoin, StringTake, StringDrop, StringCases, StringLength, TextString, StringTrim, StringReplace, StringRiffle, Characters, CharacterRange, StringSplit, StringInsert, StringDelete, StringPadLeft, StringPadRight, IntegerString,
FromCharacterCode, ToCharacterCode, StringCount, StringExtract, StringPartition, StringPosition, StringRepeat, StringReplacePart, StringReverse,
Keys, KeyTake, KeyDrop, KeySort, KeySortBy, KeyUnion, KeyIntersection, KeyComplement, KeyExistsQ, Values, Key, AssociationMap, AssociationThread, Lookup, KeyMap, KeySelect, KeyValueMap, Thread,
PositionIndex, Merge,
Options, OptionsPattern, OptionValue, FilterRules,
AllTrue, AnyTrue, NoneTrue,
StringQ, AssociationQ, ListQ, IntegerQ, FailureQ, VectorQ, MatrixQ, ArrayQ, NumberQ, GraphQ, NumericQ, BooleanQ,
FreeQ, MemberQ, MatchQ, SameQ, UnsameQ, Equal, Unequal, TrueQ, MissingQ,
StringMatchQ, StringFreeQ, StringContainsQ, StringStartsQ, StringEndsQ, DuplicateFreeQ,
And, Or, Not, Greater, Less, LessEqual, GreaterEqual, Between, Positive, Negative, NonNegative,
If, While, Which, Do, Switch, Table, ConstantArray,
UnitVector, ArrayFlatten, ArrayReshape, Inverse, Hold @ RotationMatrix, Hold @ IdentityMatrix,
Sqrt, Power, Abs, Dot, Cross, Times, Plus, Minus, Subtract, Divide, Min, Max, Mod, MinMax, Floor, Ceiling, Round,
N, Sin, Cos, Tan, Tanh, ArcTan, Re, Im, Exp, Log, Log10, Boole, Sign,
Total, Mean, Median, Norm, Normalize, Clip, EuclideanDistance, Rescale, Standardize,
Interpolation, Blend,
EdgeList, VertexList, IndexGraph, VertexCount, EdgeCount, AdjacencyMatrix, Subgraph, PathGraph, GraphPlot, Graph3D,
SymbolName, Names, NameQ,
CoordinateBounds, CoordinateBoundsArray, CoordinateBoundingBox,
(* TODO: Move these to special since they are IO and side-effecty? *)
ParentDirectory, Directory, DirectoryName, DirectoryQ, CopyFile, CopyDirectory, DeleteFile, DeleteDirectory, CreateDirectory, RenameDirectory, RenameFile,
FileNames, ExpandFileName, AbsoluteFileName, FileNameJoin, FileNameSplit, FileBaseName, FileByteCount, FileDate, FileExistsQ, FileExtension, FileNameTake, FileNameDrop,
SubsetQ, EvenQ, OddQ, OrderedQ,
ByteCount, Compress, Uncompress, Hash,
Rasterize, ImageData, ImageResize, Hold @ ImageCrop, ImageTake,
MaximalBy, MinimalBy,
IntegerDigits,

(* standalone: *)
PatternHeadSymbol
},

{"System`", "MutatingFunction"} -> {
AppendTo, PrependTo, AssociateTo, ApplyTo,
SetAttributes, Protect, Unprotect, Clear, ClearAll,
Set, SetDelayed, TagSet, TagSetDelayed, TagUnset, UpSet, UpSetDelayed,
SetOptions,
AddTo, SubtractFrom, TimesBy, DivideBy, KeyDropFrom, Increment, Decrement, PreIncrement, PreDecrement
},

{"GeneralUtilities`", "MutatingFunction"} -> {
GeneralUtilities`CacheTo,
GeneralUtilities`JoinTo, GeneralUtilities`UnionTo,
GeneralUtilities`KeyAddTo, GeneralUtilities`KeyAppendTo, GeneralUtilities`KeyJoinTo, GeneralUtilities`KeyPrependTo, GeneralUtilities`KeyUnionTo,
GeneralUtilities`SetRelatedSymbolGroup,
GeneralUtilities`SetHoldFirst, GeneralUtilities`SetHoldAll, GeneralUtilities`SetHoldAllComplete, GeneralUtilities`SetHoldRest, GeneralUtilities`SetListable,
GeneralUtilities`UnpackOptions
},

{"System`", "SpecialFunction"} -> {
Return, Throw, Catch, RuleCondition, Evaluate, Fail,
Splice, Sequence,
ReleaseHold, Hold, HoldComplete, Unevaluated, Hold @ Nothing, CheckAbort, Check,
Failure, $Failed, $Aborted, $TimedOut, Hold[$Context, $ContextPath],
MakeBoxes, ToBoxes, ToExpression, MakeExpression,
Break, Continue, Goto, Label,
DynamicModule,
NotebookGet, NotebookRead, Hold @ NotebookImport, NotebookSave, NotebookFind, NotebookOpen, NotebookClose,
CreateNotebook, NotebookPut, NotebookWrite, NotebookDelete,
EvaluationNotebook, FrontEndExecute,
PreviousCell, NextCell, ParentCell, Cells, SelectedCells, EvaluationCell, SelectionMove, NotebookSelection,
CurrentValue,
SyntaxInformation, Attributes, DownValues, OwnValues, UpValues, SubValues, FormatValues, DefaultValues,

(* standalone: *)
MTLoader`Standalone`StandaloneHold,
MTLoader`Standalone`StandaloneSequence,
MTLoader`Standalone`StandaloneErrorMessage,
MTLoader`Standalone`StandaloneErrorHandler,
MTLoader`Standalone`$StandaloneErrorTag,

System`MacroHold, System`ExpandMacros, System`ContainsMacrosQ, Hold @ System`$MacroRules, Hold @ System`$MacroParentSymbol,
System`DefinePatternMacro,
System`DefineVariableMacro,
System`DefineSimpleMacro,
System`DefineMessageMacro
},

{"System`", "MessageFunction"} -> {
Message, MessageName, Quiet, General, Assert,

(* standalone: *)
System`CatchError, System`CatchErrorAsFailure, System`ThrowError, System`ThrowErrorMessage,
System`TopLevelEvaluationFunction
},

{"System`", "IOFunction"} -> {
ImportString, ImportByteArray, Import, Get, Read,  Hold @ ReadString,  Hold @ ReadLine, ReadList, BinaryRead, BinaryReadList, ReadByteArray,
ExportString, ExportByteArray, Export, Put, Write, WriteString,        Hold @ WriteLine, BinaryWrite, BinarySerialize, BinaryDeserialize,
Skip, Find, StreamPosition, SetStreamPosition, Streams,
OpenRead, OpenWrite, OpenAppend, Open, Close, CreateFile,
CopyToClipboard,
SetDirectory, ResetDirectory,
SublimeOpen, SublimeOpenProject (* Standalone *)
},

{"System`", "DebuggingFunction"} -> {
Print, CellPrint,
In, InString, Out,
EchoTiming, EchoFunction, EchoLabel, Echo,
Hold @ URLFetch, Hold @ URLRead,
Abort, AbortProtect,
Hold[Z1,Z2,Z3,Z4,Z5,Z6,Z7,Z8,Z9],

(* Init.m: *)
System`EchoIF, System`EchoPF, System`EchoFF, System`EchoGPF, System`PrintIF, System`PrintPF, System`PrintFF, System`Capture,

(* Standalone: *)
System`EchoDims, System`EchoKeys, System`EchoLen, System`EchoSet,
System`EchoH, System`EchoH0, System`EchoH1,
System`EchoF, System`EchoFL, System`EchoFH, System`EchoFLH
},

{"GeneralUtilities`", "DebuggingFunction"} -> {
GeneralUtilities`EchoHold, GeneralUtilities`EchoHoldSet, GeneralUtilities`EchoHoldTag, GeneralUtilities`Tap
},

{"System`", "Option"} -> {
TextAlignment, BaselinePosition, Alignment, AlignmentPoint, Spacings, Dividers, AspectRatio,
ImageResolution, ImageSize, ImagePadding, ImageMargins, ContentPadding, FrameMargins, PlotRange, PlotRangePadding, PlotRangeClipping, BaseStyle,
ColorFunction, ColorFunctionScaling,
ViewCenter, ViewVector, ViewPoint, ViewMatrix, ViewProjection, ViewAngle,
Frame, FrameTicks, Ticks, FrameStyle, FontFamily, FontWeight, FontSize, FontColor,
InterpolationOrder,
EdgeStyle, VertexStyle, EdgeShapeFunction, VertexShapeFunction, GraphLayout, DirectedEdges,
Lighting, ColorRules, PlotStyle, FillingStyle, MeshStyle, Epilog, Prolog, Verbose,
System`EdgeOpacity, Heads, CharacterEncoding, CompressionLevel, Background, IgnoreCase,
MaxItems, ItemSize, ItemStyle,
RowAlignments, RowMinHeight, RowsEqual, RowSpacings,
ColumnAlignments, ColumnsEqual, ColumnSpacings, ColumnWidths,

(* Standalone: *)
System`MaxRows, System`MaxColumns,
System`ItemLabels, System`ItemFunction, System`TooltipFunction,
System`MaxWidth, System`MaxHeight
},

{"System`", "TypesettingForm"} -> {
Style,
Annotation, Labeled, Legended, Hold[Callout], Hyperlink, Tooltip, Interpretation,
Grid, Row, Column, Pane, Spacer, Framed, Item,
Dynamic, EventHandler, Refresh,
Deploy, Defer,
Invisible, Magnify, Overlay,
MouseAppearance, Mouseover, StatusArea, PopupWindow,
Button, ButtonBar, Checkbox, CheckboxBar, Slider, Slider2D, ProgressIndicator, RadioButton, RadioButtonBar, FlipView,
Subscript, Subsuperscript, Superscript, Underscript, Overscript, OverDot, UnderBar, OverBar, Element,
TraditionalForm, StandardForm, InputForm, StringForm, OutputForm, NumberForm, HoldForm,

(* Standalone: *)
System`NiceTooltip, System`NicePane, System`NicePaster,
System`SymbolGrid, System`PlainGrid, System`NiceGrid, System`NiceMulticolumn
},

{"System`", "TypesettingBoxFunction"} -> {
GraphicsBox, Graphics3DBox, DynamicBox, DynamicModuleBox,
RowBox, GridBox, ItemBox, ButtonBox, PaneBox, PaneSelectorBox,
SubscriptBox, SuperscriptBox, SubsuperscriptBox, FractionBox, OverscriptBox, UnderscriptBox, UnderoverscriptBox,
StyleBox, TooltipBox, FrameBox, RawBoxes, AdjustmentBox, ErrorBox,
TemplateBox, TagBox, InterpretationBox,

(* Standalone: *)
System`ConstrainedMakeBoxes, System`InputFormLength,
System`NiceTooltipBoxes, system`NiceErrorBox
},

{"System`", "GraphicsDirective"} -> {
Directive,
RGBColor, GrayLevel, Hue, CMYKColor, XYZColor, LABColor, LCHColor, LUVColor, Lighter, Darker, Opacity,
Hold[Thin, Thick], Thickness, AbsoluteThickness,
PointSize, AbsolutePointSize,
Offset, Scaled, ImageScaled,
EdgeForm, FaceForm, Texture,
SurfaceAppearance, CapForm, JoinForm, Hold[MaterialShading, StippleShading], Specularity, Glow,
Hold[Transparent, Red, Green, Blue, Black, White, Gray, Cyan, Magenta, Yellow, Brown, Orange, Pink, Purple, LightRed, LightGreen, LightBlue, LightGray, LightCyan, LightMagenta, LightYellow, LightBrown, LightOrange, LightPink, LightPurple, Dashed, Dotted, DotDashed],
Hold[Blurring, Haloing, DropShadowing],
Dashing, AbsoluteDashing,
Italic, Bold, Plain, Underlined, Struckthrough
},

{"System`", "GraphicsPrimitive"} -> {
Rotate, Translate, Scale, GeometricTransformation,
Graphics, Graphics3D,
Inset, Text, GraphicsGroup, GraphicsComplex, Raster,
Line, Circle, Annulus, Rectangle, Triangle, Disk, DiskSegment, Point, Polygon, Arrow, Arrowheads,
BezierCurve, Simplex, BSplineCurve, JoinedCurve, FilledCurve,
HalfLine, InfiniteLine, InfinitePlane,
Sphere, Tube, Cube, Cuboid, Cylinder, Cone, CapsuleShape, StadiumShape,
FilledTorus, Dodecahedron, Icosahedron, Octahedron, Parallelepiped
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
With, Block, Module,

(* Standalone: *)
System`SWith
},

{"GeneralUtilities`", "ScopingFunction"} -> {
GeneralUtilities`Scope,
GeneralUtilities`ModuleScope
},

{"Internal`", "ScopingFunction"} -> {
Internal`InheritedBlock,
Internal`WithLocalSettings
},

{"GeneralUtilites`", "SpecialFunction"} -> {
GeneralUtilities`DefineMacro, GeneralUtilities`DefineLiteralMacro, GeneralUtilities`Seq,
GeneralUtilities`DeclareArgumentCount
},

{"GeneralUtilites`", "MessageFunction"} -> {
GeneralUtilities`ReturnFailed, GeneralUtilities`ThrowFailure, GeneralUtilities`CatchFailure
},

{"GeneralUtilities`", "Function"} -> {
GeneralUtilities`AssociationPairs, GeneralUtilities`AssociationInvert,
GeneralUtilities`ContainsQ, GeneralUtilities`ScanIndexed,
GeneralUtilities`Match, GeneralUtilities`MatchValues,
GeneralUtilities`KeyValueScan, GeneralUtilities`KeyIncrement, GeneralUtilities`KeyDecrement,
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
System`Private`ConstructNoEntry, System`Private`EntryQ, System`Private`HasAnyCodesQ, System`Private`HasAnyEvaluationsQ, System`Private`HasDelayedValueQ,
System`Private`HasDownCodeQ, System`Private`HasDownEvaluationsQ, System`Private`HasImmediateValueQ, System`Private`HasNoCodesQ, System`Private`HasNoEvaluationsQ,
(* System`Private`HasOwnCodeQ, this got disabled *)
System`Private`HasOwnEvaluationsQ, System`Private`HasPrintCodeQ, System`Private`HasSubCodeQ, System`Private`HasSubEvaluationsQ, System`Private`HasUpCodeQ,
System`Private`HasUpEvaluationsQ, System`Private`HoldEntryQ, System`Private`HoldNoEntryQ, System`Private`HoldNotValidQ, System`Private`HoldValidQ,
System`Private`MDataQ, System`Private`MightEvaluateQ, System`Private`MightEvaluateWhenAppliedQ, System`Private`NoEntryQ, System`Private`NotValidQ,
System`Private`WillNotEvaluateQ, System`Private`WillNotEvaluateWhenAppliedQ, System`Private`SetNoEntry, System`Private`SetValid, System`Private`ValidQ
},

{"Data`", "Head"} -> {
  Data`UnorderedAssociation
}
}