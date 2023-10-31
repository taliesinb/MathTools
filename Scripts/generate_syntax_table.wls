#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Section:: *)
(*Update WolframLanguage Sublime Syntax*)

$packageDirectory = ParentDirectory @ DirectoryName @ $InputFileName;

Print["Loading QG."];
Get @ FileNameJoin[{$packageDirectory, "Kernel", "init.m"}];

Print["Populating symbols."];
$AllowInternet = True;
toStrings[e_] := Cases[e, Entity["WolframLanguageSymbol", name_] :> name];
$eSymbols = toStrings @ WolframLanguageData[];
$eOptions = toStrings @ EntityList[EntityClass["WolframLanguageSymbol", "OptionName"]];
$eAtomics = toStrings @ EntityList[EntityClass["WolframLanguageSymbol", "Atomic"]];
$eAutoevaluating = toStrings @ EntityList[EntityClass["WolframLanguageSymbol", "Autoevaluating"]];
$eCurrying = toStrings @ EntityList[EntityClass["WolframLanguageSymbol", "Curryable"]];

andThread[a_, b_] := Thread @ And[a, b];
orThread[a_, b_] := Thread @ Or[a, b];
mapStrings[fn_, e_] := ToExpression[e, InputForm, fn];
evaluatingNameQ[e_] := mapStrings[System`Private`MightEvaluateQ, e];
appliedEvaluatingNameQ[e_] := mapStrings[System`Private`MightEvaluateWhenAppliedQ, e];
immediateValueNameQ[e_] := mapStrings[System`Private`HasImmediateValueQ, e];
ownEvaluationNameQ[e_] := mapStrings[System`Private`HasOwnEvaluationsQ, e];
noEvaluationNameQ[e_] := mapStrings[System`Private`HasNoEvaluationsQ, e];
delayedValueNameQ[e_] := mapStrings[System`Private`HasDelayedValueQ, e];
formattingNameQ[e_] := Quiet[ContainsQ[#, TraditionalForm]& /@ ToExpression[e, InputForm, FormatValues]];
ownOrDelayedValueNameQ[e_] := andThread[valueNameQ @ e, delayedValueNameQ @ e];
autoloadNameQ[e_] := Quiet[!FreeQ[#, Package`ActivateLoad | System`Dump`AutoLoad]& /@ ToExpression[e, InputForm, OwnValues]];
vectorSelect[syms_, fn_] := Pick[syms, fn @ syms, True];
vectorDiscard[syms_, fn_] := Pick[syms, fn @ syms, False];


$symbolTable = Normal @ QuiverGeometryLoader`$SymbolTable;
toSymName[e_Symbol] := SymbolName[e];
toSymName[Hold[e___]] := Splice @ MapUnevaluated[Function[s, SymbolName[Unevaluated[s]], HoldFirst], {e}];
$systemGroups = Association @ Merge[Cases[$symbolTable, Rule[{"System`", group_}, list_] :> Rule[group, toSymName /@ list]], Catenate];
$allGroups = Association @ Merge[Cases[$symbolTable, Rule[{_, group_}, list_] :> Rule[group, toSymName /@ list]], Catenate];
$nonSystemGroups = Association @ Merge[Cases[$symbolTable, Rule[{Except["System`"], group_}, list_] :> Rule[group, toSymName /@ list]], Catenate];
$allGroupedSymbols = Catenate @ $allGroups;
(* check for duplicates / overlap between groups *)
overlaps = Select[Counts@Cases[$symbolTable,s_Symbol:>HoldForm[s],{0,Infinity}],#>1&];
If[Length[overlaps] > 0, Print["Warning: overlaps detected: ", overlaps]];


$symbols = Names["System`*"]; 
$blackStrings = {
	"Assessment", "Channel", "Cloud", "Databin", "Wavelet", "Generate", "Acoustic", 
	"PDE", "PointProcess", "Gauge", "Network", "Ask", "Audio", "NBody", "Remote",
	"SystemsModel", "Controllability", "Chat", "Astro", "Combinator",
	"Semantic", "LLM", "Question", "BioSequence", "Speech", "Spatial",
	"Financial", "Astronomical", "Blockchain", "Geogravity", "Geodesy", "Genome", "Molecule",
	"SystemModel", "StateSpace", "PID", "Geo" ~~ RegularExpression["[A-Z]"]
};
$blackEndingString = {"ModelQ", "Process"};
$whiteStrings = {"RunProcess", "StartProcess", "KillProcess", "SpatialMedian", "AlphaChannel", "SetAlphaChannel", "RemoveAlphaChannel"};
$symbols = Discard[StringEndsQ[$blackEndingString]] @ Select[StringFreeQ[$blackStrings]] @ $symbols;
$symbols = Union[$symbols, $whiteStrings];
selectSyms[fn_] := Select[$symbols, fn];
vecSelectSyms[fn_] := vectorSelect[$symbols, fn];
$allDollarStarting = selectSyms @ StringStartsQ["$"];
$allObjectEnding = selectSyms @ StringEndsQ["Object"];
$noEvals = vecSelectSyms[noEvaluationNameQ];
$allBoxEnding = Select[StringFreeQ["BoundingBox"]] @ Select[StringEndsQ["Box"]] @ $symbols;
$unlisted = Complement[$symbols, $eSymbols];
$formal = Select[$unlisted, StringLength[#]==1&];
$autoloads = vecSelectSyms[autoloadNameQ];
$formatting = vecSelectSyms[formattingNameQ];
$allFunctionLike = vecSelectSyms[appliedEvaluatingNameQ];
$allVariableLike = vecSelectSyms[evaluatingNameQ];


$finalGroups = $systemGroups;
(* this adds to the group, but first removes any symbols that already appear in any groups *)
addToGroup[group_, syms_] := KeyUnionTo[$finalGroups, group, Complement[syms, $allGroupedSymbols]];


$boxOptions = Select[$symbols, StringContainsQ @ "BoxOptions"];
$finalOptions = Union[Intersection[$eOptions, $symbols], $boxOptions, {"HTTPRequest", "HTTPResponse"}];
addToGroup["Option", $finalOptions];


$allObjectEnding = Discard[$allObjectEnding, StringStartsQ[{"Delete", "Create"}]];
$eAtomics2 = Complement[vectorSelect[$eAtomics, appliedEvaluatingNameQ], $systemGroups["Symbol"], $systemGroups["Head"]];
$finalObjects = Union[$allObjectEnding, $eAtomics2, {"ColorDataFunction", "DataDistribution"}];


addToGroup["Object", $finalObjects];


$extraPrimitives = StringSplit["GraphicsGroup GraphicsComplex Point Line FilledCurved JoinedCurve Inset Circle Disk Raster Raster3D Polygon Rectangle Arrow Axis BezierCurve BSplineCurve Sphere Tube Cube Ball Cuboid Cylinder Cone Prism Polyhedron Hexahedron Tetrahedron Pyramid Raster3D ConicHullRegion BSplineSurface"];
addToGroup["GraphicsPrimitive", $extraPrimitives];
$allPrims = $finalGroups["GraphicsPrimitive"];
isGraphicsBoxQ[name_String] := StringContainsQ[name, "3D"] || StringContainsQ[name, $allPrims];
{$graphicsBoxes, $nonGraphicsBoxes} = SelectDiscard[$allBoxEnding, isGraphicsBoxQ];
addToGroup["TypesettingBoxFunction", $nonGraphicsBoxes];
addToGroup["GraphicsBoxFunction", $graphicsBoxes];


defCells = NotebookImport[FileNameJoin @ {PacletManager`$SystemDocumentationDirectory, "English", "System", "Tutorials", "TextualInputAndOutput.nb"}, "DefinitionBox"->"Cell"];
owbmCells = Select[defCells,ContainsQ["CirclePlus"|"SubPlus"]];
$mathOperatorForms = DeepCases[owbmCells, TextData[name_] :> name];

(* we embed these here because they aren't particularly nice to compute *)
$uiForms = SymbolName /@ {
Animate, ListAnimate,
Animator, Annotation, Button, ButtonBar, Checkbox, CheckboxBar,
ClickPane, ClickToCopy, ColorSetter, ColorSlider, Column,
Dynamic, DynamicImage, DynamicWrapper, DynamicName, DynamicSetting,
EventHandler, FlipView,
Framed, Grid, Highlighted, Hyperlink, Inset, Interpretation, InputField, IntervalSlider, Item,
Labeled, LabeledSlider, Legended, ListPicker, LocatorPane, Manipulate,
Manipulator, MenuView, Mouseover, Opener, OpenerView, OverBar,
Overlay, Pane, Panel, PaneSelector, PasteButton, Piecewise, Placed,
PopupMenu, PopupView, ProgressIndicator, RadioButton, RadioButtonBar,
Rotate, Row, Setter, SetterBar, Slider, Slider2D, SlideView, Style,
Subscript, Subsuperscript, Superscript, Squiggled, TabView, Text, TextGrid,
Toggler, TogglerBar, Tooltip, Translate, UnderBar, VerticalSlider,
Deploy, Defer, Invisible, Magnify, MouseAppearance, Spacer,
StatusArea, TraditionalForm, StandardForm, ClickToCopy
};

addToGroup["TypesettingForm", Union[$mathOperatorForms, $uiForms]];
(* this is embedded verbatim in *)


$atomicUsagePattern = "is a style or option setting" | "is a symbolic spec" | "is a setting " | "represents" | "is a symbol representing" | "is a symbol that" | "is a symbol used" | "is the symbol for" | "is an attribute ";
isOptionNameQ[name_String] := With[{s = Symbol[name]}, {usage = MessageName[s, "usage"]}, StringStartsQ[usage, name ~~ (" is an option" | " is a low-level option")] && StringFreeQ[usage, "J/Link"]];
isObjectNameQ[name_String] := With[{s = Symbol[name]}, StringContainsQ[MessageName[s, "usage"], " is an object"]];
isGlobalVarNameQ[name_String] := With[{s = Symbol[name]}, StringStartsQ[MessageName[s, "usage"], name ~~ " is a global variable"]];
isGraphicsDirectiveNameQ[name_String] := With[{s = Symbol[name]}, StringContainsQ[MessageName[s, "usage"], " graphics directive"]];
isGraphicsPrimitiveNameQ[name_String] := With[{s = Symbol[name]}, StringContainsQ[MessageName[s, "usage"], " graphics primitive"]];
atomicUsageNameQ[name_String] := With[{s = Symbol[name]}, StringStartsQ[MessageName[s, "usage"], name ~~ " " ~~ $atomicUsagePattern]];
evalBoxQ[name_String] := MatchQ[RowBox[{name, ___}]];
boxifyingNameQ[name_String] := UnsameQ[ToBoxes @ Symbol @ name, name]; 
appliedBoxifyingNameQ[name_String] := Quiet @ With[{s = Symbol[name]}, !AllTrue[ToBoxes /@ {s[1], s[1,2], s[1,2,3], s[{1,2,3}], s[{1,2},{3,4}]}, evalBoxQ[name]]];
$inertSymbols = Complement[$noEvals, $finalOptions, $unlisted];
{$boxifyingSymbols, $appliedBoxifyingSymbols} = SelectDiscard[$inertSymbols, boxifyingNameQ];
{$appliedBoxifyingSymbols, $nonBoxifyingSymbols} = SelectDiscard[$appliedBoxifyingSymbols, appliedBoxifyingNameQ];
addToGroup["TypesettingForm", $appliedBoxifyingSymbols];
addToGroup["Symbol", $boxifyingSymbols];
{$nonBoxifyingSymbols, $compoundSymbols} = SelectDiscard[$nonBoxifyingSymbols, atomicUsageNameQ];
addToGroup["Symbol", $nonBoxifyingSymbols];
{$extraOptionSymbols, $compoundSymbols} = SelectDiscard[$compoundSymbols, isOptionNameQ];
addToGroup["Option", $extraOptionSymbols];
$compoundSymbols = Complement[$compoundSymbols, $allGroupedSymbols, Catenate @ $finalGroups];
$compoundSymbols = Discard[$compoundSymbols, isGlobalVarNameQ];
{$extraObjects, $compoundSymbols} = SelectDiscard[$compoundSymbols, isObjectNameQ];
addToGroup["Object", $extraObjects];
{$extraGraphicsDirectives, $compoundSymbols} = SelectDiscard[$compoundSymbols, isGraphicsDirectiveNameQ];
addToGroup["GraphicsDirective", $extraGraphicsDirectives];
{$extraGraphicsPrimitives, $compoundSymbols} = SelectDiscard[$compoundSymbols, isGraphicsPrimitiveNameQ];
addToGroup["GraphicsPrimitive", $extraGraphicsPrimitives];


(* ::Subsubsection:: *)
(*show all remaining things that would become functions*)


$finalFunctions = Complement[$symbols,Catenate@$finalGroups];
$finalFunctions = Select[$finalFunctions, StringLength[#]>1&];
{$finalVariables, $finalFunctions} = SelectDiscard[$finalFunctions, StringStartsQ["$"]];
$finalFunctions = Discard[$finalFunctions, StringEndsQ["$"]];


$allQGsymbols = Catenate @ QuiverGeometryLoader`$SymbolGroups;
$finalFunctions = Complement[$finalFunctions,$allQGsymbols];
$finalFunctions // Length


addToGroup["Variable", $finalVariables];
addToGroup["Function", $finalFunctions];


(* ::Subsubsection:: *)
(*finally, add all the non-system symbols from the symbol table (e.g. GU etc)*)


addToGroup2[group_, syms_] := KeyUnionTo[$finalGroups, group, syms];
KeyValueMap[addToGroup2, $nonSystemGroups];


(* ::Subsubsection:: *)
(*save these to disk to be reused without running this whole rigmarole (or at least only run it at once per new version)*)

addToGroup["Head", {"UAssociation"}];

Print["Populated ", Total[Length /@ $finalGroups], " symbols."];
Print["Group names: ", Keys[$finalGroups]];

Print["Exporting symbols to disk."];

$mxDir = FileNameJoin[{$packageDirectory, "Data", "Syntax", "SystemSymbols.mx"}];
ExportMX[$mxDir, KeySort @ $finalGroups]

Print["Size on disk: ", FileByteCount[$mxDir]];

Print["Updating Sublime Syntax files."];
UpdateSublimeSyntaxFiles[]
