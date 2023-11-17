PublicFunction[NamedDiagram]
PrivateVariable[$EnableDiagramReloading]

SetInitialValue[$EnableDiagramReloading, True];

SetUsage @ "
NamedDiagram['file$/name$'] looks up a diagram located in 'Diagrams/file$.m'.
NamedDiagram[$$, opts$$] forwards option definitions.

* Diagram files are loaded on demand, and will be reloaded if they change, or the whole package is reloaded.

* Diagram definitions are established with NamedDiagram['file$/name$'] := $$.

* Option forwarding is automatically inserted into definitions, into the outermost head.
* Use $Opts to put them elsewhere.

* NamedDiagram['file$/patt$'] will return all diagrams that match a string pattern, labeled by their name.
"


$diagramsPath = LocalPath["Diagrams"];

declareFunctionAutocomplete[NamedDiagram, {FileBaseName /@ FileNames["*.m", $diagramsPath]}];

(**************************************************************************************************)

$Opts = Sequence[];

NamedDiagram[name_String, opts__Rule] := Scope[
  $Opts = Sequence[opts];
  NamedDiagram @ name
];

(**************************************************************************************************)

$currentBase = None;

NamedDiagram::badName = "No diagram named ``.";
NamedDiagram::fileError = "Message while loading ``.";
NamedDiagram::unknown = "Unknown diagram ``, not found when loading ``.";

NamedDiagram[name_String] := Scope[
  baseName = First @ StringSplit[name, "/"];
  path = LocalPath["Diagrams", baseName <> ".m"];
  If[!FileExistsQ[path], ReturnFailed["badName", name]];
  context = "QuiverGeometry`Diagrams`" <> baseName <> "`";

  (* reloading invalidates all diagrams *)
  If[$lastLoadCount =!= QuiverGeometryLoader`$LoadCount,
    $lastLoadCount ^= QuiverGeometryLoader`$LoadCount;
    VPrint["Reload happened, clearing diagram registry."];
    ClearNamedDiagramRegistry[];
  ];

  diagram = Lookup[$NamedDiagramRegistry, name];
  diagramFound = !MissingQ[diagram];

  (* if we found the diagram, it might still be stale, so check this *)
  If[diagramFound && $EnableDiagramReloading,
    VPrint["Checking freshness of stored diagram."];
    diagramFound = FileDate[path, "Modification"] === $diagramFileModificationTimes[path];
    If[!diagramFound, VPrint["Diagram stale."]];
  ];

  (* if the freshness checks passed, return now *)
  If[diagramFound, Return @ diagram];

  (* freshness failed, or the diagram was never loaded *)
  VPrint["Loading diagram file ", MsgPath @ path];
  result = Check[
    $currentBase = baseName <> "/";
    QuiverGeometryLoader`LoadSingleFile[path, context, {QuiverGeometryLoader`$ShortcutsContext}],
    $Failed
  ];
  If[FailureQ @ result, ReturnFailed["fileError", MsgPath @ path]];
  $diagramFileModificationTimes[path] ^= FileDate[path, "Modification"];

  VPrint["Finding diagram ", name];
  diagram = Lookup[$NamedDiagramRegistry, name];
  If[!MissingQ[diagram], Return @ diagram];

  VPrint["Finding diagrams matching ", name];
  keys = Select[Keys @ $NamedDiagramRegistry, StringMatchQ[name]];
  If[keys === {}, ReturnFailed["unknown", name, MsgPath @ path]];
  diagrams = Lookup[$NamedDiagramRegistry, keys];

  baseNameLen = StringLength[baseName] + 1;
  ZipMap[LabeledForm[#1, StringDrop[#2, baseNameLen]]&, diagrams, keys]
];

(**************************************************************************************************)

qualifyName[base_, name_] /; StringFreeQ[name, "/"] := base <> name;

NamedDiagram::wrongBase = "Name `` doesn't have base ``."
qualifyName[base_, name_] := (
  If[!StringStartsQ[name, base], Message[NamedDiagram::wrongBase, name, base]];
  name
);

(**************************************************************************************************)

NamedDiagram /: SetDelayed[NamedDiagram[name_String], rhs_] := With[
  {fullName = qualifyName[$currentBase, name]},
  VPrint["Storing diagram ", fullName];
  addDiagramDef[name, rhs];
];

(**************************************************************************************************)

PublicSymbol[$Opts]

SetHoldRest[addDiagramDef];

addDiagramDef[name_, head_Symbol[Shortest[args__], opts___Rule]] /; FreeQ[Hold[args], HoldPattern @ $Opts] :=
  addDiagramDef[name, head[args, $Opts, opts]];

addDiagramDef[name_, rhs_] :=
  $NamedDiagramRegistry[name] := rhs;

(**************************************************************************************************)

PrivateFunction[ClearNamedDiagramRegistry]

ClearNamedDiagramRegistry[] := (
  $diagramFileModificationTimes = UAssoc[];
  $NamedDiagramRegistry = Assoc[];
);

ClearNamedDiagramRegistry[];
