PublicFunction[NamedDiagram]

SetUsage @ "
NamedDiagram['file$/name$'] looks up a diagram located in 'Diagrams/file$.m'.
NamedDiagram[$$, opts$$] forwards option definitions.

* Diagram files are loaded on demand, and will be reloaded if they change, or the whole package is reloaded.

* Diagram definitions are established with NamedDiagram['file$/name$'] := $$.

* Option forwarding is automatically inserted into definitions, into the outermost head.
* Use $Opts to put them elsewhere.
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

$lastModTimes = UAssoc[];
$currentBase = None;

NamedDiagram::badName = "No diagram named ``.";
NamedDiagram::fileError = "Message while loading ``.";
NamedDiagram::unknown = "Unknown diagram ``, not found when loading ``.";

NamedDiagram[name_String] := Scope[
  baseName = First @ StringSplit[name, "/"];
  path = LocalPath["Diagrams", baseName <> ".m"];
  If[!FileExistsQ[path], ReturnFailed["badName", name]];
  context = "QuiverGeometry`Diagrams`" <> baseName <> "`";

  fileModTime = UnixTime @ FileDate[path, "Modification"];
  If[$lastLoadCount =!= QuiverGeometryLoader`$LoadCount || fileModTime =!= $lastModTimes[path],
    $lastModTimes[path] ^= fileModTime;
    $lastLoadCount ^= QuiverGeometryLoader`$LoadCount;
    VPrint["Loading diagram file ", MsgPath @ path];
    result = Check[
      $currentBase = baseName <> "/";
      QuiverGeometryLoader`LoadSingleFile[path, context, {QuiverGeometryLoader`$ShortcutsContext}],
      $Failed
    ];
    If[FailureQ @ result, ReturnFailed["fileError", MsgPath @ path]];
  ];

  VPrint["Finding diagram ", name];
  result = Lookup[$NamedDiagramRegistry, name];
  If[!MissingQ[result], Return @ result];

  subkeys = Select[Keys @ $NamedDiagramRegistry, StringStartsQ[name <> "/"]];
  If[subkeys === {}, ReturnFailed["unknown", name, MsgPath @ path]];
  Column @ Lookup[$NamedDiagramRegistry, subkeys]
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

PrivateVariable[$NamedDiagramRegistry]

$NamedDiagramRegistry = Assoc[];

SetHoldRest[addDiagramDef];

addDiagramDef[name_, head_Symbol[Shortest[args__], opts___Rule]] /; FreeQ[Hold[args], HoldPattern @ $Opts] :=
  addDiagramDef[name, head[args, $Opts, opts]];

addDiagramDef[name_, rhs_] :=
  $NamedDiagramRegistry[name] := rhs;
