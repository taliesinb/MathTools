PrivateVariable[$flavorData]

$flavorData = <||>;

(**************************************************************************************************)

$flavorData["Base"] = <||>;
$flavorData["Base", "AnchorTemplate"]             = blankString;
$flavorData["Base", "ClassAttributeTemplate"]     = Function[Identity];
$flavorData["Base", "ExternalImportTemplate"]     = blankString;
$flavorData["Base", "FileAnimatedImageTemplate"]  = Inherited;
$flavorData["Base", "FileImageTemplate"]          = genericImageLinkTemplate;
$flavorData["Base", "FileVideoTemplate"]          = Inherited;
$flavorData["Base", "InlineLinkTemplate"]         = defaultLinkTemplate;
$flavorData["Base", "InlineMathTemplate"]         = wrapDollar;
$flavorData["Base", "KatexPostprocessor"]         = Identity;
$flavorData["Base", "MarkdownPostprocessor"]      = Identity;
$flavorData["Base", "MultilineMathTemplate"]      = wrapDoubleDollar;
$flavorData["Base", "RasterizationFunction"]      = standardRasterizationFunction;
$flavorData["Base", "RawHTMLTemplate"]            = blankString;
$flavorData["Base", "StringImageTemplate"]        = None;

(**************************************************************************************************)

$flavorData["Franklin"] = $flavorData["Base"];
$flavorData["Franklin", "AnchorTemplate"]         = StringFunction @ """\label{#1}""";
$flavorData["Franklin", "ClassAttributeTemplate"] = franklinClassAttr;
$flavorData["Franklin", "ExternalImportTemplate"] = StringFunction @ """\textinput{#1}""";
$flavorData["Franklin", "FileImageTemplate"]      = genericImageTagTemplate /* wrapCurly;
$flavorData["Franklin", "FileVideoTemplate"]      = genericVideoTagTemplate /* wrapCurly;
$flavorData["Franklin", "KatexPostprocessor"]     = splitOpenBraces;
$flavorData["Franklin", "RawHTMLTemplate"]        = wrapCurly;

franklinClassAttr[class_] := str |-> StringJoin[StringTrim @ "@@", StringRiffle[class, " "], "\n", str, "\n@@\n"];

(**************************************************************************************************)

$flavorData["Hugo"] = $flavorData["Base"];
$flavorData["Hugo", "AnchorTemplate"]             = StringFunction @ """<span id="#1"></span>""";
$flavorData["Hugo", "ClassAttributeTemplate"]     = hugoClassAttr;
$flavorData["Hugo", "FileImageTemplate"]          = genericImageTagTemplate;
$flavorData["Hugo", "FileVideoTemplate"]          = genericVideoTagTemplate;
$flavorData["Hugo", "InlineMathTemplate"]         = StringJoin["{{<k `", #, "`>}}"]&;
$flavorData["Hugo", "MultilineMathTemplate"]      = StringJoin["{{<kk `", #, "`>}}"]&;
$flavorData["Hugo", "KatexPostprocessor"]         = splitOpenBraces;
$flavorData["Hugo", "RawHTMLTemplate"]            = Identity;

hugoClassAttr[class_] := str |-> StringJoin[StringTrim @ str, "\n{", StringRiffle[StringJoinLeft[".", class], ", "], "}\n"];

(**************************************************************************************************)

$flavorData["Preview"] = $flavorData["Base"];
$flavorData["Preview", "RasterizationFunction"]   = linearSyntaxRasterizationFunction;
$flavorData["Preview", "StringImageTemplate"]     = Key["linearSyntax"];

(**************************************************************************************************)

$flavorData["IAWriter"] = $flavorData["Base"];
$flavorData["IAWriter", "ExternalImportTemplate"] = Identity;
$flavorData["IAWriter", "KatexPostprocessor"]     = StringReplaceRepeated[{"#" ~~ d:DigitCharacter ~~ "\\" :> "{#" <> d <> "}\\"}];
$flavorData["IAWriter", "MultilineMathTemplate"]  = wrapDoubleBrace;

(**************************************************************************************************)

$flavorData[None] = $flavorData["Base"];
$flavorData[None, "FileImageTemplate"] = None;

MacroEvaluate @ UnpackAssociation[
  $flavorData[None],
  $anchorTemplate,
  $classAttributeTemplate,
  $externalImportTemplate,
  $fileAnimatedImageTemplate,
  $fileImageTemplate,
  $fileVideoTemplate,
  $inlineLinkTemplate,
  $inlineMathTemplate,
  $katexPostprocessor,
  $markdownPostprocessor,
  $multilineMathTemplate,
  $rasterizationFunction,
  $rawHTMLTemplate,
  $stringImageTemplate
];

(**************************************************************************************************)

wrapDollar[e_]            := StringJoin["$", e, "$"];
wrapDoubleDollar[e_]      := StringJoin["$$\n", e, "\n$$"];
wrapDoubleBrace[e_]       := StringJoin["\\\\[\n", e, "\n\\\\]"];
breakUnderline[e_]        := StringReplace[e, {"\\_" -> "\\_ ", "_" -> " _ "}];
wrapCurly[a_]             := StringJoin["~~~\n", StringTrim @ a, "\n~~~"];
splitOpenBraces[e_]       := StringReplaceRepeated[e, "{{" -> "{ {"];
escapeBackslash[e_]       := StringReplace[e, {"\\\\" -> "\\\\\\\\", "\\," -> "\\\\,"}];

blankString = Function[""];

(**************************************************************************************************)

genericImageTagTemplate = StringFunction @ StringTrim @  """
<img class="#classlist" src="#relativepath" width="#width" alt="#caption">
""";

genericVideoTagTemplate = StringFunction @ StringTrim @  """
<video width="#width" height="#height" controls>
  <source src="#relativepath" type="video/mp4">
</video>
""";

genericImageLinkTemplate = StringFunction @ "![](#relativepath)";
