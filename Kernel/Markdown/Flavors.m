PrivateVariable[$flavorData]

$flavorData = <||>;

(**************************************************************************************************)

$flavorData["Base"] = <||>;
$flavorData["Base", "AllowTableHeaderSkip"]       = False;
$flavorData["Base", "AnchorTemplate"]             = blankString;
$flavorData["Base", "ClassAttributeTemplate"]     = Fn[Id];
$flavorData["Base", "ExternalImportTemplate"]     = blankString;
$flavorData["Base", "FileAnimatedImageTemplate"]  = Inherited;
$flavorData["Base", "FileImageTemplate"]          = genericImageLinkTemplate;
$flavorData["Base", "FileAnimatedImageTemplate"]  = Inherited;
$flavorData["Base", "InlineLinkTemplate"]         = defaultLinkTemplate;
$flavorData["Base", "InlineMathTemplate"]         = wrapDollar;
$flavorData["Base", "KatexFontTemplate"]          = wrapDollar;
$flavorData["Base", "KatexPostprocessor"]         = Id;
$flavorData["Base", "MarkdownPostprocessor"]      = Id;
$flavorData["Base", "MultilineMathTemplate"]      = wrapDoubleDollar;
$flavorData["Base", "RasterizationFunction"]      = standardRasterizationFunction;
$flavorData["Base", "RawHTMLTemplate"]            = blankString;
$flavorData["Base", "StringImageTemplate"]        = None;

(**************************************************************************************************)

$flavorData["Franklin"] = $flavorData["Base"];
$flavorData["Franklin", "AllowTableHeaderSkip"]      = True;
$flavorData["Franklin", "AnchorTemplate"]            = StringFunction @ """\label{#1}""";
$flavorData["Franklin", "ClassAttributeTemplate"]    = franklinClassAttr;
$flavorData["Franklin", "ExternalImportTemplate"]    = StringFunction @ """\textinput{#1}""";
$flavorData["Franklin", "FileImageTemplate"]         = genericImageTagTemplate /* wrapCurly;
$flavorData["Franklin", "FileAnimatedImageTemplate"] = genericAnimatedImageTagTemplate /* wrapCurly;
$flavorData["Franklin", "KatexPostprocessor"]        = splitOpenBraces;
$flavorData["Franklin", "RawHTMLTemplate"]           = wrapCurly;

franklinClassAttr[class_] := str |-> SJoin[STrim @ "@@", SRiffle[class, " "], "\n", str, "\n@@\n"];

(**************************************************************************************************)

$flavorData["Pandoc"] = $flavorData["Base"];
$flavorData["Pandoc", "KatexPostprocessor"]         = SRep["\n\n" -> " "];

(**************************************************************************************************)

$flavorData["Hugo"] = $flavorData["Base"];
$flavorData["Hugo", "AllowTableHeaderSkip"]       = True;
$flavorData["Hugo", "AnchorTemplate"]             = StringFunction @ """<span id="#1"></span>""";
$flavorData["Hugo", "ClassAttributeTemplate"]     = hugoClassAttr;
$flavorData["Hugo", "FileImageTemplate"]          = genericImageTagTemplate;
$flavorData["Hugo", "FileAnimatedImageTemplate"]  = genericAnimatedImageTagTemplate;
$flavorData["Hugo", "InlineMathTemplate"]         = SJoin["{{<k \"", #, "\">}}"]&;
$flavorData["Hugo", "KatexFontTemplate"]          = hugoKatexFontFunction;
$flavorData["Hugo", "MultilineMathTemplate"]      = SJoin["{{<kk \"", #, "\">}}"]&;
$flavorData["Hugo", "KatexPostprocessor"]         = splitOpenBraces;
$flavorData["Hugo", "RawHTMLTemplate"]            = Id;
$flavorData["Hugo", "ExternalImportTemplate"]     = StringFunction @ """{{< readfile file="#1" >}}""";

(* TODO: fill in more cases here, like Typewriter, SanSerif, etc. and make them match up with the TypewriteForm etc. *)
hugoKatexFontFunction[str_Str] := $katexFontSpanTemplate[str, If[SContainsQ[str, DoubleStruckCharacter], "blackboard", "math"]];
$katexFontSpanTemplate = StringFunction @ """<span class='#2Font'>#1</span>""";

hugoClassAttr[class_] := str |-> SJoin[STrim @ str, "\n{", SRiffle[StringJoinLeft[".", class], ", "], "}\n"];

(**************************************************************************************************)

$flavorData["Preview"] = $flavorData["Base"];
$flavorData["Preview", "RasterizationFunction"]   = linearSyntaxRasterizationFunction;
$flavorData["Preview", "StringImageTemplate"]     = Key["linearSyntax"];

(**************************************************************************************************)

$flavorData["Jupyter"] = $flavorData["Base"];
$flavorData["Jupyter", "FileImageTemplate"]       = None;
$flavorData["Jupyter", "StringImageTemplate"]     = $JupyterStringImageTemplate;
$flavorData["Jupyter", "FileImageTemplate"]       = $JupyterFileImageTemplate;

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
  $fileAnimatedImageTemplate,
  $inlineLinkTemplate,
  $inlineMathTemplate,
  $katexFontTemplate,
  $katexPostprocessor,
  $markdownPostprocessor,
  $multilineMathTemplate,
  $rasterizationFunction,
  $rawHTMLTemplate,
  $stringImageTemplate
];

(**************************************************************************************************)

wrapDollar[e_]            := SJoin["$", e, "$"];
wrapDoubleDollar[e_]      := SJoin["$$\n", e, "\n$$"];
wrapDoubleBrace[e_]       := SJoin["\\\\[\n", e, "\n\\\\]"];
breakUnderline[e_]        := SRep[e, {"\\_" -> "\\_ ", "_" -> " _ "}];
wrapCurly[a_]             := SJoin["~~~\n", STrim @ a, "\n~~~"];
splitOpenBraces[e_]       := StringReplaceRepeated[e, "{{" -> "{ {"];
escapeBackslash[e_]       := SRep[e, {"\\\\" -> "\\\\\\\\", "\\," -> "\\\\,"}];

blankString = Fn[""];

(**************************************************************************************************)

genericImageTagTemplate = StringFunction @ STrim @  """
<img class="#classlist" src="#url" width="#width" alt="#caption">
""";

genericAnimatedImageTagTemplate = StringFunction @ STrim @  """
<video width="#width" height="#height" autoplay loop muted playsinline>
  <source src="#url" type="video/mp4">
</video>
""";

genericImageLinkTemplate = StringFunction @ "![](#url)";
