plainMD[cells___] := ToMarkdownString[Notebook[{cells}]];
hugoMD[cells___] := ToMarkdownString[Notebook[{cells}], MarkdownFlavor -> "Hugo"];

$row = Cell["1\t2\t3","Item"];

$header = Cell["foo\tbar\tbaz","Item"];
plainMD[$header, $row]
hugoMD[$header, $row]

$header = Cell[TextData[{
 StyleBox["foo", FontWeight->"Bold"], "\t",
 StyleBox["bar", FontWeight->"Bold"], "\t",
 StyleBox["baz", FontWeight->"Bold"]}], "Item"];
plainMD[$header, $row]
hugoMD[$header, $row]
