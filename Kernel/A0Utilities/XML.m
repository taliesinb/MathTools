PublicFunction[XMLToText]

$docElements = "html" | "body";
$textDecorators = "a" | "span" | "div" | "h1" | "h2" | "h3" | "h4" | "h5" | "h6" | "emph" | "b";
$nullElements = "head" | "form" | "button" | "img" | "script" | "noscript" | "style";

XMLToText = Case[
  XMLObject["Document"][_, e_, _]    := %[e];
  XMLElement[$docElements, _, e_]    := %[e];
  XMLElement["p", _, e_]             := %[e] <> "\n\n";
  XMLElement["tr", _, e_]            := %[e] <> "\n";
  XMLElement[$textDecorators, _, e_] := %[e];
  XMLElement[$nullElements, _, _]    := "";
  list_List                          := SRiffle[% /@ list, " "];
  s_Str                              := s;
  {}                                 := " ";
];