htmlFilePath[hash_] := DataPath["HTML", hash <> ".html"];

(**************************************************************************************************)

PublicIOFunction[ImportHTMLToMarkdown]

CacheVariable[$XMLFromURLCache]

ImportHTMLToMarkdown[url_Str] := Scope[
  xml = CachedInto[$XMLFromURLCache, url, getXMLfromURL @ url];
  If[H[xml] =!= XMLObject["Document"], ReturnFailed[]];
  host = URLParse[url, "Domain"];
  Switch[host,
    "www.quantamagazine.org",
      title = DeepFirstCase[xml, XMLElement["meta", {"property" -> "og:title", "content" -> str_Str}, _] :> str];
      title = StringTrimRight[title, " | Quanta Magazine"];
      abstract = DeepFirstCase[xml, XMLElement["meta", {"property" -> "og:description", "content" -> str_Str}, _] :> str];
      content = StringJoin["# Quanta: \"", title, "\"\n\n", "#doc/article in #pub/Quanta\n\n", url, "\n\n> ", abstract],
    _,
      ReturnFailed[];
  ];
  content
];

getXMLfromURL[url_Str] := Scope[
  hash = Base36Hash @ url;
  htmlPath = htmlFilePath[hash];
  If[!FileExistsQ[htmlPath],
    If[FailureQ[SafeURLDownload[url, htmlPath]],
      ReturnFailed[]
    ];
  ];
  Import[htmlPath, "XMLObject"]
];

