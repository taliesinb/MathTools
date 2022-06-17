PublicFunction[GraphTheme]

SetUsage @ "
GraphTheme is an extended option to Graph that controls multiple options simultanously via named themes.
* See $GraphThemeData for named sets of options.
"

(**************************************************************************************************)

PublicFunction[LookupGraphThemeOptions]

Graph::badtheme = "`` is not a valid GraphTheme."

LookupGraphThemeOptions[graph_] := Scope[
  theme = LookupAnnotation[graph, GraphTheme, None];
  If[ListQ @ theme, Flatten, Identity] @
    Lookup[$GraphThemeData, theme, Message[Graph::badtheme, theme]; {}]
];

PublicFunction[LookupExtendedThemedOption]

LookupExtendedThemedOption[graph_, keys_List] :=
  MapThread[
    If[#1 === $Failed, #2, #1]&,
    {
      AnnotationValue[graph, keys],
      Lookup[
        Join[LookupGraphThemeOptions @ graph, $extendedGraphOptionsRules],
        keys
      ]
    }
  ];

LookupExtendedThemedOption[graph_, key_] :=
  LookupAnnotation[graph, key,
    Lookup[LookupGraphThemeOptions @ graph, key,
      Lookup[$extendedGraphOptionsRules, key]]];

(**************************************************************************************************)

PublicFunction[LookupThemedOption]

LookupThemedOption[graph_, opt_, default_:Automatic] :=
  Quiet @ Lookup[
    Join[
      Options @ graph,
      LookupGraphThemeOptions @ graph
    ],
    opt, default
  ];
