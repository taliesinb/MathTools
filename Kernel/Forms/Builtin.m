DefineKatexDisplayFunction["DirectedEdge", TBox[##, "TaggedDirectedEdgeForm"]&];
DefineKatexDisplayFunction["UndirectedEdge", TBox[##, "TaggedUndirectedEdgeForm"]&];
DefineKatexDisplayFunction["Superscript", SuperscriptBox[#1, #2]&];
DefineKatexDisplayFunction["Subscript", SubscriptBox[#1, #2]&];
DefineKatexDisplayFunction["Subsuperscript", SubscriptBox[SubscriptBox[#1, #2], #3]&];
