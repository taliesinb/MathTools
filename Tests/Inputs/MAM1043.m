ToMarkdownString @ Cell[
  TextData[{
      "\[SmallCircle] ",
      ButtonBox[
        "ABC",
        BaseStyle -> "Hyperlink",
        ButtonData -> {URL["https://www.complexityexplorer.org/courses/120-introduction-to-dynamical-systems-and-chaos"], None},
        ButtonNote -> "https://www.complexityexplorer.org/courses/120-introduction-to-dynamical-systems-and-chaos"
      ],
      "\[SmallCircle]",
      ButtonBox[
        "DEF",
        BaseStyle -> "Hyperlink",
        ButtonData -> {URL["https://www.complexityexplorer.org/courses/120-introduction-to-dynamical-systems-and-chaos"], None},
        ButtonNote -> "https://www.complexityexplorer.org/courses/120-introduction-to-dynamical-systems-and-chaos"
      ],
      Cell[BoxData[FormBox[ButtonBox[RowBox[{RowBox[{"Complexity", " ", "Explorer", " ", "course", " ", "on", " ", "introduction"}], "-", "to", "-", "differential", "-", "equations"}],
        BaseStyle -> "Hyperlink",
        ButtonData -> {URL["https://www.complexityexplorer.org/courses/31-introduction-to-differential-equations/segments/3495?summary"], None},
        ButtonNote -> "https://www.complexityexplorer.org/courses/31-introduction-to-differential-equations/segments/3495?summary"], TraditionalForm]]]
    }],
  "Text"
]

ToMarkdownString @ Cell[
  BoxData[FormBox[
      RowBox[{
          StyleBox[RowBox[{RowBox[{"Y", "'"}], RowBox[{"(", "t", ")"}]}], FontColor -> RGBColor[1, 0, 0]],
          StyleBox["=", FontWeight -> "Plain"],
          RowBox[{StyleBox["X", FontColor -> GrayLevel[0.5]], StyleBox[RowBox[{"(", "t", ")"}], FontColor -> GrayLevel[0.5]], StyleBox[" ", FontWeight -> "Bold", FontColor -> GrayLevel[0.5]], StyleBox["   ", FontWeight -> "Bold"]}]
        }],
      TraditionalForm
    ]],
  "Equation"
]

(* image embedded badly as line of a text cell *)
ToMarkdownString[Cell[TextData[{
 "\n",
 Cell[BoxData[
 GraphicsBox[
  TagBox[RasterBox[
    RawArray["Real32",{{0.8477095365524292, 0.6358175277709961, 0.8734040856361389}, {
     0.2688387930393219, 0.909667432308197, 0.32857123017311096`}, {0.36220523715019226`,
     0.34044092893600464`, 0.08431245386600494}}], {{0, 3.}, {3., 0}}, {0., 1.},
    ColorFunction->GrayLevel],
   BoxForm`ImageTag["Real32", ColorSpace -> Automatic, Interleaving -> None],
   Selectable->False],
  DefaultBaseStyle->"ImageGraphics",
  ImageSizeRaw->{3., 3.},
  PlotRange->{{0, 3.}, {0, 3.}}]], "Input"]}], "Text"],
  RasterizationFunction -> None
]

(* inappropriately tabbed block *)
ToMarkdownString @ Cell[
  TextData[{
      "The scientific method often goes something like this:\n\n\t1.  ",
      StyleBox[
        "Find",
        FontColor -> RGBColor[1, 0.5, 0]
      ],
      " some phenomenon that you want to understand.\n\t2.  ",
      StyleBox[
        "See",
        FontColor -> RGBColor[1, 0.5, 0]
      ],
      " if you can translate that phenomenon into the language of mathematics.\n\t3.  ",
      StyleBox[
        "Study",
        FontColor -> RGBColor[1, 0.5, 0]
      ],
      " the mathematics to see if you can make predictions.\n\t4.  ",
      StyleBox[
        "Test",
        FontColor -> RGBColor[1, 0.5, 0]
      ],
      " those predictions in the real world.\n\t5.  If the predictions turn out to be true, ",
      StyleBox[
        "make some more",
        FontColor -> RGBColor[1, 0.5, 0]
      ],
      " until you break your model.\n\t6.  When the predictions turn out not to be true, ",
      StyleBox[
        "change your model",
        FontColor -> RGBColor[1, 0.5, 0]
      ],
      " until it matches the world.\n\t\nFoo"
    }],
  "Text"
]