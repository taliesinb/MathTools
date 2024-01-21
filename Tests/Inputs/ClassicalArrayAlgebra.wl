(* ::Package:: *)

LoadShortcuts["Categories"];


(* ::Title:: *)
(*Classical array algebra*)


(* ::Section:: *)
(*Introduction*)


(* ::Subsection:: *)
(*Array programming*)


(* ::Subsection:: *)
(*Abstract nonsense*)


(* ::Section:: *)
(*Basic theory*)


(* ::Subsection:: *)
(*Core concepts*)


insetCubeSpec = Sequence[
	FontSize -> 40, FontColor -> GrayLevel[0,1], CubeGap->0.2,
	CubeOpacity -> 0.75, FlipAxes -> {1,2,3},
	Displacement -> "Screen"
];
peopleVector = {3, 0, 4};
peopleArr = {{1,2,2},{3,0,4}};
people3Arr = {{{1,2}, {2, 1}, {2, 0}},{{3,2}, {0, 3}, {4, 1}}};
TestRaster @ SpacedColumn[
NeutralGraphics3D[AssembleGraphics3D[
	XYStackR[
		InsetCubeGrid[List @ List @ List @ 3, CubeStyle -> EdgeForm[None], insetCubeSpec], 
		InsetCubeGrid[List @ Map[List] @ peopleVector, insetCubeSpec],
		InsetCubeGrid[ArrayReshape[peopleArr, {2, 3, 1}], insetCubeSpec],
		InsetCubeGrid[people3Arr, insetCubeSpec], Text[""],
		Spacing -> 2
	]],
	ImageSize -> 520
],
SpacedRow["scalar  ", "vector       ", "   matrix         ", "  3-array", BaseStyle -> {FontSize -> 16, FontColor -> $DarkGray, FontFamily -> "Avenir"}, Spacings -> 60],
SpacedRow["   \[LeftAngleBracket]\[RightAngleBracket]", "    \[LeftAngleBracket]3\[RightAngleBracket]  ", "        \[LeftAngleBracket]3,2\[RightAngleBracket]  ", "       \[LeftAngleBracket]3,2,2\[RightAngleBracket]", BaseStyle -> {FontSize -> 16, FontColor -> $DarkGray, FontFamily -> "Avenir"}, Spacings -> 80],
Alignment -> Left
]


(* ::Subsection:: *)
(*Terminology*)


TestMarkdown @ Cell[
  BoxData @ GridBox[
      {
        {Cell[TextData[StyleBox["array", FontWeight -> "Bold"]], "Text"], Cell[TextData[{"a collection of ", StyleBox["cells", FontWeight -> "Bold"]}], "Text"]},
        {
          Cell[TextData[StyleBox["cell", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[{"a slot in an array\nlabeled by a ", StyleBox["key", FontWeight -> "Bold"], "\nfilled with a ", StyleBox["value", FontWeight -> "Bold"]}], "Text"]
        },
        {Cell[TextData[StyleBox["key", FontWeight -> "Bold"]], "Text"], Cell[TextData[{"position of a cell in an array\na tuple of ", StyleBox["parts", FontWeight -> "Bold"]}], "Text"]},
        {Cell[TextData[StyleBox["axis", FontWeight -> "Bold"]], "Text"], Cell["a label for a part\nan integer in `1..n`", "Text"]},
        {Cell[TextData[StyleBox["value", FontWeight -> "Bold"]], "Text"], Cell["the contents of a cell", "Text"]}
      },
      BaseStyle -> "Text",
      FrameStyle -> GrayLevel[0.9],
      GridBoxAlignment -> {"Columns" -> {{Left}}},
      GridBoxDividers -> {"Columns" -> {{True}}, "Rows" -> {{True}}},
      GridBoxSpacings -> {"Rows" -> {{1}}, "Columns" -> {{2}}}
    ],
  "Text"
]


(* ::Subsection:: *)
(*Notation*)


TestMarkdown @ StringRow[{"M = ", NAF[{{1,2,3}, {4,5, 6}}, "NormalGrid", Frame -> "[]"]}]


(* ::Subsubsection:: *)
(*Colour coding*)


TestMarkdown @ StringRow[{"M = ", NAF[{{1,2,3}, {4,5, 6}}, "NormalGrid" -> {1,2}, Frame -> "[]"], "    ", Column[{Color1Form @ "axis 1", Color2Form @ "axis 2"}]}]


(* ::Subsubsection:: *)
(*Framing*)


TestMarkdown @ StringRow[{"M = ",
	NAF[{{1,2,3}, {4, 5, 6}}, Frame -> "[]"], "  ", Column[{RedForm @ "axis 1", GreenForm @ "axis 2"}];""
}]


(* ::Subsubsection:: *)
(*Part labels *)


TestMarkdown @ FrameLabeled[NAF[{{1,2,3}, {4, 5, 6}}, Frame -> "[]"], {Top -> {3 -> 1, 5 -> 2, 7 -> 3}, Left -> {1 -> 1, 2 -> 2}}, LabelSpacing -> 1]


(* ::Subsubsection:: *)
(*Multiple axes*)

$rand432 = RandomSeeded[RandomInteger[1,{4,3,2}], 1];
TestMarkdown @ StringBlockForm[
	Labeled[
		NAF[$rand432,
			"Row" -> {1, 1, 1}, "Grid" -> {{2, 0}, 3},
			Frame -> "[]"],
	 Column[{Color1Form @ "1st axis; size 4", Color2Form @ "2nd axis; size 3", Color3Form @ "3rd axis; size 2"}]
	]
]


(* ::Subsubsection:: *)
(*Half-framing*)


TestMarkdown @ StringBlockForm[
	Labeled[
		NAF[$rand432, "Row" -> 1, "Grid" -> {2, 3}],
	 Column[{Color1Form @ "1st axis; size 4", Color2Form @ "2nd axis; size 3", Color3Form @ "3rd axis; size 2"}]
	]
]


(* ::Subsubsection:: *)
(*Layout*)


TestMarkdown @ StringRow[{NAF[{1,2,3}, "Row" -> 1], " = ", NAF[{1,2,3}, "Column" -> 1]}, RowAlignments -> Center]


a = {{1,2,3},{4,5,6}};
TestMarkdown @ StringBlockForm @ StringForm["`` = `` = `` = ``", 
	NAF[a, "Column" -> 1, "Column" -> 2],
	NAF[a, "Row" -> 1, "Column" -> 2],
	NAF[a],
	NAF[a, "Row" -> 1, "Row" -> 2]
]


(* ::Subsection:: *)
(*Part and key spaces*)


TestMarkdown @ Cell["\:27e8s_1,\[Ellipsis],s_{n}\:27e9 \[Congruent] { (p_1,\[Ellipsis],p_{n}) | p_{i} \[Element] 1..s_{i} }", "PreformattedCode"]


TestMarkdown @ Cell[
  TextData @ {
      "\:27e8",
      ColorNBox["3", 1],
      ",",
      ColorNBox["2", 2],
      "\:27e9 \[Congruent] { (",
      ColorNBox["r", 1],
      ",",
      ColorNBox["c", 2],
      ") | ",
      ColorNBox["r", 1],
      " \[Element] 1..3, ",
      ColorNBox["c", 2],
      " \[Element] 1..2 } \n      = { (",
      ColorNBox["1", 1],
      ",",
      ColorNBox["1", 2],
      "), (",
      ColorNBox["1", 1],
      ",",
      ColorNBox["2", 2],
      "), (",
      ColorNBox["2", 1],
      ",",
      ColorNBox["1", 2],
      "), (",
      ColorNBox["2", 1],
      ",",
      ColorNBox["2", 2],
      "), (",
      ColorNBox["3", 1],
      ",",
      ColorNBox["1", 2],
      "), (",
      ColorNBox["3", 1],
      ",",
      ColorNBox["2", 2],
      ") }"
    },
  "PreformattedCode"
]


TestMarkdown @ Cell["\:27e8\:27e9 = { () }", "PreformattedCode"]


(* ::Subsection:: *)
(*Arrays as functions*)


TestMarkdown @ Cell["A[\[Ellipsis]] \[Congruent] A( (\[Ellipsis]) )", "PreformattedCode"]


(* ::Subsection:: *)
(*Array circuits*)




(* ::Subsection:: *)
(*Table of arrays*)


TestMarkdown @ Cell[
  BoxData @ GridBox[
      {
        {
          Cell[TextData[StyleBox["arity", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["nested form", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["shape", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["example keys\nand values", FontWeight -> "Bold"]], "Text"]
        },
        {Cell["0 (scalar)", "Text"], Cell["`9`", "Text"], Cell["`\[LeftAngleBracket]\[RightAngleBracket]`", "Text"], Cell["`() -> 9`", "Text"]},
        {
          Cell["1 (vector)", "Text"],
          Cell[TextData[{"` ", ColorNBox["[", 1], "9 1 5`"}], "Text"],
          Cell[TextData[{"`\[LeftAngleBracket]", ColorNBox["3", 1], "\[RightAngleBracket]`"}], "Text"],
          Cell[TextData[{"`(", ColorNBox["3", 1], ") -> 5`"}], "Text"]
        },
        {
          Cell["2 (matrix)", "Text"],
          Cell[TextData @ Cell @ BoxData @ FormBox[RBox["NAF", "[", RBox["{", RBox[RBox["{", RBox["9", ",", "1", ",", " ", "5"], "}"], ",", " ", RBox["{", RBox["3", ",", " ", "2", ",", " ", "6"], "}"]], "}"], "]"], TraditionalForm], "Text"],
          Cell[TextData[{"`\[LeftAngleBracket]", ColorNBox["2", 1], ",", ColorNBox["3", 2], "\[RightAngleBracket]`"}], "Text"],
          Cell[TextData[{"`(", ColorNBox["1", 1], ",", ColorNBox["1", 2], ") -> 9\\n(", ColorNBox["2", 1], ",", ColorNBox["3", 2], ") -> 6`"}], "Text"]
        },
        {
          Cell["3", "Text"],
          Cell[TextData @ Cell @ BoxData @ FormBox[RBox["NAF", "[", RBox["{", RBox[RBox["{", RBox[RBox["{", RBox["\"0 \"", ",", " ", "1"], "}"], ",", " ", RBox["{", RBox["0", ",", " ", "10"], "}"]], "}"], ",", " ", RBox["{", RBox[RBox["{", RBox["2", ",", " ", "3"], "}"], ",", " ", RBox["{", RBox["20", ",", " ", "30"], "}"]], "}"], ",", " ", RBox["{", RBox[RBox["{", RBox["4", ",", " ", "5"], "}"], ",", " ", RBox["{", RBox["40", ",", " ", "50"], "}"]], "}"]], "}"], "]"], TraditionalForm], "Text"],
          Cell[TextData[{"`\[LeftAngleBracket]", ColorNBox["3", 1], ",", ColorNBox["2", 2], ",", ColorNBox["2", 3], "\[RightAngleBracket]`"}], "Text"],
          Cell[TextData @ {"`(", ColorNBox["1", 1], ",", ColorNBox["1", 2], ",", ColorNBox["1", 3], ") -> 0\\n(", ColorNBox["2", 1], ",", ColorNBox["1", 2], ",", ColorNBox["1", 3], ") -> 2\\n(", ColorNBox["3", 1], ",", ColorNBox["2", 2], ",", ColorNBox["2", 3], ") -> 50\\n `"}, "Text"]
        },
        {
          Cell["4", "Text"],
          Cell[TextData @ Cell @ BoxData @ FormBox[RBox["NAF", "[", RBox["{", "\[IndentingNewLine]", RBox[RBox["{", RBox[RBox["{", RBox[RBox["{", RBox["0", ",", "0"], "}"], ",", " ", RBox["{", RBox["0", ",", " ", "0"], "}"]], "}"], ",", " ", RBox["{", RBox[RBox["{", RBox["1", ",", "2"], "}"], ",", " ", RBox["{", RBox["2", ",", " ", "1"], "}"]], "}"]], "}"], ",", "\[IndentingNewLine]", RBox["{", RBox[RBox["{", RBox[RBox["{", RBox["3", ",", "4"], "}"], ",", " ", RBox["{", RBox["4", ",", " ", "3"], "}"]], "}"], ",", " ", RBox["{", RBox[RBox["{", RBox["0", ",", "0"], "}"], ",", " ", RBox["{", RBox["0", ",", " ", "0"], "}"]], "}"]], "}"], ",", "\[IndentingNewLine]", RBox["{", RBox[RBox["{", RBox[RBox["{", RBox["0", ",", "0"], "}"], ",", " ", RBox["{", RBox["0", ",", " ", "0"], "}"]], "}"], ",", " ", RBox["{", RBox[RBox["{", RBox["5", ",", "6"], "}"], ",", " ", RBox["{", RBox["6", ",", " ", "5"], "}"]], "}"]], "}"]], "}"], "]"], TraditionalForm], "Text"],
          Cell[TextData[{"`\[LeftAngleBracket]", ColorNBox["3", 1], ",", ColorNBox["2", 2], ",", ColorNBox["2", 3], ",", ColorNBox["2", 4], "\[RightAngleBracket]`"}], "Text"],
          Cell[TextData @ {"`(", ColorNBox["1", 1], ",", ColorNBox["1", 2], ",", ColorNBox["1", 3], ",", ColorNBox["1", 4], ") -> 0\\n(", ColorNBox["2", 1], ",", ColorNBox["1", 2], ",", ColorNBox["1", 3], ",", ColorNBox["2", 4], ") -> 4\\n(", ColorNBox["3", 1], ",", ColorNBox["2", 2], ",", ColorNBox["2", 3], ",", ColorNBox["2", 4], ") -> 5\\n `"}, "Text"]
        }
      },
      BaseStyle -> "Text",
      FrameStyle -> GrayLevel[0.9],
      GridBoxAlignment -> {"Columns" -> {{Left}}},
      GridBoxDividers -> {"Columns" -> {{True}}, "Rows" -> {{True}}},
      GridBoxSpacings -> {"Rows" -> {{1}}, "Columns" -> {{2}}}
    ],
  "Text"
]


(* ::Section:: *)
(*Example arrays*)


(* ::Subsection:: *)
(*Text*)


TestMarkdown @ Cell[
  BoxData @ GridBox[
      {
        {
          Cell[TextData[StyleBox["axis", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["meaning", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["part space", FontWeight -> "Bold"]], "Text"]
        },
        {Cell[TextData[{"`", ColorNBox["1", 1], "`"}], "Text"], Cell["position of symbol in string", "Text"], Cell["`1..N`", "Text"]}
      },
      BaseStyle -> "Text",
      FrameStyle -> GrayLevel[0.9],
      GridBoxAlignment -> {"Columns" -> {{Left}}},
      GridBoxDividers -> {"Columns" -> {{True}}, "Rows" -> {{True}}},
      GridBoxSpacings -> {"Rows" -> {{1}}, "Columns" -> {{2}}}
    ],
  "Text"
]


TestMarkdown @ Cell[TextData[{"S : \[LeftAngleBracket]", ColorNBox["N", 1], "\[RightAngleBracket] -> \[DoubleStruckCapitalU] "}], "PreformattedCode"]


TestMarkdown @ Cell[
  TextData @ {
      "S = ",
      ColorNBox["[", 1],
      BackgroundNBox["h", 7],
      " ",
      BackgroundNBox["e", 7],
      " ",
      BackgroundNBox["l", 7],
      " ",
      BackgroundNBox["l", 7],
      " ",
      BackgroundNBox["o", 7],
      " ",
      BackgroundNBox[" ", 7],
      " ",
      BackgroundNBox["w", 7],
      " ",
      BackgroundNBox["o", 7],
      " ",
      BackgroundNBox["r", 7],
      " ",
      BackgroundNBox["l", 7],
      " ",
      BackgroundNBox["d", 7],
      ColorNBox["]", 1]
    },
  "PreformattedCode"
]


(* ::Subsection:: *)
(*Histograms*)


$titanicAges = Uncompress["
1:eJzt0D8OwiAYh+FqPIO7N+Dj4+8RTJw8goOJk0O9f6xhMkFACi1QBt6HBYbf6fa83o+HYRinM1we
4+u8my5QQGmiYnDZjHJnRUDlj6qA6sjuP2sTB+CAOkA/LBIeiSgBWRCq841uG0pKBzoB0E44WDOskx
S+RURzyFZRjaM3AZKOFagLWglYF2xheF2IVpEpUdnRxcHIqkB26KrgPFgSeHmISGR2VHb0LDhZCoiE
BoMOWBJ4JMKP9KOC0RYE+RcIhvpBB8wPt2GmE2YeSb4AG+Yzad4p8gYv98to"];
TestRaster @ Histogram[
	$titanicAges, {0, 79, 5}, Frame -> True, PlotRangePadding -> 0.3,
	PlotRange -> {Full, {0, 200}}, PerformanceGoal->"Speed", 
	FrameLabel->{"age","count"}
]


TestMarkdown @ Cell[
  BoxData @ GridBox[
      {
        {
          Cell[TextData[StyleBox["axis", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["meaning", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["part space", FontWeight -> "Bold"]], "Text"]
        },
        {Cell[TextData[{"`", ColorNBox["1", 1], "`"}], "Text"], Cell["bin", "Text"], Cell[TextData[{"`", ColorNBox["B ", 1], "= {[l_1,u_1), [l_2,u_2), ...}`"}], "Text"]}
      },
      BaseStyle -> "Text",
      FrameStyle -> GrayLevel[0.9],
      GridBoxAlignment -> {"Columns" -> {{Left}}},
      GridBoxDividers -> {"Columns" -> {{True}}, "Rows" -> {{True}}},
      GridBoxSpacings -> {"Rows" -> {{1}}, "Columns" -> {{2}}}
    ],
  "Text"
]


TestMarkdown @ Cell[TextData[{"H : \[LeftAngleBracket]", ColorNBox["B", 1], "\[RightAngleBracket] -> \[DoubleStruckCapitalN]"}], "PreformattedCode"]


$titanicBinCounts = {51,31,25,118,183,161,130,102,69,66,43,27,27,5,6};
TestMarkdown @ SBF @ StringForm["H = ``", NAF[$titanicBinCounts, Frame -> "[]"]]


(* ::Subsection:: *)
(*Discrete probability distributions*)


TestMarkdown @ Cell[
  BoxData @ GridBox[
      {
        {
          Cell[TextData[StyleBox["axis", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["meaning", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["part space", FontWeight -> "Bold"]], "Text"]
        },
        {Cell[TextData[{"`", ColorNBox["1", 1], "`"}], "Text"], Cell["symbol from a fixed alphabet", "Text"], Cell["`A = {a,b,c,d,\[Ellipsis]}`", "Text"]}
      },
      BaseStyle -> "Text",
      FrameStyle -> GrayLevel[0.9],
      GridBoxAlignment -> {"Columns" -> {{Left}}},
      GridBoxDividers -> {"Columns" -> {{True}}, "Rows" -> {{True}}},
      GridBoxSpacings -> {"Rows" -> {{1}}, "Columns" -> {{2}}}
    ],
  "Text"
]


TestMarkdown @ Cell[TextData[{"P : \[LeftAngleBracket]", ColorNBox["A", 1], "\[RightAngleBracket] -> \[DoubleStruckOne]"}], "PreformattedCode"]


TestMarkdown @ SBF @ StringForm["P = ", FrameLabeled[
	NAF[StringDrop[TextString[#], 1]& /@ ReplacePart[N @ Zeros[26], {1 -> .6, 5 -> .2, 6 -> .2}]],
	{Top -> RuleThread[Range[26]*3, Characters@$LowercaseRomanLetters]}
], Alignment -> Bottom]


(* ::Subsection:: *)
(*Contingency tables*)


TestMarkdown @ Cell[
  BoxData @ GridBox[
      {
        {
          Cell[TextData[StyleBox["axis", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["name", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["meaning", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["part space", FontWeight -> "Bold"]], "Text"]
        },
        {Cell[TextData[ColorNBox["1", 1]], "Text"], Cell[TextData[{"`", ColorNBox["T", 1], "`"}], "Text"], Cell["treatment", "Text"], Cell["`{none,  medicine 1, medicine 2}`", "Text"]},
        {Cell[TextData[ColorNBox["2", 2]], "Text"], Cell[TextData[{"`", ColorNBox["O", 2], "`"}], "Text"], Cell["outcome", "Text"], Cell["`{recovered, ill}`", "Text"]}
      },
      BaseStyle -> "Text",
      FrameStyle -> GrayLevel[0.9],
      GridBoxAlignment -> {"Columns" -> {{Left}}},
      GridBoxDividers -> {"Columns" -> {{True}}, "Rows" -> {{True}}},
      GridBoxSpacings -> {"Rows" -> {{1}}, "Columns" -> {{2}}}
    ],
  "Text"
]


TestMarkdown @ Cell[TextData[{"C : \[LeftAngleBracket]", ColorNBox["T", 1], ",", ColorNBox["O", 2], "\[RightAngleBracket] -> \[DoubleStruckCapitalN]"}], "PreformattedCode"]


TestMarkdown @ SBF @ StringForm[
	"C = ``",
	FrameLabeled[NAF[{{10, 28, 13},{40, 22, 37}}, "Grid" -> {{1,1}, {2,1,2}}], {Top -> {5 -> "no",  9-> "m1", 13 -> "m2"}, Right -> {1 -> "recovered", 2 -> "ill"}}],
	Alignment -> Center
]


(* ::Subsection:: *)
(*Grayscale images *)


TestMarkdown @ Cell[
  BoxData @ GridBox[
      {
        {
          Cell[TextData[StyleBox["axis", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["name", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["meaning", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["part space", FontWeight -> "Bold"]], "Text"]
        },
        {
          Cell[TextData[ColorNBox["1", 5]], "Text"],
          Cell[TextData[{"`", ColorNBox["Y", 5], "`"}], "Text"],
          Cell["vertical position of a pixel", "Text"],
          Cell[TextData[{"`1..", ColorNBox["H", 5], "`"}], "Text"]
        },
        {
          Cell[TextData[ColorNBox["2", 6]], "Text"],
          Cell[TextData[{"`", ColorNBox["X", 6], "`"}], "Text"],
          Cell["horizontal position of a pixel", "Text"],
          Cell[TextData[{"`1..", ColorNBox["W", 6], "`"}], "Text"]
        }
      },
      BaseStyle -> "Text",
      FrameStyle -> GrayLevel[0.9],
      GridBoxAlignment -> {"Columns" -> {{Left}}},
      GridBoxDividers -> {"Columns" -> {{True}}, "Rows" -> {{True}}},
      GridBoxSpacings -> {"Rows" -> {{1}}, "Columns" -> {{2}}}
    ],
  "Text"
]


TestMarkdown @ Cell[TextData[{"I : \[LeftAngleBracket]", ColorNBox["H", 5], ",", ColorNBox["W", 6], "\[RightAngleBracket] -> \[DoubleStruckOne]"}], "PreformattedCode"]


$grayF = 1 - {{1,1,1}, {1,0,0}, {1,1,0}, {1,0,0}, {1,0,0}};
TestRaster @ MeshImage[N @ $grayF, 30]


TestMarkdown @ SBF @ StringForm["I = ``", NAF[$grayF, "Grid" -> {5, 6}], Alignment -> Center]


(* ::Subsection:: *)
(*Colour images *)


TestMarkdown @ Cell[
  TextData @ {
      "     ",
      StyleBox["r g b", FontColor -> GrayLevel[0.5]],
      StyleBox["        r g b        r g b      ", FontColor -> GrayLevel[0.5]],
      "\n\[FilledSquare] = [0 0 0   ",
      StyleBox["\[FilledSquare]", FontColor -> RGBColor[1, 0, 0]],
      " = [1 0 0   ",
      StyleBox["\[FilledSquare]", FontColor -> RGBColor[1, 0.5, 0]],
      " = [1 1 0     \n",
      StyleBox["\[FilledSquare]", FontColor -> GrayLevel[0.5]],
      " = [\.bd \.bd \.bd   ",
      StyleBox["\[FilledSquare]", FontColor -> RGBColor[0, 1, 0]],
      " = [0 1 0   ",
      StyleBox["\[FilledSquare]", FontColor -> RGBColor[0, 1, 1]],
      " = [0 1 1  \n\[EmptySquare] = [1 1 1   ",
      StyleBox["\[FilledSquare]", FontColor -> RGBColor[0, 0, 1]],
      " = [0 0 1   ",
      StyleBox["\[FilledSquare]", FontColor -> RGBColor[0.5, 0, 0.5]],
      " = [1 0 1"
    },
  "PreformattedCode"
]


TestMarkdown @ Cell[
  BoxData @ GridBox[
      {
        {
          Cell[TextData[StyleBox["axis", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["name", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["meaning", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["part space", FontWeight -> "Bold"]], "Text"]
        },
        {
          Cell[TextData[ColorNBox["1", 8]], "Text"],
          Cell[TextData[{"`", ColorNBox["C", 8], "`"}], "Text"],
          Cell["colour channel of a subpixel", "Text"],
          Cell["`{R,G,B}` or `{1,2,3}`", "Text"]
        },
        {
          Cell[TextData[ColorNBox["2", 5]], "Text"],
          Cell[TextData[{"`", ColorNBox["Y", 5], "`"}], "Text"],
          Cell["vertical position of a subpixel", "Text"],
          Cell[TextData[{"`1..", ColorNBox["H", 5], "`"}], "Text"]
        },
        {
          Cell[TextData[ColorNBox["3", 6]], "Text"],
          Cell[TextData[{"`", ColorNBox["X", 6], "`"}], "Text"],
          Cell["horizontal position of a subpixel", "Text"],
          Cell[TextData[{"`1..", ColorNBox["W", 6], "`"}], "Text"]
        }
      },
      BaseStyle -> "Text",
      FrameStyle -> GrayLevel[0.9],
      GridBoxAlignment -> {"Columns" -> {{Left}}},
      GridBoxDividers -> {"Columns" -> {{True}}, "Rows" -> {{True}}},
      GridBoxSpacings -> {"Rows" -> {{1}}, "Columns" -> {{2}}}
    ],
  "Text"
]


TestMarkdown @ Cell[TextData[{"I : \[LeftAngleBracket]", ColorNBox["3", 8], ",", ColorNBox["H", 5], ",", ColorNBox["W", 6], "> -> \[DoubleStruckOne]"}], "PreformattedCode"]


fShape = RotateLeft /@ {{0,1,1,1}, {0,1,0,0}, {0,1,1,0}, {0,1,0,0}, {0,1,0,0}};
colonShape = {{0,0,0,0}, {0,0,0,0}, {0,0,0,1}, {0,0,0,0}, {0,0,0,1}};
fData = Transpose[{fShape, 2 * colonShape, 3 * colonShape}, {3, 1, 2}];
TestRaster @ MeshImage[N@fData, 30]


$imgArrayOpts = Sequence[
	CubeGap -> 0.15,
	FrameStyle -> Directive[AbsoluteThickness[3], GrayLevel[0.2,.5]], CubeStyle -> EdgeForm @ AbsoluteThickness[1.5],
	ColorRules -> {1 -> $Red, 2 -> $Green, 3 -> $Blue, 4 -> $Red},
	FlipAxes -> {1, 2, 3},
	Ticks -> {Style[Right, $Pink, Bold], Style[TopRight, $DarkTeal, Bold], Style[TopLeft, $DarkPurple, Bold]},
	TickSpacing -> .3, FlipTicks -> {1,2,3}, TicksStyle -> 25
];
TestRaster @ NeutralGraphics3D @ ColoredCubeGrid[fData,  $imgArrayOpts]


fData2 = Map[If[# === 0, LightGrayForm["0"], "1"]&, fData, {3}];
TestMarkdown @ SBF @ StringForm["I = ``", 
	FrameLabeled[NAF[Transpose[fData2, {2,3,1}], "Row" -> 8, "Grid" -> {5, 6}], Top -> {7 -> "r", 17 -> "g", 27 -> "b"}, LabelStyle -> 8],
	Alignment -> Center]


TestMarkdown @ SBF @ StringForm["I = ``",
	FrameLabeled[NAF[fData2, "Grid" -> {5, 6}, "Row" -> 8], Top -> {4 -> "r", 6 -> "g", 8 -> "b", 11 -> "r", 13 -> "g", 15 -> "b", 18 -> "r", 20 -> "g", 22 -> "b", 25 -> "r", 27 -> "g", 29 -> "b"}, LabelStyle -> 8],
	Alignment -> Center
]


TestMarkdown @ Cell[
  TextData @ {
      "     ",
      ColorNBox["r", 8],
      " ",
      ColorNBox["g", 8],
      " ",
      ColorNBox["b", 8],
      StyleBox["\n\[FilledSquare]", FontColor -> RGBColor[1, 0, 0]],
      " = ",
      ColorNBox["[", 8],
      "1 ",
      StyleBox["0", FontColor -> $LightGray],
      " ",
      StyleBox["0", FontColor -> $LightGray],
      "\n\[FilledSquare] = ",
      ColorNBox["[", 8],
      StyleBox["0", FontColor -> $LightGray],
      " ",
      StyleBox["0", FontColor -> $LightGray],
      " ",
      StyleBox["0", FontColor -> $LightGray],
      "\n",
      StyleBox["\[FilledSquare]", FontColor -> RGBColor[0, 1, 1]],
      " = ",
      ColorNBox["[", 8],
      StyleBox["0", FontColor -> $LightGray],
      " 1 1"
    },
  "PreformattedCode"
]


TestMarkdown @ NAF[Map[PixelForm,fData,{2}], "Grid" -> {5, 6}]


(* ::Subsection:: *)
(*Voxel images*)


(*WithInternet[$kneeRaw = ExampleData[{"TestImage3D","MRknee"}]];*)


TestMarkdown @ Cell[TextData[{"M = \[LeftAngleBracket]", ColorNBox["D", 2], ",", ColorNBox["H", 5], ",", ColorNBox["W", 6], "\[RightAngleBracket] -> \[DoubleStruckCapitalR]"}], "PreformattedCode"]


TestMarkdown @ Cell[
  BoxData @ GridBox[
      {
        {
          Cell[TextData[StyleBox["axis", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["name", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["meaning", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["part space", FontWeight -> "Bold"]], "Text"]
        },
        {
          Cell[TextData[ColorNBox["1", 2]], "Text"],
          Cell[TextData[{"`", ColorNBox["Z", 2], "`"}], "Text"],
          Cell["lateral position of a voxel ", "Text"],
          Cell[TextData[{"`1..", ColorNBox["D", 2], "`"}], "Text"]
        },
        {
          Cell[TextData[ColorNBox["2", 5]], "Text"],
          Cell[TextData[{"`", ColorNBox["Y", 5], "`"}], "Text"],
          Cell["vertical position of a voxel", "Text"],
          Cell[TextData[{"`1..", ColorNBox["H", 5], "`"}], "Text"]
        },
        {
          Cell[TextData[ColorNBox["3", 6]], "Text"],
          Cell[TextData[{"`", ColorNBox["X", 6], "`"}], "Text"],
          Cell["horizontal position of a voxel", "Text"],
          Cell[TextData[{"`1..", ColorNBox["W", 6], "`"}], "Text"]
        }
      },
      BaseStyle -> "Text",
      FrameStyle -> GrayLevel[0.9],
      GridBoxAlignment -> {"Columns" -> {{Left}}},
      GridBoxDividers -> {"Columns" -> {{True}}, "Rows" -> {{True}}},
      GridBoxSpacings -> {"Rows" -> {{1}}, "Columns" -> {{2}}}
    ],
  "Text"
]


shape3D = {
	{{1, 1, 1}, {1, 0, 0}, {1, 0, 1}},
	{{0, 0, 1}, {0, 0, 0}, {1, 0, 0}},
	{{0, 0, 1}, {0, 0, 1}, {1, 1, 1}}};
TestRaster @ ArrayPlot3D[shape3D, ColorRules -> {0 -> Transparent, 1 -> $LightGray}, ImageSize -> 200]


TestMarkdown @ SBF @ StringForm["M = ``",
NAF[Map[FadedRationalForm, shape3D, {3}], "Row" -> 2, "Grid" -> {5, 6}],
	Alignment -> Center]


(* ::Subsection:: *)
(*Vector fields*)


vecField = Table[({y-1.5,1.5-x})2,{x,0,3},{y,0,3}];
pts = Catenate @ Table[{i,j}, {i,4}, {j, 4}];
$windVecStyle = Sequence[
	ArrowheadShape -> "FilledTriangle",
	ArrowColor -> $Gray,
	ArrowheadPosition -> Offset[0.05, 1],
	ArrowThickness -> 2,
	ArrowheadLength -> 0.1
];
vecFieldPlot[array_, sz_] := Graphics[
	Style[
		MapIndexed[vecFieldElem[sz], array, {2}],
		$windVecStyle
	],
	Frame -> True,
	FrameTicks -> {{1,2,3,4}, {{1, "4"}, {2, "3"}, {3, "2"}, {4, "1"}}}
];
vecFieldElem[sz_][vec_, pos_] := {ExtendedArrow[VectorCurve[pos, sz * vec]], Disk[pos, .04]};
TestRaster @ vecFieldPlot[vecField,0.15]


$labelTextStyle = Sequence[BaseStyle -> {FontFamily -> "Fira Code", FontWeight -> "Medium", FontSize -> 11}];
signStr[0] := " 0";
signStr[n_?Positive] := "+" <> IntegerString[n];
signStr[n_] := n;
TestRaster @ Graphics[
	Style[{
		vecFieldElem[1][{3,3}, {0,0}], Text["[+3 -3]", {3,3}, {0.2, -1.1}, $labelTextStyle],
		vecFieldElem[1][{1,-3}, {0,0}], Text["[+1 +3]", {1,-3}, {0, 1.1}, $labelTextStyle],
		(*vecFieldElem[1][{-1,-1}, {0,0}], Text["[-1 +1]", {-1.5,-1.6}, $labelTextStyle],*)
		Disk[{0, 0}, .2]},
		$windVecStyle, ArrowheadLength -> .4],
	PlotRange -> {{-3, 3}, {-3, 3}},
	ImageSize -> Small, GridLines -> {Range[-3,3], Range[-3,3]}, GridLinesStyle-> $LightGray,
	PlotRangeClipping-> True, Frame -> True, PlotRangePadding -> 1,
	FrameTicks -> {{None, Map[{-#, signStr @ #}&, Range[-3,3]]}, {Map[{#, signStr[#]}&, Range[-3,3]], None}}
]


TestMarkdown @ Cell[
  BoxData @ GridBox[
      {
        {
          Cell[TextData[StyleBox["axis", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["meaning", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["part space", FontWeight -> "Bold"]], "Text"]
        },
        {Cell[TextData[{"`", ColorNBox["1", 1], "`"}], "Text"], Cell["longitude of a grid point", "Text"], Cell[TextData[{"`1..", ColorNBox["4", 1], "`"}], "Text"]},
        {Cell[TextData[{"`", ColorNBox["2", 2], "`"}], "Text"], Cell["lattitude of a grid point", "Text"], Cell[TextData[{"`1..", ColorNBox["4", 2], "`"}], "Text"]},
        {Cell[TextData[{"`", ColorNBox["3", 3], "`"}], "Text"], Cell["component of the wind vector", "Text"], Cell["`{x,y}`", "Text"]}
      },
      BaseStyle -> "Text",
      FrameStyle -> GrayLevel[0.9],
      GridBoxAlignment -> {"Columns" -> {{Left}}},
      GridBoxDividers -> {"Columns" -> {{True}}, "Rows" -> {{True}}},
      GridBoxSpacings -> {"Rows" -> {{1}}, "Columns" -> {{2}}}
    ],
  "Text"
]


TestMarkdown @ Cell[TextData[{"W : \[LeftAngleBracket]", ColorNBox["4", 1], ",", ColorNBox["4", 2], ",", ColorNBox["2", 3], "\[RightAngleBracket] -> \[DoubleStruckCapitalR]"}], "PreformattedCode"]


TestMarkdown @ SBF @ StringForm["W = ``", NAF[Map[If[# > 0, "+", "-"] <> IntegerString @ Round[Abs @ #]&, vecField, {3}], "Grid" -> {1, 2}, "Row" -> 3],
	Alignment -> 2]


(* ::Subsection:: *)
(*Graphs*)


graph = ExtendedGraph[{"a" -> "b", "b" -> "c", "b" -> "c", "b" -> "d", "d" -> "e", "e" -> "d"},
	VertexLabels -> Automatic, ArrowheadSize -> 20, ImageSize -> 250, EdgeThickness -> 2, 
	VertexLabelStyle -> {FontSize ->15, FontFamily -> "Fira Code", Background -> None},
	PlotRangePadding -> .15];
TestRaster @ graph


TestMarkdown @ Cell[
  BoxData @ GridBox[
      {
        {
          Cell[TextData[StyleBox["axis", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["meaning", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["part space", FontWeight -> "Bold"]], "Text"]
        },
        {Cell[TextData[{"`", ColorNBox["1", 1], "`"}], "Text"], Cell["source vertex of edge", "Text"], Cell["`V`", "Text"]},
        {Cell[TextData[{"`", ColorNBox["2", 2], "`"}], "Text"], Cell["target vertex of edge", "Text"], Cell["`V`", "Text"]}
      },
      BaseStyle -> "Text",
      FrameStyle -> GrayLevel[0.9],
      GridBoxAlignment -> {"Columns" -> {{Left}}},
      GridBoxDividers -> {"Columns" -> {{True}}, "Rows" -> {{True}}},
      GridBoxSpacings -> {"Rows" -> {{1}}, "Columns" -> {{2}}}
    ],
  "Text"
]


TestMarkdown @ Cell[TextData[{"A : \[LeftAngleBracket]", ColorNBox["V", 1], ",", ColorNBox["V", 2], "\[RightAngleBracket] -> \[DoubleStruckCapitalN]"}], "PreformattedCode"]


TestMarkdown @ SBF @ StringForm["A = ``", FrameLabeled[
		NAF[MatrixMap[FadedRationalForm] @ Normal @ AdjacencyMatrix @ graph],
		{Left -> Style[1;;5 -> "abcde", $Red], Top -> Style[3;;;;2 -> "abcde", $Blue]}
	], Alignment -> 4]


(* ::Subsection:: *)
(*Hypergraphs*)


Null;


{p1, p2, p3} = CirclePoints[3]; p4 = p1 + p1 - p3; dx = {0.1, 0};
TestRaster @ Graphics[{
	ArrowheadLength -> .15, ArrowheadThickness -> 0, ArrowheadColor -> $Gray, ArrowPathSetback -> 0.12,
	ArrowThickness -> 2, ArrowheadPosition -> Offset[.03, 1], ArrowColor -> $Gray,
	ArrowheadShape -> "Arrow",
	ExtendedArrow[RollingCurve[{p1, 0.98 p2, p3}, ArrowPathSetback -> {0.1, 0}]],
	ExtendedArrow[RollingCurve[{0.9 p2 + dx/2, p3 + dx, p1}], ArrowPathSetback -> 0.1],
	ExtendedArrow[RollingCurve[{p4, p1+ dx*1.5, p2 + dx*1.3}], ArrowPathSetback -> 0.1],
	Disk[#, .03]& /@ {p1, p2, p3, p4},
	Text[#1, #2, #3, BaseStyle -> {FontFamily -> "Fira Code", FontSize -> 15}]& @@@ {
		{"3", p1, {0, 1.5}},
		{"1", p2, {0, -1.5}},
		{"2", p3, {0, 1.5}},
		{"4", p4, {0, 1.5}}
	}	
}, ImageSize -> 250]


TestMarkdown @ Cell[
  BoxData @ GridBox[
      {
        {
          Cell[TextData[StyleBox["axis", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["meaning", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["part space", FontWeight -> "Bold"]], "Text"]
        },
        {Cell[TextData[{"`", ColorNBox["1", 1], "`"}], "Text"], Cell["source vertex of hyperedge", "Text"], Cell["`V`", "Text"]},
        {Cell[TextData[{"`", ColorNBox["2", 2], "`"}], "Text"], Cell["middle vertex of hyperedge", "Text"], Cell["`V`", "Text"]},
        {Cell[TextData[{"`", ColorNBox["3", 3], "`"}], "Text"], Cell["target vertex of hyperedge", "Text"], Cell["`V`", "Text"]}
      },
      BaseStyle -> "Text",
      FrameStyle -> GrayLevel[0.9],
      GridBoxAlignment -> {"Columns" -> {{Left}}},
      GridBoxDividers -> {"Columns" -> {{True}}, "Rows" -> {{True}}},
      GridBoxSpacings -> {"Rows" -> {{1}}, "Columns" -> {{2}}}
    ],
  "Text"
]


TestMarkdown @ Cell[TextData[{"A : \[LeftAngleBracket]", ColorNBox["V", 1], ",", ColorNBox["V", 2], ",", ColorNBox["V", 3], "\[RightAngleBracket] -> \[DoubleStruckCapitalN]"}], "PreformattedCode"]


adjArray3 = Normal@ExtendedSparseArray[{{1,2,3},{3,1,2},{4,3,1}},{4,4,4}];
TestMarkdown @ SBF @ StringForm["A = ``", NAF[Map[FadedRationalForm, adjArray3, {3}], "Row" -> 1, "Grid" -> {2, 3}], Alignment -> 2]


(* ::Section:: *)
(*Primitive operations*)


(* ::Subsection:: *)
(*Introduction*)


TestMarkdown @ Cell[
  BoxData @ GridBox[
      {
        {
          Cell[TextData[StyleBox["name", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["#", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["description", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["example", FontWeight -> "Bold"]], "Text"]
        },
        {Cell[TextData[StyleBox["lift", FontWeight -> "Bold"]], "Text"], Cell["$n", "Text"], Cell["apply fn to matching cells", "Text"], Cell["`A +^{\[UpArrow]} B`", "Text"]},
        {Cell[TextData[StyleBox["aggregate", FontWeight -> "Bold"]], "Text"], Cell["1", "Text"], Cell["use a fn to summarize an axis", "Text"], Cell["`sum_2(A)`", "Text"]},
        {Cell[TextData[StyleBox["merge", FontWeight -> "Bold"]], "Text"], Cell["1", "Text"], Cell["merge multiple parts of an axis together", "Text"], Cell["`sum^{R}_2(A)`", "Text"]},
        {Cell[TextData[StyleBox["broadcast", FontWeight -> "Bold"]], "Text"], Cell["1", "Text"], Cell["introduce constant novel axis", "Text"], Cell["`A^{2\:279c}`", "Text"]},
        {Cell[TextData[StyleBox["transpose", FontWeight -> "Bold"]], "Text"], Cell["1", "Text"], Cell["permute the order of the axes", "Text"], Cell["`A^{\[Sigma]}`", "Text"]},
        {
          Cell[TextData[{StyleBox["nest", FontWeight -> "Bold"], "\n", StyleBox["unnest", FontWeight -> "Bold"]}], "Text"],
          Cell["1", "Text"],
          Cell["turn an array into nested arrays\nturn nested arrays into one array", "Text"],
          Cell["`A^{2\[Succeeds]}\nA^{2\[Precedes]}`", "Text"]
        },
        {Cell[TextData[StyleBox["diagonal", FontWeight -> "Bold"]], "Text"], Cell["1", "Text"], Cell["obtain a diagonal along an array", "Text"], Cell["`M^{1:(1,2)}`", "Text"]},
        {Cell[TextData[StyleBox["pick", FontWeight -> "Bold"]], "Text"], Cell["2", "Text"], Cell["pick cells by their keys", "Text"], Cell["`A[P]`", "Text"]}
      },
      BaseStyle -> "Text",
      FrameStyle -> GrayLevel[0.9],
      GridBoxAlignment -> {"Columns" -> {{Left}}},
      GridBoxDividers -> {"Columns" -> {{True}}, "Rows" -> {{True}}},
      GridBoxSpacings -> {"Rows" -> {{1}}, "Columns" -> {{2}}}
    ],
  "Text"
]


(* ::Subsection:: *)
(*Cellwise definitions*)


TestMarkdown @ Cell[
  TextData @ {
      " ",
      ColorNBox["M", 4],
      "[i,j] \[Congruent] ",
      ColorNBox["V", 6],
      "[i] + ",
      ColorNBox["W", 6],
      "[j]\n ",
      ColorNBox["M", 4],
      "[i,j] \[Congruent] ",
      ColorNBox["S", 6],
      "[]\n   ",
      ColorNBox["V", 4],
      "[i] \[Congruent] ",
      ColorNBox["V", 6],
      "[i] * ",
      ColorNBox["M", 6],
      "[i]\n    ",
      ColorNBox["S", 4],
      "[] \[Congruent] sum_{i\[Element]1..N}{ ",
      ColorNBox["V", 6],
      "[i] }"
    },
  "PreformattedCode"
]


(* ::Subsection:: *)
(*Colourful notation*)


TestMarkdown @ Cell["P[i,j] \[Congruent] A[i] + B[j]", "PreformattedCode"]


TestMarkdown @ Cell[TextData[{"P[", ColorNBox["i", 1], ",", ColorNBox["j", 2], "] \[Congruent] A[", ColorNBox["i", 1], "] + B[", ColorNBox["j", 2], "]"}], "PreformattedCode"]


TestMarkdown @ StringBlockForm @ StringForm["`` = `` + ``", NAF[{{1,2}, {3,4}, {5,6}}], NAF[{1,3,5}, "Column" -> 1], NAF[{0, 1}, "Row" -> 2], Alignment -> Center]


TestMarkdown @ Cell[TextData[{"P[", ColorNBox["\[FilledCircle]", 1], ",", ColorNBox["\[FilledCircle]", 2], "] \[Congruent] A[", ColorNBox["\[FilledCircle]", 1], "] + B[", ColorNBox["\[FilledCircle]", 2], "]"}], "PreformattedCode"]


TestMarkdown @ Cell[
  TextData @ {
      "P : \[LeftAngleBracket]",
      ColorNBox["\[FilledSquare]", 1],
      ",",
      ColorNBox["\[FilledSquare]", 2],
      "\[RightAngleBracket]\nA : \[LeftAngleBracket]",
      ColorNBox["\[FilledSquare]", 1],
      "\[RightAngleBracket]\nB : \[LeftAngleBracket]",
      ColorNBox["\[FilledSquare]", 2],
      "\[RightAngleBracket]\n\nP[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["\[FilledCircle]", 2],
      "] \[Congruent] A[",
      ColorNBox["\[FilledCircle]", 1],
      "] + B[",
      ColorNBox["\[FilledCircle]", 2],
      "]"
    },
  "PreformattedCode"
]


TestMarkdown @ Cell[
  TextData @ {
      "P : \[LeftAngleBracket]",
      ColorNBox["m", 1],
      ",",
      ColorNBox["n", 2],
      "\[RightAngleBracket]\nA : \[LeftAngleBracket]",
      ColorNBox["m", 1],
      "\[RightAngleBracket]\nB : \[LeftAngleBracket]",
      ColorNBox["n", 2],
      "\[RightAngleBracket]\n\nP[",
      ColorNBox["i", 1],
      ",",
      ColorNBox["j", 2],
      "] \[Congruent] A[",
      ColorNBox["i", 1],
      "] + B[",
      ColorNBox["j", 2],
      "]"
    },
  "PreformattedCode"
]


(* ::Subsection:: *)
(*Lift op*)


TestMarkdown @ Cell["f : \[DoubleStruckCapitalV]^{n} -> \[DoubleStruckCapitalV]", "PreformattedCode"]


TestMarkdown @ Cell["f^{\[UpArrow]} : (\[DoubleStruckCapitalK]->\[DoubleStruckCapitalV])^{n} -> (\[DoubleStruckCapitalK]->\[DoubleStruckCapitalV])", "PreformattedCode"]


TestMarkdown @ Cell[
  TextData @ {
      "^{ }f(A,B,\[Ellipsis]) : \[LeftAngleBracket]",
      ColorNBox["\[FilledSquare]", 1],
      ",",
      ColorNBox["\[FilledSquare]", 2],
      ",",
      ColorNBox["\[FilledSquare]", 3],
      ",\[Ellipsis]\[RightAngleBracket] -> \[DoubleStruckCapitalV]\nf^{\[UpArrow]}(A,B,\[Ellipsis]) : \[LeftAngleBracket]",
      ColorNBox["\[FilledSquare]", 1],
      ",",
      ColorNBox["\[FilledSquare]", 2],
      ",",
      ColorNBox["\[FilledSquare]", 3],
      ",\[Ellipsis]\[RightAngleBracket] -> \[DoubleStruckCapitalV]\n\nf^{\[UpArrow]}(A,B,\[Ellipsis])[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["\[FilledCircle]", 2],
      ",",
      ColorNBox["\[FilledCircle]", 3],
      ",\[Ellipsis]] = f(A[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["\[FilledCircle]", 2],
      ",",
      ColorNBox["\[FilledCircle]", 3],
      ",\[Ellipsis]], B[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["\[FilledCircle]", 2],
      ",",
      ColorNBox["\[FilledCircle]", 3],
      ",\[Ellipsis]], \[Ellipsis])"
    },
  "PreformattedCode"
]


(* ::Subsubsection:: *)
(*Array circuit*)


(* ::Subsubsection:: *)
(*Examples*)


TestMarkdown @ Cell[TextData[{"(A +^{\[UpArrow]} B)[", ColorNBox["\[FilledCircle]", 1], "] \[Congruent] A[", ColorNBox["\[FilledCircle]", 1], "] + B[", ColorNBox["\[FilledCircle]", 1], "]\n\n\[VerticalEllipsis]"}], "PreformattedCode"]


a1 = {1,2, 3}; a2 = {0, 3, 5};
TestMarkdown @ SBF @ StringForm["A = ``   B = ``   A \!\(\*SuperscriptBox[\(+\), \(\[UpArrow]\)]\) B = ``", NAF[a1, "Column" -> 1], NAF[a2, "Column" -> 1], NAF[a1 + a2, "Column" -> 1], Alignment -> Center]


TestMarkdown @ Cell[
  TextData[{"(A \[And]^{\[UpArrow]} B)[", ColorNBox["\[FilledCircle]", 1], ",", ColorNBox["\[FilledCircle]", 2], "] \[Congruent] A[", ColorNBox["\[FilledCircle]", 1], ",", ColorNBox["\[FilledCircle]", 2], "] \[And] B[", ColorNBox["\[FilledCircle]", 1], ",", ColorNBox["\[FilledCircle]", 2], "]\n\n\[VerticalEllipsis]"}],
  "PreformattedCode"
]


m1 = {{False, True}, {False, True}};
m2 = {{False, False}, {True, True}};
m3 = {{False, False}, {False, True}};
TestMarkdown @ SBF @ StringForm["A = ``   B = ``   A \!\(\*SuperscriptBox[\(\[And]\), \(\[UpArrow]\)]\) B = ``", NAF[m1], NAF[m2], NAF[m3]]


TestMarkdown @ Cell[
  TextData[{"(\[Not]^{\[UpArrow]}A)[", ColorNBox["\[FilledCircle]", 1], ",", ColorNBox["\[FilledCircle]", 2], ",", ColorNBox["\[FilledCircle]", 3], "] \[Congruent] \[Not](A[", ColorNBox["\[FilledCircle]", 1], ",", ColorNBox["\[FilledCircle]", 2], ",", ColorNBox["\[FilledCircle]", 3], "])\n\n\[VerticalEllipsis]"}],
  "PreformattedCode"
]


m4 = RandomSeeded[RandomChoice[{False, True}, {2,2,3}], 1];
TestMarkdown @ SBF @ StringForm["A = ``   \!\(\*SuperscriptBox[\(\[Not]\), \(\[UpArrow]\)]\)A = ``", NAF[m4, "Column" -> 1, "Grid" -> {2,3}], NAF[Map[Not, m4, {3}]]]


(* ::Subsubsection:: *)
(*Algebra*)


TestMarkdown @ Cell[
  BoxData @ GridBox[
      {
        {
          Cell[TextData[StyleBox["value space", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["algebraic structure", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["operations that can be lifted", FontWeight -> "Bold"]], "Text"]
        },
        {Cell["`\[DoubleStruckCapitalR]`", "Text"], Cell["field", "Text"], Cell["addition (`+`), multiplication (`*`)\nsubtraction (`-`), division (`/`)\nnegation (`-\[Square]`), inverse (`\[Square]^{ -1}`)", "Text"]},
        {Cell["`\[DoubleStruckCapitalN], \[DoubleStruckCapitalR], \[DoubleStruckCapitalZ]` ", "Text"], Cell["semiring", "Text"], Cell["addition (`+`), multiplication (`*`)", "Text"]},
        {Cell["`\[DoubleStruckCapitalB]`", "Text"], Cell["semiring", "Text"], Cell["and (`\[And]`), or (`\[Or]`), xor (`\[Xor]`), not (`\[Not]`)", "Text"]},
        {Cell["`\[DoubleStruckCapitalP](X)`", "Text"], Cell["lattice", "Text"], Cell["meet (`\[And]`), join (`\[Or]`) ", "Text"]}
      },
      BaseStyle -> "Text",
      FrameStyle -> GrayLevel[0.9],
      GridBoxAlignment -> {"Columns" -> {{Left}}},
      GridBoxDividers -> {"Columns" -> {{True}}, "Rows" -> {{True}}},
      GridBoxSpacings -> {"Rows" -> {{1}}, "Columns" -> {{2}}}
    ],
  "Text"
]


(* ::Subsubsection:: *)
(*Technicalities*)


TestMarkdown @ Cell["f : (\[DoubleStruckCapitalV]_1,\[DoubleStruckCapitalV]_2,...,\[DoubleStruckCapitalV]_{n}) -> \[DoubleStruckCapitalW]", "PreformattedCode"]


TestMarkdown @ Cell["f^{\[UpArrow]} : (\[DoubleStruckCapitalK]->\[DoubleStruckCapitalV]_1, \[DoubleStruckCapitalK]->\[DoubleStruckCapitalV]_2,..., \[DoubleStruckCapitalK]->\[DoubleStruckCapitalV]_{n}) -> (\[DoubleStruckCapitalK]->\[DoubleStruckCapitalW])", "PreformattedCode"]


TestMarkdown @ Cell["div : (\[DoubleStruckCapitalZ], \[DoubleStruckCapitalN]^{+}) -> \[DoubleStruckCapitalQ] \ndiv(r, n) \[Congruent] r / n", "PreformattedCode"]


TestMarkdown @ SBF @ StringForm["```` = ``", Superscript["div", "\[UpArrow]"], TupleForm[NAF[{1,2,3}, "Column" -> 1], NAF[{3,2,1}, "Column" -> 1]], NAF[{"1/3",1,3}, "Column" -> 1], Alignment -> Center]


(* ::Subsection:: *)
(*Aggregate op*)


TestMarkdown @ Cell["agg : \[DoubleStruckCapitalV]^{*} -> \[DoubleStruckCapitalV]", "PreformattedCode"]


TestMarkdown @ Cell[
  BoxData @ GridBox[
      {
        {
          Cell[TextData[StyleBox["value space", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["algebraic structure", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["aggregations", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["example", FontWeight -> "Bold"]], "Text"]
        },
        {Cell["`\[DoubleStruckCapitalR]`", "Text"], Cell["field", "Text"], Cell["mean", "Text"], Cell["`mean{ 1, 2 } = 1.5`", "Text"]},
        {Cell["`\[DoubleStruckCapitalN], \[DoubleStruckCapitalR], \[DoubleStruckCapitalZ]` ", "Text"], Cell["semiring", "Text"], Cell["sum, product, max, min", "Text"], Cell["`sum{ 1, 2, 3 } = 6`", "Text"]},
        {Cell["`\[DoubleStruckCapitalB]`", "Text"], Cell["semiring", "Text"], Cell["and (`\[And]`), or (`\[Or]`), xor (`\[Xor]`)", "Text"], Cell["`or{ F, T } = T`", "Text"]}
      },
      BaseStyle -> "Text",
      FrameStyle -> GrayLevel[0.9],
      GridBoxAlignment -> {"Columns" -> {{Left}}},
      GridBoxDividers -> {"Columns" -> {{True}}, "Rows" -> {{True}}},
      GridBoxSpacings -> {"Rows" -> {{1}}, "Columns" -> {{2}}}
    ],
  "Text"
]


TestMarkdown @ Cell[
  TextData @ {
      "_{ }    A  : \[LeftAngleBracket]\[Ellipsis],",
      ColorNBox["\[FilledSquare]_{n}", 1],
      ",\[Ellipsis]\[RightAngleBracket] -> \[DoubleStruckCapitalV]\nagg_{",
      ColorNBox["n", 1],
      "}(A) : \[LeftAngleBracket]\[Ellipsis]\[RightAngleBracket] -> \[DoubleStruckCapitalV]      \n\nagg_{",
      ColorNBox["n", 1],
      "}(A)[\[Ellipsis]] = agg{ A[\[Ellipsis],",
      ColorNBox["\[FilledCircle]_{n}", 1],
      ",\[Ellipsis]] | ",
      ColorNBox["\[FilledCircle]_{n}", 1],
      " \[Element] ",
      ColorNBox["\[FilledSquare]_{n}", 1],
      " }"
    },
  "PreformattedCode"
]


(* ::Subsubsection:: *)
(*Summing a 3-array*)


TestMarkdown @ Cell[
  TextData @ {
      "sum_{",
      ColorNBox["2", 2],
      "}(H)[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["\[FilledCircle]", 3],
      "] \[Congruent] sum_{",
      ColorNBox["\[FilledCircle]", 2],
      "\[Element]1..",
      ColorNBox["2", 2],
      "}{ H[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["\[FilledCircle]", 2],
      ",",
      ColorNBox["\[FilledCircle]", 3],
      "] } = H[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["1", 2],
      ",",
      ColorNBox["\[FilledCircle]", 3],
      "] + H[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["2", 2],
      ",",
      ColorNBox["\[FilledCircle]", 3],
      "]\n\n\[VerticalEllipsis]"
    },
  "PreformattedCode"
]


highlight1[part___] := MapAt[Background4Form, {{part}}];
highlight2[part___] := MapAt[Background5Form, {{part}}];
arr = {{{1, 1, 0, 2}, {0, 0, 0, 0}}, {{2, 1, 1, 0}, {0, 0, 2, 2}}};
arr2 = Total[arr, {2}];
StringBlockForm[Row[{Subscript["sum", Color2Form @ 2], NAF[highlight1[2, All, 3] @ highlight2[1, All, 1] @ arr, "Column" -> 1, "Grid" -> {2, 3}], " = ",
	NAF[highlight1[2, 3] @ highlight2[1, 1] @ arr2, "Grid" -> {1, 3}]}, Alignment -> Top], StylingFunction -> "Input"];
TestMarkdown @ StringBlockForm @ StringForm["`` `` = ``", Subscript["sum", Color2Form @ 2], NAF[arr, "Column" -> 1, "Grid" -> {2, 3}],
	NAF[arr2, "Column" -> {1, 1, 0}, "Row" -> 3]]


arr = {{{1, 1, 0, 2}, {0, 0, 0, 0}}, {{2, 1, 1, 0}, {0, 0, 2, 2}}};
arr2 = Total[arr, {2}];
TestMarkdown @ StringBlockForm[Column[{
	StringForm["`` `` =  ``", Subscript["sum", Color1Form @ 1], NAF[HighlightBackgroundAt[1, All, 1, 1] @ arr, "Column" -> 1, "Grid" -> {2, 3}],  NAF[HighlightBackgroundAt[1, 1, 1] @ Total[arr, {1}], "Grid" -> {2, 3}]],
	StringForm["`` `` = ``", Subscript["sum", Color2Form @ 2], NAF[HighlightBackgroundAt[2, 1, All, 1] @ arr, "Column" -> 1, "Grid" -> {2, 3}], NAF[HighlightBackgroundAt[2, 1, 1] @ Total[arr, {2}], "Grid" -> {{1, 1}, 3}]],
	StringForm["`` `` = ``", Subscript["sum", Color3Form @ 3], NAF[HighlightBackgroundAt[3, 1, 1, All] @ arr, "Column" -> 1, "Grid" -> {2, 3}], NAF[HighlightBackgroundAt[3, 1, 1] @ Total[arr, {3}], "Column" -> 1, "Column" -> {2,1}]]
},
Spacings -> 1
]]


Clear[plotCube]; SetHoldFirst[plotCube];
plotCube[body_, {m_, n_, p_}, col_, sz_, opts___Rule] := NeutralGraphics3D[
	{ColoredCubeGrid[Boole @ Table[body,{r,m},{b,n},{g,p}], ColorRules -> {1 -> col}, opts,
	Ticks -> {Style[Right, $Green], Style[TopRight, $Blue], Style[TopLeft, $Red]},
	FrameStyle -> Directive[AbsoluteThickness[2], GrayLevel[0.2,.5]],
	MeshStyle -> Directive[AbsoluteThickness[2], GrayLevel[0.5,0.1]], $imgArrayOpts], Transparent, Point[{-1,0,0}]},
	ImageSize -> sz * 0.9]; 
TestRaster @ SpacedColumn[
	SpacedRow[
		plotCube[g == b == 1, {2, 2, 4}, $Red, 150],
		Pane @ Labeled[MakeArrow[50, 10, 2, $Gray], SubscriptForm["sum", Color1Form @ 1]],
		plotCube[g == b == 1, {1, 2, 4}, $Red, 115, Ticks -> {Style[Right, $Green], Style[TopRight, $Blue], None}],
		Spacings -> 30
	],
	SpacedRow[
		plotCube[r == g == 1, {2, 2, 4}, $Blue, 150],
		Pane @ Labeled[MakeArrow[50, 10, 2, $Gray], SubscriptForm["sum", Color2Form @ 2]],
		plotCube[r == g == 1, {2, 1, 4}, $Blue, 115, Ticks -> {Style[Right, $Green], None, Style[TopLeft, $Red]}],
		Spacings -> 30
	],
	SpacedRow[
		plotCube[r == b == 1, {2, 2, 4}, $Green, 150],
		Pane @ Labeled[MakeArrow[50, 10, 2, $Gray], SubscriptForm["sum", Color3Form @ 3]],
		plotCube[r == b == 1, {2, 2, 1}, $Green, 140, Ticks -> {None, Style[TopRight, $Blue], Style[TopLeft, $Red]}],
		Spacings -> 30
	],
	Spacings -> 10, Alignment -> Left
]


(* ::Subsubsection:: *)
(*Multiple axes*)


arr = {{{1, 1, 0, 2}, {0, 0, 0, 0}}, {{2, 1, 1, 0}, {0, 0, 2, 2}}};
sum23 = SubscriptForm["sum", TightCommaRowForm[Color2Form @ 2, Color3Form @ 3]];
TestMarkdown @ StringBlockForm @ StringForm["`` `` = ``", sum23, 
	NAF[HighlightBackgroundAt[6, 1, All, All] @ arr, "Column" -> 1, "Grid" -> {2, 3}],
	NAF[HighlightBackgroundAt[6, 1] @ Total[arr, {2, 3}], "Column" -> {1,1,2}]]


TestRaster @ SpacedRow[
		plotCube[r == 1, {2, 2, 4}, $Teal, 150],
		Pane @ Labeled[MakeArrow[50, 10, 2, $Gray], sum23],
		plotCube[r == 1, {2, 1, 1}, $Teal, 115, Ticks -> {None, None, Style[TopLeft, $Red]}],
		Spacings -> 30
	]


sum123 = SubscriptForm["sum", TightCommaRowForm[Color1Form @ 1, Color2Form @ 2, Color3Form @ 3]];
TestMarkdown @ StringBlockForm @ StringForm["`` `` = ``", sum123, 
	NAF[HighlightBackgroundAt[7, All, All, All] @ arr, "Column" -> 1, "Grid" -> {2, 3}],
	HighlightBackgroundAt[7] @ Total[Flatten @ arr]]


TestRaster @ SpacedRow[
		plotCube[True, {2, 2, 4}, $Gray, 150],
		Pane @ Labeled[MakeArrow[50, 10, 2, $Gray], sum123],
		NeutralGraphics3D[InsetCubeGrid[List @ List @ List @ "", CubeStyle -> EdgeForm[None], insetCubeSpec], ImageSize -> 50],
		Spacings -> 30
	]


(* ::Subsubsection:: *)
(*Maximum over a matrix*)


TestMarkdown @ Cell[
  TextData @ {
      "max_{",
      ColorNBox["1", 1],
      "}(M)[",
      ColorNBox["\[FilledCircle]", 2],
      "] \[Congruent] max_{",
      ColorNBox["\[FilledCircle]", 1],
      "\[Element]1..",
      ColorNBox["2", 1],
      "}{ M[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["\[FilledCircle]", 2],
      "] } = max(M[",
      ColorNBox["1", 1],
      ",",
      ColorNBox["\[FilledCircle]", 2],
      "], M[",
      ColorNBox["2", 1],
      ",",
      ColorNBox["\[FilledCircle]", 2],
      "])\n\n\[VerticalEllipsis]"
    },
  "PreformattedCode"
]


arr = {{2, 1, 1, 0}, {0, 0, 2, 3}};
TestMarkdown @ StringBlockForm @ StringForm["`` `` = ``",
	SubscriptForm["max", Color1Form @ 1],
	NAF[HighlightBackgroundAt[1, All, 4] @ arr, "Grid" -> {1, 2}],
	NAF[HighlightBackgroundAt[1, 4] @ ArrayReduce[Max, arr, {1}], "Row" -> 2]
]


TestMarkdown @ Cell[
  TextData @ {
      "max_{",
      ColorNBox["2", 2],
      "}(M)[",
      ColorNBox["\[FilledCircle]", 1],
      "] \[Congruent] max_{",
      ColorNBox["\[FilledCircle]", 2],
      "\[Element]1..",
      ColorNBox["4", 2],
      "}{ M[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["\[FilledCircle]", 2],
      "] } = max(M[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["1", 2],
      "], M[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["2", 2],
      "], M[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["3", 2],
      "], M[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["4", 2],
      "])\n\n\[VerticalEllipsis]"
    },
  "PreformattedCode"
]


arr = {{2, 1, 1, 0}, {0, 0, 2, 3}};
TestMarkdown @ StringBlockForm @ StringForm["`` `` = ``",
	SubscriptForm["max", Color2Form @ 2],
	NAF[HighlightBackgroundAt[2, 2, All] @ arr, "Grid" -> {1, 2}],
	NAF[HighlightBackgroundAt[2, 2] @ ArrayReduce[Max, arr, {2}], "Column" -> 1]
]


(* ::Subsubsection:: *)
(*Logical \[And] over a vector*)


TestMarkdown @ Cell[
  TextData @ {
      "and_{",
      ColorNBox["1", 1],
      "}(V)[] \[Congruent] and_{",
      ColorNBox["\[FilledCircle]", 1],
      "\[Element]1..",
      ColorNBox["3", 1],
      "}{ V[",
      ColorNBox["\[FilledCircle]", 1],
      "] } = V[",
      ColorNBox["1", 1],
      "] \[And] V[",
      ColorNBox["2", 1],
      "] \[And] V[",
      ColorNBox["3", 1],
      "]\n\n\[VerticalEllipsis]"
    },
  "PreformattedCode"
]


TestMarkdown @ StringBlockForm @ StringForm["`` `` = ``", Subscript["and", Color1Form @ 1], "\!\(\*
StyleBox[\"[\",\nFontColor:>CurrentValue[{StyleDefinitions, \"Color1\", FontColor}]]\)T F F\!\(\*
StyleBox[\"]\",\nFontColor:>CurrentValue[{StyleDefinitions, \"Color1\", FontColor}]]\)", "F"]


(* ::Subsubsection:: *)
(*Applications*)


(* ::Subsubsection:: *)
(*Technicalities*)


TestMarkdown @ Cell[TextData[{"_{ }   agg : \[DoubleStruckCapitalV]^{*} -> \[DoubleStruckCapitalW]\n_{ }    A  : \[LeftAngleBracket]\[Ellipsis],", ColorNBox["\[FilledSquare]_{n}", 1], ",\[Ellipsis]\[RightAngleBracket] -> \[DoubleStruckCapitalV]\nagg_{", ColorNBox["n", 1], "}(A) : \[LeftAngleBracket]\[Ellipsis]\[RightAngleBracket] -> \[DoubleStruckCapitalW]      "}], "PreformattedCode"]


(* ::Subsubsection:: *)
(*Array circuit*)


TestMarkdown @ Cell["x : \[DoubleStruckCapitalX]\nY : \[DoubleStruckCapitalY]^{*}\nY = mset{ y | y \[Element] \[DoubleStruckCapitalY] }", "PreformattedCode"]


TestMarkdown @ Cell["z : \[DoubleStruckCapitalV]^{*}\nz = mset{ M[x,y] | y \[Element] \[DoubleStruckCapitalY] }", "PreformattedCode"]


TestMarkdown @ Cell["v : \[DoubleStruckCapitalV]\nv = sum( z ) \n  = sum( mset{ M[x,y] | y \[Element] \[DoubleStruckCapitalY] } )\n  = sum{ M[x,y] | y \[Element] \[DoubleStruckCapitalY] }", "PreformattedCode"]


(* ::Subsection:: *)
(*Multiset sugar*)



(* ::Subsection:: *)
(*Merging op*)


TestMarkdown @ Cell[
  TextData @ {
      "  R \[Subset] ",
      ColorNBox["\[FilledSquare]", 1],
      " * ",
      ColorNBox["\[FilledSquare]", 2],
      "\nagg : \[DoubleStruckCapitalV]^{*} -> \[DoubleStruckCapitalV]\n\n^{ }_{ }    A  : \[LeftAngleBracket]\[Ellipsis],",
      ColorNBox["\[FilledSquare]_{n}", 1],
      ",\[Ellipsis]\[RightAngleBracket] -> \[DoubleStruckCapitalV]\nagg^{R}_{n}(A) : \[LeftAngleBracket]\[Ellipsis],",
      ColorNBox["\[FilledSquare]_{n}", 2],
      ",\[Ellipsis]\[RightAngleBracket] -> \[DoubleStruckCapitalV]"
    },
  "PreformattedCode"
]


TestMarkdown @ Cell[TextData[{"agg^{R}_{n}(A)[\[Ellipsis],", ColorNBox["\[FilledCircle]_{n}", 2], ",\[Ellipsis]] \[Congruent] agg{ A[\[Ellipsis],", ColorNBox["\[FilledCircle]_{n}", 1], ",\[Ellipsis]] | R(", ColorNBox["\[FilledCircle]", 1], ",", ColorNBox["\[FilledCircle]", 2], ") } "}], "PreformattedCode"]


TestMarkdown @ SBF @ Column[{
	StringForm["V = ``", FrameLabeled[NAF[{1, 2, 3}, Frame -> "[]"], Top -> 2;;;;2 -> "abc"], Alignment -> Bottom],
	"",
	"W = \!\(\*SubscriptBox[SuperscriptBox[\(sum\), \(R\)], 
StyleBox[\"1\",\nFontColor:>CurrentValue[{StyleDefinitions, \"Color1\", FontColor}]]]\)(V) "
}]


TestMarkdown @ Cell[
  TextData @ {
      "                          ",
      ColorNBox["x y", 7],
      "\nR = {}               W = ",
      ColorNBox["[", 2],
      "0 0\nR = {",
      ColorNBox["a", 1],
      "\[RightTeeArrow]",
      ColorNBox["x", 2],
      "}            W = ",
      ColorNBox["[", 2],
      "1 0\nR = {",
      ColorNBox["a", 1],
      "\[RightTeeArrow]",
      ColorNBox["x", 2],
      ",",
      ColorNBox["b", 1],
      "\[RightTeeArrow]",
      ColorNBox["x", 2],
      "}        W = ",
      ColorNBox["[", 2],
      "3 0\nR = {",
      ColorNBox["a", 1],
      "\[RightTeeArrow]",
      ColorNBox["x", 2],
      ",",
      ColorNBox["a", 1],
      "\[RightTeeArrow]",
      ColorNBox["y", 2],
      "}        W = ",
      ColorNBox["[", 2],
      "1 1\nR = {",
      ColorNBox["a", 1],
      "\[RightTeeArrow]",
      ColorNBox["x", 2],
      ",",
      ColorNBox["b", 1],
      "\[RightTeeArrow]",
      ColorNBox["y", 2],
      ",",
      ColorNBox["c", 1],
      "\[RightTeeArrow]",
      ColorNBox["y", 2],
      "}    W = ",
      ColorNBox["[", 2],
      "1 5"
    },
  "PreformattedCode"
]


(* ::Subsubsection:: *)
(*Applications*)


$titanicBinCounts = {51,31,25,118,183,161,130,102,69,66,43,27,27,5,6};
TestMarkdown @ SBF @ StringForm["H = ``", NAF[$titanicBinCounts]]


TestRaster @ Histogram[
	$titanicAges, {0, 79, 5}, Frame -> True, PlotRangePadding -> 0.3,
	PlotRange -> {Full, {0, 200}}, PerformanceGoal->"Speed", 
	FrameLabel->{"age","count"}, ImageSize -> 250
]


TestRaster @ g = ExtendedGraph[
	Range[21],
	Map[#1 -> 17 + Round[(#-2) / 3]&, Range[1,15]],
	VertexCoordinates -> Join[Table[{0, -x}, {x, 16}], Table[{3, -(x*3-1)}, {x, 1, 5}]],
	ImageSize -> 150, (*Epilog -> (Text[Style[#1, FontFamily -> "Fira Code", FontSize -> 15], {0, #2}, {0,0.1}]& @@@ {"A" -> 0, "B" -> -3}),
	ImagePadding -> {Left -> 50, Bottom -> 10}*)
	VertexColorRules -> RuleThread[Range[21], Join[Table[$Red, 16], Table[$Blue, 5]]],
	VertexLabels -> Join[
		Table[Row[{i,"-", i+4}, BaselinePosition-> 1], {i, 0, 74, 5}],
		Table[Row[{i,"-", i+19}, BaselinePosition-> 1], {i, 0, 80, 15}]
	],
	VertexLabelPosition -> "XSign", VertexLabelSpacing -> 3
]


$titanicBinCounts = {51,31,25,118,183,161,130,102,69,66,43,27,27,5,6};
TestMarkdown @ SBF @ StringForm["H' = \!\(\*SubscriptBox[SuperscriptBox[\(sum\), \(R\)], 
StyleBox[\"1\",\nFontColor:>CurrentValue[{StyleDefinitions, \"Color1\", FontColor}]]]\)(H) = ``", NAF[Map[Total, Partition[$titanicBinCounts, 3]]]]


TestRaster @ Histogram[
	$titanicAges, {0, 79, 15}, Frame -> True, PlotRangePadding -> 0.3,
	PlotRange -> {Full, {0, 500}}, PerformanceGoal->"Speed", 
	FrameLabel->{"age","count"},
	ImageSize -> 250
]


(* ::Subsubsection:: *)
(*Array circuit*)



TestMarkdown @ Cell["x : \[DoubleStruckCapitalX]\nY : \[DoubleStruckCapitalY]^{*}\n\nY = mset{ y | R(x,y), y \[Element] \[DoubleStruckCapitalY] }", "PreformattedCode"]


TestMarkdown @ Cell["z : \[DoubleStruckCapitalV]^{*}\nz = mset{ M[x,y] | R(x,y), y \[Element] \[DoubleStruckCapitalY] }", "PreformattedCode"]


TestMarkdown @ Cell["v = sum( z ) \n  = sum( mset{ M[x,y] | R(x,y), y \[Element] \[DoubleStruckCapitalY] } )\n  = sum{ M[x,y] | R(x,y), y \[Element] \[DoubleStruckCapitalY] }", "PreformattedCode"]


(* ::Subsection:: *)
(*Broadcast op*)


TestMarkdown @ SBF @ StringForm["`` |-> ``", NAF[{1,2,3}, "Column" -> 1], NAF[{{1, 1}, {2, 2}, {3, 3}}, "Grid" -> {1, 2}], Alignment -> Center]


TestMarkdown @ Cell[TextData[{"M[", ColorNBox["r", 1], ",", ColorNBox["c", 2], "] \[Congruent] V[", ColorNBox["r", 1], "]"}], "PreformattedCode"]


TestMarkdown @ Cell[
  BoxData @ GridBox[
      {
        {Cell["broadcast scalar to vector", "Text"], Cell[TextData[{"`V[", ColorNBox["i", 1], "] \[Congruent] S[]`"}], "Text"]},
        {
          Cell["broadcast vector to matrix", "Text"],
          Cell[TextData @ {"`M[", ColorNBox["r", 1], ",", ColorNBox["c", 2], "] \[Congruent] V[", ColorNBox["r", 1], "]\nM[", ColorNBox["r", 1], ",", ColorNBox["c", 2], "] \[Congruent] V[", ColorNBox["c", 2], "]`"}, "Text"]
        },
        {Cell["broadcast scalar to matrix", "Text"], Cell[TextData[{"`M[", ColorNBox["r", 1], ",", ColorNBox["c", 2], "] \[Congruent] S[]`"}], "Text"]},
        {
          Cell["broadcast matrix to 3-array", "Text"],
          Cell[TextData @ {"`H[", ColorNBox["i", 1], ",", ColorNBox["j", 2], ",", ColorNBox["k", 3], "] \[Congruent] M[", ColorNBox["i", 1], ",", ColorNBox["j", 2], "]\nH[", ColorNBox["i", 1], ",", ColorNBox["j", 2], ",", ColorNBox["k", 3], "] \[Congruent] M[", ColorNBox["j", 2], ",", ColorNBox["k", 3], "]\nH[", ColorNBox["i", 1], ",", ColorNBox["j", 2], ",", ColorNBox["k", 3], "] \[Congruent] M[", ColorNBox["i", 1], ",", ColorNBox["k", 3], "]`"}, "Text"]
        }
      },
      BaseStyle -> "Text",
      FrameStyle -> GrayLevel[0.9],
      GridBoxAlignment -> {"Columns" -> {{Left}}},
      GridBoxDividers -> {"Columns" -> {{True}}, "Rows" -> {{True}}},
      GridBoxSpacings -> {"Rows" -> {{1}}, "Columns" -> {{2}}}
    ],
  "Text"
]


TestMarkdown @ Cell[
  BoxData @ GridBox[
      {
        {
          Cell[TextData[{"`A^{", ColorNBox["1", 4], "->9}`"}], "Text"],
          Cell[TextData[{"`\[LeftAngleBracket]", ColorNBox["9", 4], ",", ColorNBox["1", 1], ",", ColorNBox["2", 2], ",", ColorNBox["3", 3], "\[RightAngleBracket]`"}], "Text"]
        },
        {
          Cell[TextData[{"`A^{", ColorNBox["2", 4], "->9}`"}], "Text"],
          Cell[TextData[{"`\[LeftAngleBracket]", ColorNBox["1", 1], ",", ColorNBox["9", 4], ",", ColorNBox["2", 2], ",", ColorNBox["3", 3], "\[RightAngleBracket]`"}], "Text"]
        },
        {
          Cell[TextData[{"`A^{", ColorNBox["3", 4], "->9}`"}], "Text"],
          Cell[TextData[{"`\[LeftAngleBracket]", ColorNBox["1", 1], ",", ColorNBox["2", 2], ",", ColorNBox["9", 4], ",", ColorNBox["3", 3], "\[RightAngleBracket]`"}], "Text"]
        },
        {
          Cell[TextData[{"`A^{", ColorNBox["4", 4], "->9}`"}], "Text"],
          Cell[TextData[{"`\[LeftAngleBracket]", ColorNBox["1", 1], ",", ColorNBox["2", 2], ",", ColorNBox["3", 3], ",", ColorNBox["9", 4], "\[RightAngleBracket]`"}], "Text"]
        }
      },
      BaseStyle -> "Text",
      FrameStyle -> GrayLevel[0.9],
      GridBoxAlignment -> {"Columns" -> {{Left}}},
      GridBoxDividers -> {"Columns" -> {{True}}, "Rows" -> {{True}}},
      GridBoxSpacings -> {"Rows" -> {{1}}, "Columns" -> {{2}}}
    ],
  "Text"
]


TestMarkdown @ StringBlockForm @ Column[{
	Row[{"\!\(\*SuperscriptBox[\(U\), \(\*
StyleBox[\"1\",\nFontColor:>CurrentValue[{StyleDefinitions, \"Color3\", FontColor}]] -> 2\)]\) = ", NAF[{{1,2,3}, {1,2,3}}, "Grid" -> {3, 1}]}],
	Row[{"\!\(\*SuperscriptBox[\(U\), \(\*
StyleBox[\"2\",\nFontColor:>CurrentValue[{StyleDefinitions, \"Color2\", FontColor}]] -> 3\)]\) = ", NAF[{{1,1,1}, {2,2,2}, {3,3,3}}, "Grid" -> {1, 2}]}]
}, Left, 1]


TestMarkdown @ StringBlockForm @ Column[{
	Row[{"    \!\(\*SuperscriptBox[\(S\), \(\*
StyleBox[\"1\",\nFontColor:>CurrentValue[{StyleDefinitions, \"Color1\", FontColor}]] -> 2\)]\) = ", NAF @ {9, 9}}],
	Row[{"\!\(\*SuperscriptBox[\(S\), \(\*
StyleBox[\"1\",\nFontColor:>CurrentValue[{StyleDefinitions, \"Color1\", FontColor}]] -> 2, \*
StyleBox[\"2\",\nFontColor:>CurrentValue[{StyleDefinitions, \"Color2\", FontColor}]] -> 3\)]\) = ", NAF[{{9, 9, 9}, {9, 9, 9}}]}]
}, Left, 1]


TestMarkdown @ Cell[
  TextData @ {"^{   }A:\[LeftAngleBracket]\[Ellipsis]\[RightAngleBracket] -> \[DoubleStruckCapitalV]\nA^{", ColorNBox["n", 6], "\:279cs}:\[LeftAngleBracket]\[Ellipsis],s_{", ColorNBox["n", 6], "},\[Ellipsis]\[RightAngleBracket] -> \[DoubleStruckCapitalV]\n\nA^{", ColorNBox["n", 6], "\:279cs}[\[Ellipsis],", ColorNBox["\[FilledCircle]", 6], "_{", ColorNBox["n", 6], "},\[Ellipsis]] \[Congruent] A[\[Ellipsis]]"},
  "PreformattedCode"
]


(* ::Subsubsection:: *)
(*Combination with lift*)


$m = {{1, 0}, {0, 1}, {1, 1}}; $v = {1, 2, 3};
TestMarkdown @ StringBlockForm @ StringForm["M = ``   V = ``", NAF[$m], NAF[$v, "Column" -> 1], Alignment -> Center]


TestMarkdown @ Cell[TextData[{"M'[", ColorNBox["r", 1], ",", ColorNBox["c", 2], "] \[Congruent] V[", ColorNBox["r", 1], "] * M[", ColorNBox["r", 1], ",", ColorNBox["c", 2], "]\n\n\[VerticalEllipsis]"}], "PreformattedCode"]


$m2 = $m * $v;
TestMarkdown @ StringBlockForm @ StringForm["`` = ``", Superscript["M'","    "], NAF[$m2], Alignment -> Center]


$v2 = {#, #}& /@ $v;
TestMarkdown @ StringBlockForm @ StringForm["``  = ``", Superscript["V","\!\(\*
StyleBox[\"2\",\nFontColor:>CurrentValue[{StyleDefinitions, \"Color1\", FontColor}]]\)->2"], NAF[$v2], Alignment -> Center]


TestMarkdown @ StringBlockForm @ StringForm["M `` `` = `` `` `` = `` = M'", Superscript["*", "\[UpArrow]"], Superscript["V","\!\(\*
StyleBox[\"2\",\nFontColor:>CurrentValue[{StyleDefinitions, \"Color1\", FontColor}]]\)->2"], NAF[$m], Superscript["*", "\[UpArrow]"], NAF[$v2], NAF[$m2], Alignment -> Center]


(* ::Subsubsection:: *)
(*Array circuit*)




(* ::Subsection:: *)
(*Deriving familiar operations*)


(* ::Subsubsection:: *)
(*Dot product*)


TestMarkdown @ Cell[
  TextData @ {
      "    U : \[LeftAngleBracket]",
      ColorNBox["\[FilledSquare]", 1],
      "\[RightAngleBracket] \n    V : \[LeftAngleBracket]",
      ColorNBox["\[FilledSquare]", 1],
      "\[RightAngleBracket] \nU \[CenterDot] V : \[LeftAngleBracket]\[RightAngleBracket]\n\n(U \[CenterDot] V)[] \[Congruent] sum_{",
      ColorNBox["\[FilledCircle]", 1],
      "\[Element]",
      ColorNBox["\[FilledSquare]", 1],
      "}{ U[",
      ColorNBox["\[FilledCircle]", 1],
      "] * V[",
      ColorNBox["\[FilledCircle]", 1],
      "] }"
    },
  "PreformattedCode"
]


TestMarkdown @ Cell[
  TextData @ {
      "(U \[CenterDot] V)[] \[Congruent] sum_{",
      ColorNBox["\[FilledCircle]", 1],
      "\[Element]",
      ColorNBox["\[FilledSquare]", 1],
      "}{ ^{ }U[",
      ColorNBox["\[FilledCircle]", 1],
      "] * V[",
      ColorNBox["\[FilledCircle]", 1],
      "] }    ",
      StyleBox["given", FontColor -> $Gray],
      "\n          = sum_{",
      ColorNBox["\[FilledCircle]", 1],
      "\[Element]",
      ColorNBox["\[FilledSquare]", 1],
      "}{  (U *^{\[UpArrow]} V)[",
      ColorNBox["\[FilledCircle]", 1],
      "] }    ",
      StyleBox["def of lifted *", FontColor -> $Gray],
      "\n          = sum_{",
      ColorNBox["1", 1],
      "}( U *^{\[UpArrow]} V )[",
      ColorNBox["\[FilledCircle]", 1],
      "]  _{  }     ",
      StyleBox["def of aggregate sum", FontColor -> $Gray],
      "\n          ",
      StyleBox["\[Therefore]", FontColor -> $Gray],
      "\n U \[CenterDot] V    \[Congruent] sum_{",
      ColorNBox["1", 1],
      "}( U *^{\[UpArrow]} V )"
    },
  "PreformattedCode"
]


(* ::Subsubsection:: *)
(*Tensor product*)


TestMarkdown @ Cell[
  TextData @ {
      "    U : \[LeftAngleBracket]",
      ColorNBox["\[FilledSquare]", 1],
      "\[RightAngleBracket] \n    V : \[LeftAngleBracket]",
      ColorNBox["\[FilledSquare]", 2],
      "\[RightAngleBracket] \nU \[CircleTimes] V : \[LeftAngleBracket]",
      ColorNBox["\[FilledSquare]", 1],
      ",",
      ColorNBox["\[FilledSquare]", 2],
      "\[RightAngleBracket]\n\n\n(U \[CircleTimes] V)[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["\[FilledCircle]", 2],
      "] \[Congruent] U[",
      ColorNBox["\[FilledCircle]", 1],
      "] * V[",
      ColorNBox["\[FilledCircle]", 2],
      "]"
    },
  "PreformattedCode"
]


TestMarkdown @ Cell[
  TextData @ {
      "(U \[CircleTimes] V)[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["\[FilledCircle]", 2],
      "] \[Congruent] ^{  }U[",
      ColorNBox["\[FilledCircle]", 1],
      "  ] ^{ }* ^{  }V[  ",
      ColorNBox["\[FilledCircle]", 2],
      "]    ",
      StyleBox["given", FontColor -> $Gray],
      "\n             = U^{",
      ColorNBox["2", 2],
      "\:279c}[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["\[FilledCircle]", 2],
      "] *^{\[UpArrow]} V^{",
      ColorNBox["1", 1],
      "\:279c}[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["\[FilledCircle]", 2],
      "]    ",
      StyleBox["def of broadcast", FontColor -> $Gray],
      "\n             =    (U^{",
      ColorNBox["2", 2],
      "\:279c} *^{\[UpArrow]} V^{",
      ColorNBox["1", 1],
      "\:279c})[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["\[FilledCircle]", 2],
      "]    ",
      StyleBox["def of lifted *", FontColor -> $Gray],
      "\n             \[Therefore]\n U \[CircleTimes] V       \[Congruent] U^{",
      ColorNBox["2", 2],
      "\:279c} * V^{",
      ColorNBox["1", 1],
      "\:279c}"
    },
  "PreformattedCode"
]


(* ::Subsubsection:: *)
(*Matrix multiplication *)


TestMarkdown @ Cell[
  TextData @ {
      "    M : \[LeftAngleBracket]",
      ColorNBox["\[FilledSquare]", 1],
      ",",
      ColorNBox["\[FilledSquare]", 2],
      "\[RightAngleBracket]\n    N : \[LeftAngleBracket]",
      ColorNBox["\[FilledSquare]", 2],
      ",",
      ColorNBox["\[FilledSquare]", 3],
      "\[RightAngleBracket]\nM \:22c5 N : \[LeftAngleBracket]",
      ColorNBox["\[FilledSquare]", 1],
      ",",
      ColorNBox["\[FilledSquare]", 3],
      "\[RightAngleBracket]\n\n\n(M \:22c5 N)[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["\[FilledCircle]", 3],
      "] = \[Sum]_{",
      ColorNBox["\[FilledCircle]", 2],
      "\[Element]",
      ColorNBox["\[FilledSquare]", 2],
      "} M[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["\[FilledCircle]", 2],
      "] * N[",
      ColorNBox["\[FilledCircle]", 2],
      ",",
      ColorNBox["\[FilledCircle]", 3],
      "]"
    },
  "PreformattedCode"
]


TestMarkdown @ Cell[
  TextData @ {
      "(M \:22c5 N)[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["\[FilledCircle]", 3],
      "] \[Congruent] sum_{",
      ColorNBox["\[FilledCircle]", 2],
      "\[Element]",
      ColorNBox["\[FilledSquare]", 2],
      "}{ ^{  }M[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["\[FilledCircle]", 2],
      "  ] * ^{  }N[  ",
      ColorNBox["\[FilledCircle]", 2],
      ",",
      ColorNBox["\[FilledCircle]", 3],
      "] }   ",
      StyleBox["def of \:22c5", FontColor -> $Gray],
      "\n             = sum_{",
      ColorNBox["\[FilledCircle]", 2],
      "\[Element]",
      ColorNBox["\[FilledSquare]", 2],
      "}{ M^{",
      ColorNBox["3", 3],
      "\:279c}[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["\[FilledCircle]", 2],
      ",",
      ColorNBox["\[FilledCircle]", 3],
      "] * ^{",
      ColorNBox["1", 1],
      "\:279c}N[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["\[FilledCircle]", 2],
      ",",
      ColorNBox["\[FilledCircle]", 3],
      "] }   ",
      StyleBox["def of broadcast", FontColor -> $Gray],
      "\n             = sum_{",
      ColorNBox["\[FilledCircle]", 2],
      "\[Element]",
      ColorNBox["\[FilledSquare]", 2],
      "}{ ^{   }  (M^{",
      ColorNBox["3", 3],
      "\:279c} *^{\[UpArrow]} ^{",
      ColorNBox["1", 1],
      "\:279c}N)[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["\[FilledCircle]", 2],
      ",",
      ColorNBox["\[FilledCircle]", 3],
      "] }   ",
      StyleBox["def of lifted *", FontColor -> $Gray],
      "\n             =     ^{     }sum_{",
      ColorNBox["2", 2],
      "}(M^{",
      ColorNBox["3", 3],
      "\:279c} *^{\[UpArrow]} ^{",
      ColorNBox["1", 1],
      "\:279c}N)[",
      ColorNBox["\[FilledCircle]", 1],
      ",  ",
      ColorNBox["\[FilledCircle]", 3],
      "]     ",
      StyleBox["def of aggregate sum", FontColor -> $Gray],
      "\n             \[Therefore]\n M \:22c5 N       \[Congruent] sum_{",
      ColorNBox["2", 2],
      "}(M^{",
      ColorNBox["3", 3],
      "\:279c} * ^{",
      ColorNBox["1", 1],
      "\:279c}N)"
    },
  "PreformattedCode"
]




(* ::Subsection:: *)
(*Transpose op*)


TestMarkdown @ Cell[TextData[{"M\:1d40[", ColorNBox["r", 1], ",", ColorNBox["c", 2], "] \[Congruent] M[", ColorNBox["c", 2], ",", ColorNBox["r", 1], "]"}], "PreformattedCode"]


TestMarkdown @ Cell[TextData[{"I'[", ColorNBox["y", 5], ",", ColorNBox["x", 6], ",", ColorNBox["c", 8], "] \[Congruent] I[", ColorNBox["c", 8], ",", ColorNBox["y", 5], ",", ColorNBox["x", 6], "]"}], "PreformattedCode"]


TestMarkdown @ Cell[
  TextData @ {
      "A^{\[Sigma]}[",
      ColorNBox["\[FilledCircle]", 7],
      "_1,",
      ColorNBox["\[FilledCircle]", 7],
      "_2,\[Ellipsis],",
      ColorNBox["\[FilledCircle]", 7],
      "_{n}] \[Congruent] ^{ }A[",
      ColorNBox["\[FilledCircle]", 7],
      "_{\[Sigma](1)},",
      ColorNBox["\[FilledCircle]", 7],
      "_{\[Sigma](2)},\[Ellipsis],",
      ColorNBox["\[FilledCircle]", 7],
      "_{\[Sigma](n)}] "
    },
  "PreformattedCode"
]


TestMarkdown @ Cell[
  TextData @ {"(", ColorNBox["\[FilledCircle]", 7], "_1,", ColorNBox["\[FilledCircle]", 7], "_2,\[Ellipsis],", ColorNBox["\[FilledCircle]", 7], "_{n}) |-> (", ColorNBox["\[FilledCircle]", 7], "_{\[Sigma](1)},", ColorNBox["\[FilledCircle]", 7], "_{\[Sigma](2)},\[Ellipsis],", ColorNBox["\[FilledCircle]", 7], "_{\[Sigma](n)})"},
  "PreformattedCode"
]


(* ::Subsubsection:: *)
(*Applications*)


TestMarkdown @ Cell[TextData[{"M\:1d40 = M^{(", ColorNBox["1", 1], ",", ColorNBox["2", 2], ")}\nI' = I^{(", ColorNBox["1", 5], ",", ColorNBox["2", 6], ",", ColorNBox["3", 8], ")}"}], "PreformattedCode"]


(* ::Subsubsection:: *)
(*Array circuit*)




(* ::Subsection:: *)
(*Nest op*)


arr = {{1,2}, {3,4}, {5,6}};
TestMarkdown @ StringRow[{NAF[{"x", "y", "z"}, "Column" -> 1, Frame -> "[]"], "   \!\(\*
StyleBox[\"where\",\nFontColor:>CurrentValue[{StyleDefinitions, \"Color7\", FontColor}]]\)   ", 
	StringColumn @ MapThread[StringForm["`` = ``", #1, NAF[#2, "Row" -> 2, Frame -> "[]"]]&, {{"x", "y", "z"}, arr}]}, RowAlignments -> Center]


arr = {{1,2}, {3,4}, {5,6}};
TestMarkdown @ StringRow[{
	NAF[{"a", "b"}, "Row" -> 2, Frame -> "[]"],
	"   \!\(\*
StyleBox[\"where\",\nFontColor:>CurrentValue[{StyleDefinitions, \"Color7\", FontColor}]]\)   ", 
	StringRow[
		MapThread[
			StringForm["`` = ``   ", #1, NAF[#2, "Column" -> 1, Frame -> "[]"], Alignment -> Center]&, {{"a", "b"},
			Transpose @ arr}
		],
		RowAlignments -> Center
	]
	}, RowAlignments -> Center]


TestMarkdown @ Cell[TextData[{"A^{", ColorNBox["n", 6], "\[Succeeds]}[\[Ellipsis]][", ColorNBox["\[FilledCircle]", 6], "] \[Congruent] A[\[Ellipsis],", ColorNBox["\[FilledCircle]", 6], "_{", ColorNBox["n", 6], "},\[Ellipsis]]"}], "PreformattedCode"]


TestMarkdown @ Cell[TextData[{" ^{  }A:\[LeftAngleBracket]\[Ellipsis],", ColorNBox["\[FilledSquare]_{n}", 6], ",\[Ellipsis]\[RightAngleBracket] -> \[DoubleStruckCapitalV]\n A^{", ColorNBox["n", 6], "\[Succeeds]}:\[LeftAngleBracket]\[Ellipsis]\[RightAngleBracket] -> \[LeftAngleBracket]", ColorNBox["\[FilledSquare]", 6], "\[RightAngleBracket] -> \[DoubleStruckCapitalV]"}], "PreformattedCode"]


TestMarkdown @ SBF @ StringForm["M = ``", NAF[{{1,2},{3,4},{5,6}}], Alignment -> Center]


TestMarkdown @ Cell[TextData[{"M^{", ColorNBox["1", 1], "\[Succeeds]}[", ColorNBox["\[FilledCircle]", 2], "][", ColorNBox["\[FilledCircle]", 1], "] \[Congruent] M[", ColorNBox["\[FilledCircle]", 1], ",", ColorNBox["\[FilledCircle]", 2], "] "}], "PreformattedCode"]


TestMarkdown @ Cell[
  TextData @ {
      "M^{",
      ColorNBox["1", 1],
      "\[Succeeds]}[",
      ColorNBox["1", 2],
      "] = M[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["1", 2],
      "] = ",
      ColorNBox["[", 1],
      "1 3 5",
      ColorNBox["]", 1],
      "\nM^{",
      ColorNBox["1", 1],
      "\[Succeeds]}[",
      ColorNBox["2", 2],
      "] = M[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["2", 2],
      "] = ",
      ColorNBox["[", 1],
      "2 4 6",
      ColorNBox["]", 1]
    },
  "PreformattedCode"
]


TestMarkdown @ Cell[TextData[{"M^{", ColorNBox["2", 2], "\[Succeeds]}[", ColorNBox["\[FilledCircle]", 1], "][", ColorNBox["\[FilledCircle]", 2], "] \[Congruent] M[", ColorNBox["\[FilledCircle]", 1], ",", ColorNBox["\[FilledCircle]", 2], "] "}], "PreformattedCode"]


TestMarkdown @ Cell[
  TextData @ {
      "M^{",
      ColorNBox["2", 2],
      "\[Succeeds]}[",
      ColorNBox["1", 1],
      "] = M[",
      ColorNBox["1", 1],
      ",",
      ColorNBox["\[FilledCircle]", 2],
      "] = ",
      ColorNBox["[", 2],
      "1 2",
      ColorNBox["]", 2],
      "\nM^{",
      ColorNBox["2", 2],
      "\[Succeeds]}[",
      ColorNBox["2", 1],
      "] = M[",
      ColorNBox["2", 1],
      ",",
      ColorNBox["\[FilledCircle]", 2],
      "] = ",
      ColorNBox["[", 2],
      "3 4",
      ColorNBox["]", 2],
      "\nM^{",
      ColorNBox["2", 2],
      "\[Succeeds]}[",
      ColorNBox["3", 1],
      "] = M[",
      ColorNBox["3", 1],
      ",",
      ColorNBox["\[FilledCircle]", 2],
      "] = ",
      ColorNBox["[", 2],
      "5 6",
      ColorNBox["]", 2]
    },
  "PreformattedCode"
]


(* ::Subsubsection:: *)
(*Multiple axes*)


TestMarkdown @ Cell[
  TextData @ {
      "A^{",
      ColorNBox["1", 1],
      ",",
      ColorNBox["3", 3],
      "\[Succeeds]}[",
      ColorNBox["\[FilledCircle]", 2],
      "][",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["\[FilledCircle]", 3],
      "] \[Congruent] A[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["\[FilledCircle]", 2],
      ",",
      ColorNBox["\[FilledCircle]", 3],
      "]"
    },
  "PreformattedCode"
]


(* ::Subsubsection:: *)
(*Correspondence with nested notation*)


arr = {{{1, 1, 0, 2}, {0, 0, 0, 0}}, {{2, 1, 1, 0}, {0, 0, 2, 2}}};
TestMarkdown @ NAF[arr]


TestMarkdown @ NAF[arr, "Column" -> 1, "Column" -> {2, 1}, "LeftParen", "Row" -> {3, 0, 1}]


TestMarkdown @ Cell[
  TextData @ {
      "H : \[LeftAngleBracket]",
      ColorNBox["2", 1],
      ",",
      ColorNBox["2", 2],
      ",",
      ColorNBox["4", 3],
      "\[RightAngleBracket] -> \[DoubleStruckCapitalZ]   <==>   H^{",
      ColorNBox["3", 3],
      "\[Succeeds]} : \[LeftAngleBracket]",
      ColorNBox["2", 1],
      ",",
      ColorNBox["2", 2],
      "\[RightAngleBracket] -> \[LeftAngleBracket]",
      ColorNBox["4", 3],
      "\[RightAngleBracket] -> \[DoubleStruckCapitalZ]"
    },
  "PreformattedCode"
]


TestMarkdown @ NAF[arr, "Column" -> {1, 1, 1}, "LeftParen", "Column" -> {2, 0, 0, None}, "Row" -> {3, 0, 1, 0}]


TestMarkdown @ Cell[
  TextData @ {
      "H : \[LeftAngleBracket]",
      ColorNBox["2", 1],
      ",",
      ColorNBox["2", 2],
      ",",
      ColorNBox["4", 3],
      "\[RightAngleBracket] -> \[DoubleStruckCapitalZ]   <==>   H^{",
      ColorNBox["2", 2],
      ",",
      ColorNBox["3", 3],
      "\[Succeeds]} : \[LeftAngleBracket]",
      ColorNBox["2", 1],
      "\[RightAngleBracket] -> \[LeftAngleBracket]",
      ColorNBox["2", 2],
      ",",
      ColorNBox["4", 3],
      "\[RightAngleBracket] -> \[DoubleStruckCapitalZ]"
    },
  "PreformattedCode"
]


TestMarkdown @ NAF[arr, "LeftParen", "Column" -> 1, "Grid" -> {2, 3}]


TestMarkdown @ Cell[
  TextData @ {
      "H : \[LeftAngleBracket]",
      ColorNBox["2", 1],
      ",",
      ColorNBox["2", 2],
      ",",
      ColorNBox["4", 3],
      "\[RightAngleBracket] -> \[DoubleStruckCapitalZ]   <==>   H^{",
      ColorNBox["1", 1],
      ",",
      ColorNBox["2", 2],
      ",",
      ColorNBox["3", 3],
      "\[Succeeds]} : \[LeftAngleBracket]\[RightAngleBracket] -> \[LeftAngleBracket]",
      ColorNBox["2", 1],
      ",",
      ColorNBox["2", 2],
      ",",
      ColorNBox["4", 3],
      "\[RightAngleBracket] -> \[DoubleStruckCapitalZ]"
    },
  "PreformattedCode"
]


(* ::Subsubsection:: *)
(*Applications*)


TestMarkdown @ Cell[
  TextData @ {
      "^{  }I : \[LeftAngleBracket]",
      ColorNBox["H", 5],
      ",",
      ColorNBox["W", 6],
      ",",
      ColorNBox["3", 8],
      "\[RightAngleBracket]\nI^{",
      ColorNBox["3", 8],
      "\[Succeeds]} : \[LeftAngleBracket]",
      ColorNBox["H", 5],
      ",",
      ColorNBox["W", 6],
      "\[RightAngleBracket] -> \[LeftAngleBracket]",
      ColorNBox["3", 8],
      "\[RightAngleBracket] -> \[DoubleStruckCapitalR]"
    },
  "PreformattedCode"
]


TestMarkdown @ Cell[
  TextData @ {
      "I[",
      ColorNBox["1", 5],
      ",",
      ColorNBox["1", 6],
      "] = ",
      ColorNBox["[", 8],
      "1 ",
      StyleBox["0", FontColor -> $LightGray],
      " ",
      StyleBox["0", FontColor -> $LightGray],
      " = ",
      StyleBox["\[FilledSquare]", FontColor -> RGBColor[1, 0, 0]]
    },
  "PreformattedCode"
]


(* ::Subsubsection:: *)
(*Array circuit*)


(* ::Subsection:: *)
(*Unnest op*)


TestMarkdown @ Cell[TextData[{" ^{  }A:\[LeftAngleBracket]\[Ellipsis]\[RightAngleBracket] -> \[LeftAngleBracket]", ColorNBox["\[FilledSquare]", 6], "\[RightAngleBracket] -> \[DoubleStruckCapitalV]\n A^{", ColorNBox["n", 6], "\[Precedes]}:\[LeftAngleBracket]\[Ellipsis],", ColorNBox["\[FilledSquare]_{n}", 6], ",\[Ellipsis]\[RightAngleBracket] -> \[DoubleStruckCapitalV]"}], "PreformattedCode"]


TestMarkdown @ Cell[
  TextData @ {
      "A : \[LeftAngleBracket]",
      ColorNBox["\[FilledSquare]", 1],
      ",",
      ColorNBox["\[FilledSquare]", 2],
      "\[RightAngleBracket] -> \[LeftAngleBracket]",
      ColorNBox["\[FilledSquare]", 3],
      "\[RightAngleBracket] -> \[DoubleStruckCapitalV] \n\nA^{",
      ColorNBox["1", 3],
      "\[Precedes]}[",
      ColorNBox["\[FilledCircle]", 3],
      ",",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["\[FilledCircle]", 2],
      "] \[Congruent] A[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["\[FilledCircle]", 2],
      "][",
      ColorNBox["\[FilledCircle]", 3],
      "]"
    },
  "PreformattedCode"
]


TestMarkdown @ Cell[TextData[{"(A^{", ColorNBox["n", 6], "\[Succeeds]})^{", ColorNBox["n", 6], "\[Precedes]} \[Congruent] A"}], "PreformattedCode"]


(* ::Subsubsection:: *)
(*Array circuit*)




(* ::Subsection:: *)
(*Diagonal op*)


TestMarkdown @ Cell[
  TextData @ {
      " ^{   }M:\[LeftAngleBracket]",
      ColorNBox["N", 1],
      ",",
      ColorNBox["N", 2],
      "\[RightAngleBracket] -> \[DoubleStruckCapitalV]\nM^{",
      ColorNBox["1", 3],
      ":(",
      ColorNBox["1", 1],
      ",",
      ColorNBox["2", 2],
      ")}:\[LeftAngleBracket]",
      ColorNBox["N", 3],
      "\[RightAngleBracket] -> \[DoubleStruckCapitalV]\n \nM^{",
      ColorNBox["1", 3],
      ":(",
      ColorNBox["1", 1],
      ",",
      ColorNBox["2", 2],
      ")}[",
      ColorNBox["d", 3],
      "] \[Congruent] M[",
      ColorNBox["d", 3],
      ",",
      ColorNBox["d", 3],
      "] "
    },
  "PreformattedCode"
]



TestMarkdown @ StringBlockTemplate["M = #1", NAF[{{1,2, 3}, {4, 5, 6}, {7, 8, 9}}]]


TestMarkdown @ Cell[
  TextData @ {
      "D[",
      ColorNBox["\[FilledCircle]", 3],
      "] \[Congruent] M[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["\[FilledCircle]", 2],
      "]\n\nD[",
      ColorNBox["1", 3],
      "] \[Congruent] M[",
      ColorNBox["1", 1],
      ",",
      ColorNBox["1", 2],
      "] = 1\nD[",
      ColorNBox["2", 3],
      "] \[Congruent] M[",
      ColorNBox["2", 1],
      ",",
      ColorNBox["2", 2],
      "] = 5\nD[",
      ColorNBox["3", 3],
      "] \[Congruent] M[",
      ColorNBox["3", 1],
      ",",
      ColorNBox["3", 2],
      "] = 9\n\nD = ",
      ColorNBox["[", 3],
      "1 5 9",
      ColorNBox["]", 3]
    },
  "PreformattedCode"
]


TestMarkdown @ NAF[MapAt[Background3Form, {{1,1},{2,2},{3,3}}] @ {{1,2, 3}, {4, 5, 6}, {7, 8, 9}}]




(* ::Subsection:: *)
(*Pick op*)


TestMarkdown @ Cell[
  TextData @ {
      "   ",
      ColorNBox["P", 5],
      " : ",
      StyleBox["K", FontWeight -> "Bold"],
      " -> \:27e8s_1,s_2,\[Ellipsis]\:27e9\n   ",
      ColorNBox["A", 6],
      " :      \:27e8s_1,s_2,\[Ellipsis]\:27e9 -> ",
      StyleBox["V", FontWeight -> "Bold"],
      "\n",
      ColorNBox["A", 6],
      "[",
      ColorNBox["P", 5],
      "] : ",
      StyleBox["K", FontWeight -> "Bold"],
      "     _{  }         -> ",
      StyleBox["V", FontWeight -> "Bold"]
    },
  "PreformattedCode"
]


TestMarkdown @ Cell[
  TextData @ {
      "(",
      ColorNBox["A", 6],
      "[",
      ColorNBox["P", 5],
      "])[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["\[FilledCircle]", 2],
      ",",
      ColorNBox["\[FilledCircle]", 3],
      ",\[Ellipsis]] = ",
      ColorNBox["A", 6],
      "[",
      ColorNBox["P", 5],
      "[",
      ColorNBox["\[FilledCircle]", 1],
      ",",
      ColorNBox["\[FilledCircle]", 2],
      ",",
      ColorNBox["\[FilledCircle]", 3],
      ",\[Ellipsis]]]"
    },
  "PreformattedCode"
]


TestMarkdown @ Cell[
  TextData @ {
      "P = (",
      ColorNBox["2", 1],
      ")            =>   A[P] = 20\nP = ",
      ColorNBox["[", 3],
      "(",
      ColorNBox["3", 1],
      ") (",
      ColorNBox["1", 1],
      ") (",
      ColorNBox["2", 1],
      ")   =>   A[P] = ",
      ColorNBox["[", 3],
      "30 10 20",
      ColorNBox["]", 3],
      "\nP = ",
      ColorNBox["[", 3],
      ColorNBox["[", 4],
      "(",
      ColorNBox["1", 1],
      ")",
      ColorNBox["]", 4],
      " ",
      ColorNBox["[", 4],
      "(",
      ColorNBox["2", 1],
      ")",
      ColorNBox["]", 4],
      ColorNBox["]", 3],
      "  =>   A[P] = ",
      ColorNBox["[", 3],
      ColorNBox["[", 4],
      "10",
      ColorNBox["]", 4],
      " ",
      ColorNBox["[", 4],
      "20",
      ColorNBox["]", 4],
      ColorNBox["]", 3]
    },
  "PreformattedCode"
]


TestMarkdown @ Cell[
  TextData @ {
      "P = (",
      ColorNBox["1", 1],
      ",",
      ColorNBox["1", 2],
      ")                 =>   A[P] = 10\nP = ",
      ColorNBox["[", 3],
      "(",
      ColorNBox["1", 1],
      ",",
      ColorNBox["1", 2],
      ") (",
      ColorNBox["2", 1],
      ",",
      ColorNBox["2", 2],
      ") (",
      ColorNBox["2", 1],
      ",",
      ColorNBox["1", 2],
      ")",
      ColorNBox["]", 3],
      "   =>   A[P] = ",
      ColorNBox["[", 3],
      "10 40 30",
      ColorNBox["]", 3],
      "\nP = ",
      ColorNBox["[", 3],
      ColorNBox["[", 4],
      "(",
      ColorNBox["2", 1],
      ",",
      ColorNBox["1", 2],
      ")",
      ColorNBox["]", 4],
      " ",
      ColorNBox["[", 4],
      "(",
      ColorNBox["1", 1],
      ",",
      ColorNBox["2", 2],
      ")",
      ColorNBox["]", 4],
      ColorNBox["]", 3],
      "     =>   A[P] = ",
      ColorNBox["[", 3],
      ColorNBox["[", 4],
      "30",
      ColorNBox["]", 4],
      " ",
      ColorNBox["[", 4],
      "20",
      ColorNBox["]", 4],
      ColorNBox["]", 3]
    },
  "PreformattedCode"
]


(* ::Subsubsection:: *)
(*Array circuit*)




(* ::Section:: *)
(*Summary*)


TestMarkdown @ Cell[
  BoxData @ GridBox[
      {
        {
          Cell[TextData[StyleBox["operation", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["meaning", FontWeight -> "Bold"]], "Text"],
          Cell[TextData[StyleBox["cellwise definition", FontWeight -> "Bold"]], "Text"]
        },
        {Cell[TextData[StyleBox["lift", FontWeight -> "Bold"]], "Text"], Cell["apply `f` to matching cells", "Text"], Cell["`f^{\[UpArrow]}(A,B)[\[Ellipsis]] \[Congruent] f(A[\[Ellipsis]], B[\[Ellipsis]])`", "Text"]},
        {
          Cell[TextData[StyleBox["aggregate", FontWeight -> "Bold"]], "Text"],
          Cell["use `f` to summarize axis", "Text"],
          Cell[TextData[{"`f_{", ColorNBox["n", 2], "}(A)[\[Ellipsis]] \[Congruent] f_{", ColorNBox["\[FilledCircle]", 2], "\[Element]", ColorNBox["\[FilledSquare]", 2], "}{ A[\[Ellipsis],", ColorNBox["\[FilledCircle]", 2], ",\[Ellipsis]] }`"}], "Text"]
        },
        {
          Cell[TextData[StyleBox["merge", FontWeight -> "Bold"]], "Text"],
          Cell["collapse parts of an axis together", "Text"],
          Cell[TextData @ {"`f^{R}_{", ColorNBox["n", 2], "}(A)[\[Ellipsis],", ColorNBox["\[FilledCircle]", 1], ",\[Ellipsis]] \[Congruent] f_{", ColorNBox["\[FilledCircle]", 2], "\[Element]", ColorNBox["\[FilledSquare]", 2], "}{ A[\[Ellipsis],", ColorNBox["\[FilledCircle]", 2], ",\[Ellipsis]] | R(", ColorNBox["\[FilledCircle]", 2], ",", ColorNBox["\[FilledCircle]", 1], ") }`"}, "Text"]
        },
        {
          Cell[TextData[StyleBox["broadcast", FontWeight -> "Bold"]], "Text"],
          Cell["add constant axis", "Text"],
          Cell[TextData[{"`A^{", ColorNBox["n", 2], "\:279c}[\[Ellipsis],", ColorNBox["\[FilledCircle]", 2], ",\[Ellipsis]] \[Congruent] A[\[Ellipsis]]`"}], "Text"]
        },
        {Cell[TextData[StyleBox["transpose", FontWeight -> "Bold"]], "Text"], Cell["permute (reorder) axes", "Text"], Cell["`A^{\[Sigma]}[\[Ellipsis]] \[Congruent] A[\[Sigma](\[Ellipsis])]`", "Text"]},
        {
          Cell[TextData[StyleBox["nest", FontWeight -> "Bold"]], "Text"],
          Cell["form array of arrays", "Text"],
          Cell[TextData[{"`A^{", ColorNBox["n", 2], "\[Succeeds]}[\[Ellipsis]][", ColorNBox["\[FilledCircle]", 2], "] \[Congruent] A[\[Ellipsis],", ColorNBox["\[FilledCircle]", 2], ",\[Ellipsis]]`"}], "Text"]
        },
        {
          Cell[TextData[StyleBox["diagonal", FontWeight -> "Bold"]], "Text"],
          Cell["fuse multiple axes together", "Text"],
          Cell[TextData[{"`A^{", ColorNBox["1", 1], ":12}[", ColorNBox["\[FilledCircle]", 1], ",\[Ellipsis]] = A[", ColorNBox["\[FilledCircle]", 1], ",", ColorNBox["\[FilledCircle]", 1], ",\[Ellipsis]]`"}], "Text"]
        },
        {Cell[TextData[StyleBox["pick", FontWeight -> "Bold"]], "Text"], Cell["choose cells by key", "Text"], Cell["`A[P][\[Ellipsis]] \[Congruent] A[P[\[Ellipsis]]]`", "Text"]}
      },
      BaseStyle -> "Text",
      FrameStyle -> GrayLevel[0.9],
      GridBoxAlignment -> {"Columns" -> {{Left}}},
      GridBoxDividers -> {"Columns" -> {{True}}, "Rows" -> {{True}}},
      GridBoxSpacings -> {"Rows" -> {{1}}, "Columns" -> {{2}}}
    ],
  "Text"
]
