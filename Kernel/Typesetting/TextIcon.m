PublicTypesettingForm[TextIcon]

TextIcon @ "
TextIcon['text$'] produces an inline %Graphics that contains graphics primitives depicting 'text$'.
* certain Unicode characters like ➡︎ (%BoldRightArrowSymbol) have hard-coded primitives.
* the resulting %Graphics is wrapped in %DynamicBox so that it will display with the current %FontSize.
* the following options are supported:
| %FontColor | a single color or a pair or %ColorGradient[$$] |
| %FontWeight | font boldness |
| %FontSize | %Inherited for dynamic behavior, or a fixed font size |
| %FontFamily | font family to use, defaulting to $MathFont |
| %Method | either 'Vector' or 'Raster' |
| %ContentPadding | fix plot range vertically |
| %Background | color of background |
| %FormatType | either 'MathForm' or None |
| %CompressionFactor | default %CompressionFactor to use for a color gradient |
* for %Method -> 'Raster', an %Image[$$] object will be produced, and the text does not have to be a string.
"

Options[TextIcon] = {
  FontSize -> Inherited,
  FontColor -> Auto,
  FontWeight -> Auto,
  FontFamily -> Auto,
  FontSlant -> Auto,
  Method -> "Vector",
  ContentPadding -> False,
  Background -> None,
  FormatType -> MathForm,
  CompressionFactor -> 0.8
}

DefineStandardTraditionalForm[ti:TextIcon[_, ___Rule] :> textIconBoxes[ti]];

TextIcon::vectorOnlyStr = "Method -> \"Vector\" only supports strings, not ``.";
TextIcon::nostrpoly = "Could not form a Polygon for `` using options ``.";

textIconBoxes[TextIcon[str_, opts___Rule]] := Scope[

  UnpackOptionsAs[TextIcon, opts,
    fontSize, fontColor, fontWeight, fontFamily, fontSlant,
    method, contentPadding, background, formatType, compressionFactor
  ];

  SetAuto[fontColor, Black];
  SetAuto[fontWeight, "Regular"];
  SetAuto[fontFamily, $MathFont];

  If[!ColorQ[fontColor],
    fontColor = ToColorGradient[fontColor];
    If[FreeQ[fontColor, CompressionFactor],
      AppTo[fontColor, CompressionFactor -> compressionFactor]];
  ];

  Switch[method,
    "Raster",
      SetAuto[fontSlant, Plain];
      SetNone[formatType, Id];
      textIconRasterBoxes[str, fontSize, fontColor, fontWeight, fontFamily, fontSlant, formatType, contentPadding],
    "Vector",
      textIconVectorBoxes[str, fontSize, fontColor, fontWeight, fontFamily, fontSlant, formatType, contentPadding, background],
    _,
      BadOptionSetting[TextIcon, Method, method];
      "?"
  ]
];

(**************************************************************************************************)

CacheVariable[$TextIconVectorCache]

textIconVectorBoxes[str_, fontSize_, fontColor_, fontWeight_, fontFamily_, fontSlant_, formatType_, contentPadding_, background_] := Scope @ CachedInto[
  $TextIconVectorCache, Hash @ {str, fontSize, fontColor, fontWeight, fontFamily, fontSlant, formatType, contentPadding, background},

  (* MathForm implies SingleLetterItalics, which we simulate here *)
  SetAuto[fontSlant, If[formatType === MathForm && StrQ[str] && SMatchQ[str, RomanLetter], Italic, Plain]];

  If[!StrQ[str],
    Message[TextIcon::vectorOnlyStr, MsgExpr @ str];
    Return @ {};
  ];

  opts = Sequence[FontSize -> 20, FontFamily -> fontFamily, FontWeight -> fontWeight, FontSlant -> fontSlant];
  result = TextToPolygon[str, opts];
  If[FailureQ[result],
    Message[TextIcon::nostrpoly, MsgExpr @ s, MsgExpr @ {opts}];
    Return @ {};
  ];

  {polygons, bounds, bshift} = result;
  If[contentPadding, bounds[[2]] = {-8, 21}, bounds[[2]] += {-1, 1}];
  baseImageSize = BoundsToSize[bounds] / 20;
  primBoxes = ToGraphicsBoxes @ polygons;

  If[MatchQ[fontSlant, "Italic" | Italic], fontColor //= toItalicDir];
  $disableAdjustment = False;
  primBoxes = Which[
    H[fontColor] === ColorGradient,
      $disableAdjustment = True;
      ColorGradientBoxes[primBoxes, fontColor, bounds],
    ColorQ[fontColor],     StyleBox[primBoxes, FaceForm @ fontColor],
    True,                  primBoxes
  ];
  gboxConstructor = makeTextIconGraphicsBox[primBoxes, bounds, baseImageSize, background, bshift];
  If[$disableAdjustment, gboxConstructor = gboxConstructor /. AdjustmentBox[b_, _] :> b];

  If[fontSize === Inherited,
    Fn[Null, DynamicBox[
      With[{System`\[FormalF] = CurrentValue[FontSize]}, #],
      TrackedSymbols :> {}
    ], HoldFirst] @@ gboxConstructor,
    F[gboxConstructor /. System`\[FormalF] -> fontSize]
  ]
];

$italicDir = {1, -0.27};
toItalicDir = Case[
  ColorGradient[col_, opts___Rule] := ColorGradient[col, $italicDir, opts];
  other_                           := other
];

(**************************************************************************************************)

CacheVariable[$TextIconRasterCache]

textIconRasterBoxes[str_, fontSize_Int, fc_, fw_, ff_, fs_, form_, cp_] := CachedInto[
  $TextIconRasterCache, {str, fontSize, fc, fw, ff, fs, form, cp},
  ToBoxes @ ColorGradientRasterize[
    Style[
      form @ str,
      FontSize -> fontSize, FontFamily -> ff, FontWeight -> fw, FontSlant -> fs
    ],
    fc,
    ContentPadding -> cp
  ]
];

(* NOTE: SingleEvaluation prevents script scaling *)
textIconRasterBoxes[str_, Inherited, fc_, fw_, ff_, fs_, form_, cp_] :=
  DynamicBox[
    textIconRasterBoxes[str, Round @ CurrentValue[FontSize], fc, fw, ff, fs, form, cp],
    TrackedSymbols :> {}
  ];

(**************************************************************************************************)

(* TODO: convert this to a TemplateBox! *)
(* the internal Construct has to be in there, apparently the FE thinks the expression doesn't need dynamic evaluation otherwise *)
makeTextIconGraphicsBox[boxes_, bounds_, baseImageSize_, background_, bshift_] :=
  Hold @ AdjustmentBox[
    Construct[
      GraphicsBox,
      boxes,
      PlotRange -> bounds, PlotRangePadding -> 0, AspectRatio -> Full, PlotRangeClipping -> False,
      ImageSize -> Ceiling[baseImageSize * System`\[FormalF] + 2, .5], ImagePadding -> {{1, 1}, {1, 1}},
      BaselinePosition -> Axis, Background -> background
    ],
    BoxBaselineShift -> (-bshift / System`\[FormalF])
  ];
