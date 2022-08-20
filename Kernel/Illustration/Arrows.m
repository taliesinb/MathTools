
$arrow = Graphics[{Opacity[1], EdgeForm[None], RGBColor[0.77444039253824, 0.7744403678635464, 0.7744403010737108], FilledCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}}, 2 * {{{-0.3166, -0.3333}, {-0.1833, 0.}, {-0.3166, 0.3333},
   {0.25, 0.}}}]}, AspectRatio -> Automatic, PlotRangeClipping -> False];

makeTextArrow[dir_, text_, offset1_, _] := {
   makeArrow[dir],
   makeText[text, Offset[-12 * Sign[offset1], dir/2], offset1]
};

makeTextArrow[dir_, {text1_, text2_}, offset1_, offset2_] := {
   makeArrow[dir],
   makeText[text1, Offset[-12 * Sign[offset1], dir/2], offset1],
   makeText[text2, Offset[-12 * Sign[offset2], dir/2], offset2]
};

makeArrow[dir_] := Style[Arrow[{{0, 0}, dir}], AbsoluteThickness[10], $LightGray, Arrowheads[{{Large, 1, $arrow}}]];

makeText[None, ___] := Nothing;
makeText[text_, offset__] := Text[Style[text, FontFamily -> "Avenir", FontSize -> 16, FontWeight -> Bold, LineSpacing -> {.75,0}], offset];

PublicFunction[LabeledArrowE, LabeledArrowS, LabeledArrowSE, LabeledArrowSW]

LabeledArrowE[text_, sz_:1] := makeTextArrow[sz*{2.5, 0}, text, {0, -1}, {0, 1}];
LabeledArrowW[text_, sz_:1] := makeTextArrow[sz*{-2.5, 0}, text, {0, -1}, {0, 1}];
LabeledArrowS[text_, sz_:1] := makeTextArrow[sz*{0, -2}, text, {-1.2, 0}, {1.2, 0}];
LabeledArrowN[text_, sz_:1] := makeTextArrow[sz*{0, 2}, text, {-1.2, 0}, {1.2, 0}];
LabeledArrowSE[text_, sz_:1] := makeTextArrow[sz*{1,-1}*2, text, {-1.2, -0.4}, {1.2, 0.1}];
LabeledArrowSW[text_, sz_:1] := makeTextArrow[sz*{-1,-1}*2, text, {1.2, -0.4}, {-1.2, 0.1}];


PublicFunction[LabeledArrow3D]

LabeledArrow3D[pos_, d_, label_, size:Except[_Rule]:Automatic, color:Except[_Rule]:$Gray, opts___Rule] := Scope[
   nd = szo * Normalize[d]; sz = size; label1 = label;
   SetAutomatic[sz, Norm[d] / 6];
   If[ListQ[sz], {szi, szo} = sz, szo = sz; szi = sz/4];
   other = If[Abs[Dot[Normalize[d], {0, 0, 1}]] > 0.9, Normalize @ {2, 1.5, 0}, {0, 0, -1}];
   l = Cross[nd, other]; o = pos;
   li = szi * Normalize[l];
   If[ListQ[label1], {label1, label2} = label1, label2 = None];
   opts2 = Sequence[TextAlignment -> Center, BaseStyle -> $CubeBaseStyle, LineSpacing -> {0.9, 0}];
   {
      Style[Polygon[{o + li, o + d + li, o + d + l/Sqrt[2], o + d + nd, o + d - l/Sqrt[2], o + d - li, o - li}], EdgeForm[None], FaceForm[color]],
      If[label1 === None, Nothing, PlaneInset[label1, o+d/2+li, {d, l}, {0, -1}, opts, opts2]],
      If[label2 === None, Nothing, PlaneInset[label2, o+d/2-li, {d, l}, {0, 1}, opts, opts2]]
   }
]
NeutralGraphics3D[LabeledArrow3D[{0,0,0},{2,0,0},"Abc"]]