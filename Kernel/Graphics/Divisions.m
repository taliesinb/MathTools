PublicFunction[NiceTickDivisions]

SetUsage @ "
NiceTickDivisions[{min$, max$}, $minSpacing] returns an association of division specifications for the range {min$, max$}.
NiceTickDivisions[$$, $$, maxCount$] avoids creating more than maxCount$ lines.
* the gap between successive divisions will not be below $minSpacing.
* if $minSpacing would produce no divisions, one division on the left and one on the right are still produced.
* the association has keys {i$, num$} where i$ gives the depth and num$ gives the number of divisions into a power of ten.
"

NiceTickDivisions[{il:$NumberP, ir:$NumberP}, Into[n_], rest___] :=
	NiceTickDivisions[{il, ir}, Dist[il, ir] / n];

NiceTickDivisions[{il:$NumberP, ir:$NumberP}, im:$NumberP, maxCount_Integer:1000] := Scope[
	{il, ir} = Sort @ N @ List[il, ir];
	If[im == 0, im = 0.00000000001];
	If[il == ir || im < 0., Return @ <||>];
	d = Dist[ir, il];
	b = Log10Ceiling[d / 10]; (* what dec should we use to split this up *)
	{l, r, m} = {il, ir, im}/b;
	l //= Floor; r //= Ceiling;
	$maxDivs = 1 / m;
	divs = Flatten @ List @ genDivs @ 1;
	divs = Select[divs, LessEqualThan[maxCount]];
	result = If[divs === {},
		ran = Select[Range[l, r, 1], Between[{il, ir}]];
		If[Len[ran] >= 2, Assoc[{1, 1} -> b * FirstLast[ran]], Assoc[]]
	,
		ticks = Catenate @ MapIndex1[
			{div, ind} |-> Thread[{ind, Select[b * Range[l, r, 1/div], Between[{il, ir}]]}],
			divs
		];
		GroupBy[DedupBy[ticks, Last], {{ind}} |-> {ind, Part[divs, ind]}, Col2 /* N]
	]
];

_NiceDivisions := BadArguments[];

genDivs[n_] := Which[
	n * 10 <= $maxDivs, {n, n * 2, genDivs[n * 10]},
	n * 5  <= $maxDivs, {n, n * 5},
	n * 4  <= $maxDivs, {n, n * 4},
	n * 2  <= $maxDivs, {n, n * 2},
	n      <= $maxDivs,  n,
	True              ,  Nothing
];

(**************************************************************************************************)

PublicFunction[NiceBinDivisions]

SetUsage @ "
NiceBinDivisions[{min$, max$}, $maxCount] returns a list of bin boundaries.
* bin boundaries are chosen to be nice decimal values.
* the min and max boundaries will always contain min$ and max$.
* the outer boundaries will snap to 0 if appropriate.
"

NiceBinDivisions[{imin_, imax_}, n_] := Scope[
	{min, max} = N @ {imin, imax};
  d = Dist[min, max];
  zthresh = d * 0.1;
  If[min > 0 && imin =!= 1 && Dist[0, min] < zthresh, min = 0];
  If[max < 0 && imax =!= -1 && Dist[0, max] < zthresh, max = 0];
  d = Dist[min, max];
  Do[
  	round = Log10Ceiling[d / n, spec];
		l = Floor[min, round];
		r = Ceiling[max, round];
		divs = Range[l, r, round];
		If[Len[divs] > 0.6 * n, Break[]];
	,
		{spec, {2, 4, 5}}
	];
	divs
];

(**************************************************************************************************)

PublicFunction[NicePlotRange]

SetUsage @ "
NiceBinDivisions[{min$, max$}] will use snap min$ and max$ to nearby powers of 10 or their decimal multiples.
"

PublicFunction[NicePlotRange]

NicePlotRange[axes:{__List}] := Map[NicePlotRange, axes];

NicePlotRange[{il_?NumberQ, ir_?NumberQ}] := Scope[
	d = N @ Dist[il, ir];
	round = Log10Floor[d / 10, 2];
	l = Floor[N @ il, round];
	r = Ceiling[N @ ir, round];
	{l, r}
];

(**************************************************************************************************)

PublicTypesettingBoxFunction[NiceDivisionStringBoxes]

NiceDivisionStringBoxes[divs_, prec_:3, signColors_:None] := Scope[
	{b, divs2} = chooseScientificBase @ divs;

	$prec = prec;
	strs = Map[fmtTickItem, divs2];

	$signFn = If[ListQ[signColors], fmtSign[signColors], #1&];
	If[b =!= 0, strs //= MapFirstLast[scientificBox[b]]];

	strs
];

$strTrimRules = {StartOfString ~~ "-0." -> "-.", StartOfString ~~ "0." -> "."};

fmtSign[cols_][s_, r_] := StyleBox[StringTrimLeft["-"] @ s, Part[cols, Sign[r] + 2]];

General::badTickLabel = "`` is not a number or string.";
fmtTickItem = Case[
	r_ ? NumberQ  := $signFn[SRep[$strTrimRules] @ RealString[r, $prec], r];
	s_Str         := s;
	r_RowBox      := r;
	e_            := Msg::badTickLabel[e];
];


scientificBox[n_][s:("0" | StyleBox["0", _])] := "0";
scientificBox[n_][s_] := RowBox[{s, "\[Times]", SuperscriptBox["1\[NegativeVeryThinSpace]0", IntegerString[n]]}];

chooseScientificBase[divs_] := Scope[
	num = Select[RealQ] @ DeleteCases[0.] @ N @ Abs @ divs;
	base = Median @ Ceiling @ Log10 @ num;
	base = Floor[base / 4];
	dec = Power[1000, base];
	{base * 4, If[NumberQ[#], # / dec, #]& /@ divs}
];
