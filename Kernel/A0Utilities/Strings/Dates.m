PublicHead[RecentYearPattern]

$recentYearRE = """(?:20[012]\d)""";

DefineStringPattern[
  RecentYearPattern :> $recentYearRE
];

(**************************************************************************************************)

PublicHead[NumericDatePattern]

$longYearRE  = """(?:(?:19|20)\d\d)""";
$shortYearRE = """(?:[012789]\d)""";
$yearRE      = "(?:" <> $longYearRE <> "|" <> $shortYearRE <>")";
$monthRE     = """(?:0?[1-9]|1[012])""";
$dayRE       = """(?:0?[1-9]|[12]\d|3[01])""";

dateSep[args__] := Splice[StringJoin[Riffle[{args}, #]]& /@ {"/", "-"}];

(* YMD, MDY, DMY according to https://en.wikipedia.org/wiki/Date_format_by_country *)

$numericDatePatternRE = """(?<![-/0-9])(?:""" <> Riffle[{
  dateSep[$longYearRE, $monthRE, $dayRE],
  dateSep[$longYearRE, $monthRE],
  dateSep[$monthRE, $longYearRE],
  dateSep[$dayRE, $dayRE, $yearRE]
}, "|"] <> """)(?![-/0-9])""";

DefineStringPattern[
  NumericDatePattern :> $numericDatePatternRE
];

(**************************************************************************************************)

PublicHead[SlashDatePattern]

$strictMonthRE     = """(?:0[1-9]|1[012])""";
$strictDayRE       = """(?:0[1-9]|[12]\d|3[01])""";

$slashDateRE = StringJoin[$longYearRE, "/", $strictMonthRE, "/", $strictDayRE];

DefineStringPattern[
  SlashDatePattern :> $slashDateRE
];

(**************************************************************************************************)

PublicHead[NoteDatePattern]

$noteDateRE = "Y" <> $longYearRE <> "M" <> $strictMonthRE <> "D" <> $strictDayRE;

DefineStringPattern[
  NoteDatePattern :> $noteDateRE
];

(**************************************************************************************************)

PublicHead[YAMLDatePattern, YAMLDateTimePattern]

$yamlDateRE = """(?:19|20)\d\d-[01]\d-[0123]\d"""
$yamlTimeRE = """[012]\d:[0-5]\d:[0-5]\d""";
$yamlDateTimeRE = $yamlDateRE <> "T" <> $yamlTimeRE;

DefineStringPattern[
  YAMLDatePattern :> $yamlDateRE,
  YAMLDateTimePattern :> $yamlDateTimeRE
];

(**************************************************************************************************)

PublicHead[SpelledDatePattern]

$spelledMonthRE = "(?:Jan|January|Feb|February|March|Mar|April|Apr|May|June|Jun|July|Jul|August|Aug|September|Sept|Sep|October|Oct|November|Nov|December|Dec)";

dateSep2[args__] := Splice[StringJoin[Riffle[{args}, #]]& /@ {"/", "-", ",", ", ", " "}];

$spelledDatePatternRE = """(?<![-/0-9])(?:""" <> Riffle[{
  StringJoin[$spelledMonthRE, " ", $dayRE, ",? ", $yearRE],
  StringJoin[$dayRE, " ", $spelledMonthRE, ",? ", $yearRE],
  StringJoin[$spelledMonthRE, "[ -]", $longYearRE],
  StringJoin[$longYearRE, "[ -]", $spelledMonthRE]
}, "|"] <> """)(?![-/0-9])""";

DefineStringPattern[
  SpelledDatePattern :> $spelledDatePatternRE
];
