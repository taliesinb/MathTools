(* see /Applications/Mathematica.app/Contents/SystemFiles/FrontEnd/TextResources/Macintosh/MenuSetup.tr for info on anchors *)
If[!TrueQ[QuiverGeometryLoader`$menuModified] && $FrontEnd =!= Null,
With[{qgPath = QuiverGeometryLoader`$initFile},
  LinkWrite[$ParentLink, FrontEnd`AddMenuCommands["CDFPreview", {
    Delimiter,
    MenuItem[
      "Open as markdown",
      KernelExecute[SystemOpen @ QuiverGeometry`BuildSitePage[]],
      FrontEnd`MenuKey["'", FrontEnd`Modifiers -> {"Command", "Shift"}],
      MenuEvaluator -> Automatic
    ],
    MenuItem[
      "Serve to browser",
      KernelExecute[QuiverGeometry`ServeSitePage[]],
      FrontEnd`MenuKey["h", FrontEnd`Modifiers -> {"Command", "Shift"}],
      MenuEvaluator -> Automatic
    ],
    MenuItem[
      "Build page",
      KernelExecute[QuiverGeometry`BuildSitePage[]],
      FrontEnd`MenuKey["j", FrontEnd`Modifiers -> {"Command", "Shift"}],
      MenuEvaluator -> Automatic
    ],
    MenuItem[
      "Reload Quiver&Geometry",
      KernelExecute[QuiverGeometryLoader`Load[False]],
      FrontEnd`MenuKey["g", FrontEnd`Modifiers -> {"Command", "Option"}],
      MenuEvaluator -> Automatic, Method -> "Queued"
    ],
    MenuItem[
      "Load Quiver&Geometry",
      KernelExecute[Get[qgPath]],
      FrontEnd`MenuKey["g", FrontEnd`Modifiers -> {"Command", "Option", "Shift"}],
      MenuEvaluator -> Automatic, Method -> "Queued"
    ]
  }]];
  LinkWrite[$ParentLink, FrontEnd`AddMenuCommands["ClearCellOptions", {
    Delimiter,
    MenuItem["Struckthrough", FrontEnd`FontVariationsStrikeThrough->Toggle],
    Menu["Named Colors",
      MenuItem[#1, FontColor -> #2]& @@@ {
        "Red"    -> $Red,
        "Blue"   -> $Blue,
        "Green"  -> $Green,
        "Orange" -> $Orange,
        "Purple" -> $Purple,
        "Teal"   -> $Teal,
        "Gray"   -> $Gray,
        "Pink"   -> $Pink,
        "Yellow" -> $Yellow
      }
    ],
    Menu["Indexed Colors",
      Construct[MenuItem, #1, currentStyleSetting[FontColor, #2], FrontEnd`MenuKey[#1, FrontEnd`Modifiers->{"Option"}]]& @@@ {
        "1" -> "Color1",
        "2" -> "Color2",
        "3" -> "Color3",
        "4" -> "Color4",
        "5" -> "Color5",
        "6" -> "Color6",
        "7" -> "Color7",
        "8" -> "Color8",
        "9" -> "Color9"
      }
    ],
    Menu["Indexed Backgrounds",
      Construct[MenuItem, #1, currentStyleSetting[Background, #2], FrontEnd`MenuKey[#1, FrontEnd`Modifiers->{"Option", "Command"}]]& @@@ {
        "1" -> "Background1",
        "2" -> "Background2",
        "3" -> "Background3",
        "4" -> "Background4",
        "5" -> "Background5",
        "6" -> "Background6",
        "7" -> "Background7",
        "8" -> "Background8",
        "9" -> "Background9"
      }
    ]
  }]];
  LinkWrite[$ParentLink, FrontEnd`AddMenuCommands["InsertSplitBreak", {
    Delimiter,
      Menu["Table cell", {
          MenuItem["1-column table cell", KernelExecute[QuiverGeometry`InsertCellTable[6, 1]], MenuEvaluator -> Automatic],
          MenuItem["2-column table cell", KernelExecute[QuiverGeometry`InsertCellTable[6, 2]], MenuEvaluator -> Automatic],
          MenuItem["3-column table cell", KernelExecute[QuiverGeometry`InsertCellTable[6, 3]], MenuEvaluator -> Automatic],
          MenuItem["4-column table cell", KernelExecute[QuiverGeometry`InsertCellTable[6, 4]], MenuEvaluator -> Automatic],
          MenuItem["5-column table cell", KernelExecute[QuiverGeometry`InsertCellTable[6, 5]], MenuEvaluator -> Automatic]
      }]
  }]];
  LinkWrite[$ParentLink, FrontEnd`AddMenuCommands["Balance", {
    Delimiter,
    MenuItem[
      "&Toggle inline cells",
      KernelExecute[QuiverGeometry`ToggleInlineCells[]],
      FrontEnd`MenuKey["t", FrontEnd`Modifiers -> {"Command", "Option"}],
      MenuEvaluator -> Automatic
    ]
  }]];
  QuiverGeometryLoader`$menuModified = True;
]];
