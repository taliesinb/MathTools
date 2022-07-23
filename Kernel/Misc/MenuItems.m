If[!TrueQ[QuiverGeometryPackageLoader`$menuModified],
With[{qgPath = QuiverGeometryPackageLoader`$initFile},
  LinkWrite[$ParentLink, FrontEnd`AddMenuCommands["CDFPreview", {
    Delimiter,
    MenuItem[
      "&Export to markdown",
      KernelExecute[QuiverGeometry`QGNotebookExport[]],
      FrontEnd`MenuKey["e", FrontEnd`Modifiers -> {"Command", "Option"}],
      MenuEvaluator -> Automatic
    ],
    MenuItem[
      "Reload Quiver&Geometry",
      KernelExecute[QuiverGeometryPackageLoader`Load[False]],
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
  LinkWrite[$ParentLink, FrontEnd`AddMenuCommands["Balance", {
    Delimiter,
    MenuItem[
      "&Toggle inline cells",
      KernelExecute[QuiverGeometry`ToggleInlineCells[]],
      FrontEnd`MenuKey["t", FrontEnd`Modifiers -> {"Command", "Option"}],
      MenuEvaluator -> Automatic
    ]
  }]];
  QuiverGeometryPackageLoader`$menuModified = True;
]];

"Balance"