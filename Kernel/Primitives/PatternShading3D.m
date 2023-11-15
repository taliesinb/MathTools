PublicGraphicsDirective[PatternShading3D]

DeclareGraphicsPrimitive[PatternShading3D, "Color", patternShading3DBoxes, {3}];

patternShading3DBoxes[PatternShading3D[color:$ColorPattern, density:$NumberP:5]] :=
  ToGraphics3DBoxes @ toSurfaceAppearance[
    "UseScreenSpace" -> 1, "FeatureColor" -> color,
    "Shape" -> "Line", "StepCount" -> 1, "Tiling" -> {1, 1}*density,
    "IsTwoTone" -> 1
  ];

Options[toSurfaceAppearance] = {
  "StepCount" -> 1, "Tiling" -> {5, 5},
  "FeatureColor" -> Black, "UseScreenSpace" -> 1, "IsTwoTone" -> 1,
  "LuminanceModifier" -> 0.0, "Shape" -> "Disk"
};

(* https://mathematica.stackexchange.com/questions/240690/regionplot3d-knotdata/240859#240859 *)

toSurfaceAppearance[opts:OptionsPattern[toSurfaceAppearance]] :=
  SurfaceAppearance["RampShading",
    Sequence @@ FilterRules[{opts, Options[toSurfaceAppearance]}, Except["Shape"]],
    "Arguments" -> {"HalftoneShading", 0.5, Red, OptionValue["Shape"]},
    EdgeForm[], Texture["HalftoneShading" <> OptionValue["Shape"]]]

