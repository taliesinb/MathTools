PublicIOFunction[ImageMagickConvert]

Options[ImageMagickConvert] = {ImageSize -> Auto};

ImageMagickConvert[File[src_Str] | src_Str, OptionsPattern[]] := Scope[
  UnpackOptions[imageSize];
  src //= NormalizePath;
  imageSizeSpec = Switch[imageSize,
    Auto,            {},
    _Int,             {"-resize", imageSize},
    {_Int, _Int}, {"-resize", SRiffle[imageSize, "-"]},
    _,                    ReturnFailed[]
  ];
  tmp = MakeTemporaryFile["image_magick_output.png"];
  If[!RunTool["convert", src, Sequence @@ imageSizeSpec, tmp], ReturnFailed[]];
  If[!FileExistsQ[tmp], ReturnFailed[]];
  Import @ tmp
];
