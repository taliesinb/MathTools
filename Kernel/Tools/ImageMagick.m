PublicFunction[ImageMagickConvert]

Options[ImageMagickConvert] = {ImageSize -> Automatic};

ImageMagickConvert[File[src_Str] | src_Str, OptionsPattern[]] := Scope[
  UnpackOptions[imageSize];
  src //= NormalizePath;
  imageSizeSpec = Switch[imageSize,
    Automatic,            {},
    _Int,             {"-resize", imageSize},
    {_Int, _Int}, {"-resize", StringRiffle[imageSize, "-"]},
    _,                    ReturnFailed[]
  ];
  tmp = MakeTemporaryFile["image_magick_output.png"];
  If[!RunTool["convert", src, Sequence @@ imageSizeSpec, tmp], ReturnFailed[]];
  If[!FileExistsQ[tmp], ReturnFailed[]];
  Import @ tmp
];
