PublicFunction[ImageMagickConvert]

Options[ImageMagickConvert] = {ImageSize -> Automatic};

ImageMagickConvert[File[src_String] | src_String, OptionsPattern[]] := Scope[
  UnpackOptions[imageSize];
  src //= NormalizePath;
  imageSizeSpec = Switch[imageSize,
    Automatic,            {},
    _Integer,             {"-resize", imageSize},
    {_Integer, _Integer}, {"-resize", StringRiffle[imageSize, "-"]},
    _,                    ReturnFailed[]
  ];
  tmp = MakeTemporaryFile["image_magick_output.png"];
  If[!RunTool["convert", src, Sequence @@ imageSizeSpec, tmp], ReturnFailed[]];
  If[!FileExistsQ[tmp], ReturnFailed[]];
  Import @ tmp
];
