PublicFunction[ColabCopyImageToClipboard]

ColabCopyImageToClipboard[e_] := Scope[
  img = Rasterize[Style[e, Antialiasing -> False], ImageResolution-> 72];
  str = ExportString[img, "PNG", CompressionLevel -> 1, "ColorMapLength" -> 16, IncludeMetaInformation -> False];
  code = ToCharacterCode[str];
  result = StringJoin["<center><img width=\"", IntegerString[Round[First[ImageDimensions[img]]*2/3]], "\" src=\"data:image/png;base64,", Base64String[code], "\"></center>"];
  CopyToClipboard[result];
  import = ImportString[str, "PNG"];
  GoodBeep[];
  import
]