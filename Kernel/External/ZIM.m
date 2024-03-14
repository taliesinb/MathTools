(* TODO: move this file into an Imports/ folder *)

(* PublicObject[ZimArchive]

DefineStandardTraditionalForm[
  ZimArchive[path_String] :> ZimArchiveBoxes[path]
];

ZimArchiveBoxes[path_Str] := ToBoxes @ tightColoredBoxes[FileNameTake[path], $LightPurple];

 *)

(**************************************************************************************************)

(* ok, so when you make a ZimArchive it rewrites itself to have second arg the filename string.
then in Data/Zim/hash/property.mx it will save MX files that contain the corresponding data.
so we will need to have unified CachedIntoMX[$symbol, "Zim", {filename, prop}, computation] that can
either look under Zim/filename/prop.mx, or compute it and save it. it will also save it under that key
in the associated cache symbol.
*)

CacheVariable[$ZimDataCache]

(* these should be macros so that $stream will be localized *)
SetHoldAll[zimCached, zimUncached]

toBase[path_] := toBase[path] = FileBaseName @ path;

zimCached[path_, prop_, body_] := CachedIntoMX[$ZimDataCache, "Zim", {toBase @ path, prop},
  $stream = getZimStream[path];
  body
];

zimUncached[path_, prop_, body_] := (
  $stream = getZimStream[path];
  body
);

zimSeek[n_] := SetStreamPosition[$stream, n];
zimBinaryRead[t_] := BinaryRead[$stream, t /. $binTypeStr];
zimBinaryReadList[t_, n_] := BinaryReadList[$stream, t /. $binTypeStr, n];

(**************************************************************************************************)

PublicFunction[ClearZimCache]

ClearZimCache[] := (
  $ZimDataCache = UAssoc[];
  Scan[DeleteFile, FileNames["*.mx", DataPath["Zim"], Inf]];
);

(**************************************************************************************************)

CacheVariable[$ZimStreamCache]

General::ZimFilePathNotFound = "`` does not exist."

getZimStream[path_Str] := CachedInto[$ZimStreamCache, path, openZimStream @ path];

openZimStream[path_Str] := Scope[
  path == NormalizePath;
  If[!FileExistsQ[path], Msg::ZimFilePathNotFound[MsgPath @ path]];
  OpenRead[path, BinaryFormat -> True]
];

(**************************************************************************************************)

PrivateFunction[ZimHeaderData]

General::ZimFileBadMagic = "Magic number `` didn't match Zim format.";
General::ZimFileBadMimes = "Couldn't read mime types.";

ZimHeaderData[path_Str] := Scope @ CatchMessage @ zimCached[path, "Header",

  zimSeek[0];
  magic = zimBinaryRead[uint32];
  If[magic =!= 72173914, ReturnFailed["ZimFileBadMagic", magic]];

  assoc = BinaryReadToAssociation[$stream,
    MajorVersion:MinorVersion:uint16,
    UUID:uint128,
    EntryCount:ClusterCount:uint32,
    UrlPtrPos:TitlePtrPos:ClusterPtrPos:MIMEListPos:uint64,
    MainPage:LayoutPage:uint16,
    ChecksumPos:uint32
  ];

  zimSeek @ assoc["MIMEListPos"];
  i = 0;
  assoc["MIMEList"] = CollectWhile[
    str = zimBinaryRead[nzstr];
    If[!StrQ[str] || SLen[str] > 256 || i++ > 64, ReturnFailed["ZimFileBadMimes"]],
    str =!= "", str
  ];

  assoc
];

(**************************************************************************************************)

PrivateFunction[ZimDirectoryData]

ZimDirectoryData[path_Str] := Scope @ CatchMessage @ zimCached[path, "DirectoryData",

  UnpackAssociation[ZimHeaderData @ path, urlPtrPos, entryCount];

  zimSeek[urlPtrPos];
  pointers = zimBinaryReadList[uint64, entryCount];

  MonitoredMap[readZimEntry, pointers]
];

readZimEntry[ptr_] := Scope[

  zimSeek[ptr];
  data = BinaryReadToAssociation[$stream,
    MIMEType:uint16,
    uint8,
    Namespace:uint8,
    uint32
  ];
  data2 = If[data["MIMEType"] === 65535,
    BinaryReadToAssociation[$stream,
      RedirectIndex:uint32,
      Url:nzstr,
      Title:nzstr
    ],
    BinaryReadToAssociation[$stream,
      ClusterNum:BlobNum:uint32,
      Url:nzstr,
      Title:nzstr
    ]
  ];
  Join[data, data2]
];

(**************************************************************************************************)

PrivateFunction[ZimClusterData]

ZimClusterData[path_Str] := Scope @ CatchMessage @ zimUncached[path, "ClusterData",

  UnpackAssociation[ZimHeaderData @ path, clusterPtrPos, clusterCount];

  zimSeek[clusterPtrPos];
  pointers = zimBinaryReadList[uint64, clusterCount];
  sizes = App[1024 * 1024 * 4] @ Differences @ pointers;
  pairs = Trans[pointers, sizes];
  MonitoredMap[readClusterEntry, Take[pointers, 2]]
];

General::zimBadClusterCompression = "Unrecognized cluster compression in type byte ``."
readClusterEntry[ptr_] := Scope[
  Print["Reading cluster at ", ptr];
  zimSeek[ptr];
  info = zimBinaryRead[uint8];
  extended = BitGet[info, 5];
  type = BitAnd[info, 15];

  ptrType = If[extended == 1, uint64, int32];
  compression = Switch[type,
    1, "none",
    4, "lzma",
    5, "zstd",
    _, Msg::zimBadClusterCompression[info]
  ];

  zimDecodeCompressed[compression];

  firstOffset = zimBinaryRead[ptrType];
  numBlobs = firstOffset / If[extended == 1, 8, 4];
  Return[];
  blobOffsets = Pre[firstOffset] @ zimBinaryReadList[ptrType, numBlobs];
  blobPtrs = blobOffsets + ptr;

  PackAssociation[
    compression,
    numBlobs,
    blobPtr
  ]
];

General::corruptBlob = "Blob is corrupt.";

$tmpZstdOut = TemporaryPath["zstd_out"];
zimDecodeZstd[ptr_, size_] := Scope[
  bytes = ReadByteArray[$stream, size];
  decoded = ArchiveTools`ATDecode["ZSTD"][bytes, $tmpZstdOut];
  If[FailureQ[decoded], ThrowMessage["corruptBlob"]];
  Block[{$stream = OpenRead[$tmpZstdOut, BinaryFormat -> True]},
    Null
  ];
];


PrivateVariable[$ZimSrc]

$ZimSrc =
"""
#include <zim/archive.h>
#include <iostream>
int main(int argc, char* argv[])
{
  try
  {
    zim::Archive f¡("wikipedia.zim");

    for (zim::File::const_iterator it = f.begin(); it != f.end(); ++it)
    {
      std::cout << "url: " << it->getUrl() << " title: " << it->getTitle() << '
';
    }
  }
  catch (const std::exception& e)
  {
    std::cerr << e.what() << std::endl;
  }
}
""";
