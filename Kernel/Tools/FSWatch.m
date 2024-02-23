PublicIOFunction[FSWatch]

FSWatch::usage = "
FSWatch[path$] runs 'fswatch', which watches for changes to a file or directory.
* an %InputStream[$$] is returned that will have strings ready whenever an event occurs.
* if path$ is All then the entire 'Kernel' directory is watched.
* use %ReadString with %TimeConstraint to perform non-blocking polling for events.
"

FSWatch::binMissing = "The fswatch binary could not be found. Please install it."
FSWatch::badStream = "Could not open input stream.";

FSWatch[path_] := Scope[

  binPath = FindTool["fswatch", None];
  If[binPath === None, Message[FSWatch::binMissing]; Return[$Failed]];
  cmdArgs = {binPath};

  paths = path /. All -> MTLoader`$SourceDirectory;
  paths = Map[SJoin["'", ExpandFileName[#], "'"]&, Developer`ToList @ paths];

  cmdStr = SJoin[binPath, " ", Riffle[Flatten @ paths, " "]];

  stream = Quiet @ Check[OpenRead["!" <> cmdStr, BinaryFormat -> True], $Failed];
  If[H[stream] =!= InputStream, Message[FSWatch::badStream]; Return[$Failed]];

  stream
];

