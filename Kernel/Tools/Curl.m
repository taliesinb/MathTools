PublicFunction[URLAvailableQ]

URLAvailableQ[path_String] /; StringStartsQ[path, "file://"] := FileExistsQ @ StringDrop[path, 7];

URLAvailableQ[url_String] /; ToolAvailableQ["curl"] := RunTool["curl", url, OpenToolOutput -> False];

URLAvailableQ[url_] := WithInternet[Quiet @ URLRead[url, "StatusCode"] === 200];