PublicIOFunction[URLAvailableQ]

URLAvailableQ[path_Str] /; SStartsQ[path, "file://"] := FileExistsQ @ SDrop[path, 7];

URLAvailableQ[url_Str] /; ToolAvailableQ["curl"] := RunTool["curl", url, OpenToolOutput -> False];

URLAvailableQ[url_] := WithInternet[Quiet @ URLRead[url, "StatusCode"] === 200];