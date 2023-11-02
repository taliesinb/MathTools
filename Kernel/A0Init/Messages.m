PrivateSpecialFunction[BadOptionSetting]

General::badOptionSetting = "The setting `` -> `` is not recognized.";

BadOptionSetting[head_, opt_, val_] :=
  Message[MessageName[head, "badOptionSetting"], opt, MsgExpr @ val];


