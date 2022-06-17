PrivateFunction[declareObjectPropertyDispatch, getObjectData]
PrivateVariable[$SelfObject]

getObjectData[_] := $Failed;

declareObjectPropertyDispatch[head_Symbol, dispatch_Symbol] := (
  getObjectData[head[data_Association] ? System`Private`NoEntryQ] := data;
  (obj:Blank[head] ? System`Private`NoEntryQ)[key_String, opts___Rule] := Block[{$SelfObject = obj},
    dispatch[getObjectData @ obj, key, opts]
  ];
  dispatch[data_, key_String] := Block[{res = Lookup[data, key, $Failed]}, res /; res =!= $Failed];
  dispatch[args___] := failDispatch[head, dispatch][args];
);

General::noobjprop = "There is no property named \"``\". Valid properties include: ``.";
General::noobjoptprop = "There is no property named \"``\" that accepts options. Such properties include: ``.";

failDispatch[head_, dispatch_][data_, key_String] :=
  Message[MessageName[head, "noobjprop"], key, commaString @ getValidProps[dispatch, data]];

failDispatch[head_, dispatch_][data_, key_String, __Rule] :=
  Message[MessageName[head, "noobjoptprop"], key, commaString @ getValidOptProps @ dispatch];

getValidProps[symbol_, data_] := Union[
  Cases[DownValues[symbol], HoldPattern[Verbatim[HoldPattern][symbol[_, key_String]] :> _] :> key],
  Keys @ data
];

getValidOptProps[symbol_] := getValidOptProps[symbol] =
  Union @ Cases[DownValues[symbol], HoldPattern[Verbatim[HoldPattern][symbol[_, key_String, __]] :> _] :> key];
