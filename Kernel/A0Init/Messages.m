PrivateMessageFunction[Msg, Fail]

SetUsage @ "
Msg is a special symbol, such that Msg::name[args$$] expands to ThrowMessage['name$', args$$].
"

SetUsage @ "
Fail is a special symbol, such that Fail::name[args$$] expands to ReturnFailed['name$', args$$].
Fail[] expands to ReturnFailed[].
"

DefineSimpleMacro[Msg,   MessageName[Msg, name_String][args___]  :> ThrowMessage[name, args]];
DefineSimpleMacro[Fail, {MessageName[Fail, name_String][args___] :> ReturnFailed[name, args], Fail[] :> ReturnFailed[]}];

(**************************************************************************************************)

PublicMessageFunction[MsgForm]

MsgForm = Case[
  p_MsgPath       := p;
  p_MsgForm       := p;
  e_              := iMsgForm[e, 3, 4];
  Seq[e_, d_]     := iMsgForm[e, d, 4];
  Seq[e_, d_, l_] := iMsgForm[e, d, l];
];

SetHoldAllComplete[iMsgForm]

iMsgForm[e_, d_, l_] := RawBoxes @ StyleBox[
  ToPrettifiedString[
    InternalHoldForm @ e,
    MaxDepth -> d, MaxLength -> l,
    $msgFormOpts
  ],
  ShowStringCharacters -> True, FontFamily -> "Source Code Pro", AutoSpacing -> False
];

toCommaForm[RawBoxes[StyleBox[s_Str, args___]]] :=
  RawBoxes @ StyleBox[StringTake[s, {2, -2}], args];

$msgFormOpts = Sequence[
  FullSymbolContext -> False, MaxStringLength -> 32,
  CompressLargeSubexpressions -> False, ElideAtomicHeads -> True,
  InlineColors -> True, CompactRealNumbers -> True, MaxIndent -> 0
];

(**************************************************************************************************)

PublicMessageFunction[MsgPrePrint, RawMsgForm, MsgCommaForm, MsgHold]

SetHoldAllComplete[MsgPrePrint, MsgHold]

DefineStandardTraditionalForm[MsgHold[e_] :> First @ MsgForm @ Uneval @ e];

MsgPrePrint = Case[
  (e_Str ? HoldAtomQ) ? likelyPathQ := MsgPath @ e;
  b_RawBoxes                        := b;
  RawMsgForm[b_]                    := b;
  HoldForm[e_]                      := % @ e;
  MsgCommaForm[a_List]              := toCommaForm @ iMsgForm[a, 2, 10];
  MsgHold[e_]                       := % @ e;
  e_                                := MsgForm @ Uneval @ e;
];

likelyPathQ[e_] := StringStartsQ[e, "/"] && StringCount[e, "/"] > 3;

(**************************************************************************************************)

PublicMessageFunction[CatchMessage]

SetUsage @ "
CatchMessage[body$] catches messages thrown by %ThrowMessage, issuing them as the current head and returning $Failed.
CatchMessage[head$, body$] issues messages from head$.
"

DefineSimpleMacro[CatchMessage, {
CatchMessage[body_]        :> CatchError[$MacroParentSymbol, body],
CatchMessage[head_, body_] :> CatchError[head, body]
}];

(**************************************************************************************************)

PublicMessageFunction[ThrowMessage]

SetUsage @ "
ThrowMessage['name$', args$$] throws message to %CatchMessage.
ThrowMessage['quiet', $$] is equivalent to ThrowFailed[].
"

ThrowMessage[msgName_Str, msgArgs___] := ThrowErrorMessage[msgName, msgArgs];
ThrowMessage["quiet", ___] := ThrowError[];
_ThrowMessage := BadArguments[];

(**************************************************************************************************)

PublicMessageFunction[ThrowFailed]

SetUsage @ "ThrowFailed[] just returns a $Failed to %CatchMessage."

ThrowFailed[] := ThrowError[];
_ThrowFailed := BadArguments[];

(**************************************************************************************************)

PublicMessageFunction[OptionMsg, OptionMatchMsg]

OptionMsg[opt_, val_] := ThrowMessage["invalidOption", opt, val];
General::invalidOption = "The setting `` -> `` is not valid.";

OptionMatchMsg[opt_, val_, patt_] := If[!MatchQ[val, patt], OptionMsg[opt, val]];

(**************************************************************************************************)

PublicMessageFunction[SameLenMsg]

SetUsage @ "
SameLenMsg[a$, b$] throws a generic message if a$ and b$ are not the same length.
SameLenMsg::name[$$] throws a specific message.
"

DefineMessageMacro[SameLenMsg, "notSameLength", "Lengths didn't match: `` and ``.",
SameLenMsg[msg_, a_, b_] :> Or[Len[a] === Len[b], ThrowMessage[msg, Len @ a, Len @ b]]
];

(**************************************************************************************************)

PublicMessageFunction[PartMsg]

SetUsage @ "
PartMsg[expr$, spec$$] returns Part[expr$, spec$$] if present, or throws a generic message if it is not present.
PartMsg::msgname[$$] throws a specific message.
* msg$ should be of the form symbol::msgname, whose first slot will be given key$ and second will be given available keys.
"

DefineMessageMacro[PartMsg, "partNotPresent", "Part `` is not present in expression ``.",
PartMsg[msg_, e_, p_] :> Replace[SafePart[e, p], m_Missing :> ThrowMessage[msg, p, e]]
];

(**************************************************************************************************)

PublicMessageFunction[FailureMsg]

SetUsage @ "
FailureMsg[expr$] throws a generic message if expr$ evaluates to $Failed.
FailureMsg::msgname[expr$, $$] throws a specific message.
"

DefineMessageMacro[FailureMsg, "computationFailed", "Internal computation failed.",
FailureMsg[msg_, e_, args___] :> OnFailed[e, ThrowMessage[msg, args]]
];

(**************************************************************************************************)

PublicMessageFunction[CheckMsg]

SetUsage @ "
CheckMsg[body$] throws a generic message if evaluating body$ produced errors.
CheckMsg::msgname[body$, msgArgs$$] throws a specific message.
"

SetHoldRest[CheckMsg];

DefineMessageMacro[CheckMsg, "messagesOccured", "Messages occurred.",
CheckMsg[msg_, body_, args___] :> Check[body, ThrowMessage[msg, args]]
];

(**************************************************************************************************)

PublicMessageFunction[LookupMsg]

SetUsage @ "
LookupMsg[assoc$, key$] returns the value associated with key$, or throws a generic message if it is not present.
LookupMsg::msgname[$$] throws a specific message.
* the message is passed the key and the list of available keys as a catenated string.
"

DefineMessageMacro[LookupMsg, "unrecognizedName", "`` is not one of ``.",
LookupMsg[msg_, assoc_, key_] :> Lookup[assoc, Key @ key, throwLookupMsg[msg, assoc, key]]
];

throwLookupMsg[msg_, assoc_, key_] := ThrowMessage[msg, key, MsgCommaForm @ Keys @ assoc];

