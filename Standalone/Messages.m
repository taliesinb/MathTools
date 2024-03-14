CatchError::usage = "CatchError[head, body] catches errors thrown by ThrowErrorMessage and ThrowError."

SetAttributes[CatchError, HoldAllComplete];

CatchError[head_Symbol, body_] := Catch[body, $StandaloneErrorTag, StandaloneErrorHandler[head]];

CatchError::noMsgDefined = "Caught an error, but no message with name `` is defined for catching symbol ``.";

StandaloneErrorHandler[_][value_, _] := value;
StandaloneErrorHandler[msgHead_][StandaloneErrorMessage[msgName_String, msgArgs___], _] := (
  If[!StringQ[MessageName[msgHead, msgName]] && !StringQ[MessageName[General, msgName]],
    Message[CatchError::noMsgDefined, msgName, msgHead],
    Message[MessageName[msgHead, msgName], msgArgs]
  ];
  $Failed
);

(**************************************************************************************************)

SetAttributes[CatchErrorAsFailure, HoldAllComplete];

CatchErrorAsFailure::usage =
"CatchFailure[head, body] catches errors thrown by ThrowErrorMessage and ThrowError, returning a Failure object.
CatchFailure[head, body, fn] applies fn to the failure."

CatchErrorAsFailure[name_, body_, fn_:Identity] := Catch[body, $StandaloneErrorTag, errorAsFailureHandler[name, fn]];

errorAsFailureHandler[_, _][value_, _] := Failure[failureName, Association[]];

errorAsFailureHandler[name_, fn_][StandaloneErrorMessage[msgName_String, msgArgs___], _] :=
  fn @ Failure[name, Association[
    "MessageTemplate" :> MessageName[General, msgName],
    "MessageParameters" -> {msgArgs}
  ]];

(**************************************************************************************************)

ThrowErrorMessage::usage = "
ThrowErrorMessage['name', body] throws a message to CatchError where it is issued.
ThrowErrorMessage['quiet', ...] is equivalent to ThrowError[].
"

ThrowErrorMessage[msgName_String, msgArgs___] :=
  Throw[StandaloneErrorMessage[msgName, msgArgs], $StandaloneErrorTag];

ThrowErrorMessage["quiet", ___] :=
  ThrowError[];

(**************************************************************************************************)

ThrowError::usage = "
ThrowError[] returns a $Failed from the containing CatchError.
ThrowError[expr$] returns $expr from the containing CatchError.
"

ThrowError[] := Throw[$Failed, $StandaloneErrorTag];
ThrowError[e_] := Throw[e, $StandaloneErrorTag];

(**************************************************************************************************)

SetAttributes[TopLevelEvaluationFunction, HoldAllComplete];

TopLevelEvaluationFunction[body_] := CatchError[TopLevelEvaluationFunction, body];
TopLevelEvaluationFunction[bodies___] := TopLevelEvaluationFunction[CompoundExpression[bodies]];

(* this relies on a customized stylesheet that contains e.g.:
Cell[StyleData["Code"],
  CellEvaluationFunction -> Function[boxes,
    If[DownValues[System`TopLevelEvaluationFunction] === {},
      ToExpression @ boxes,
      ToExpression[boxes, StandardForm, System`TopLevelEvaluationFunction]]]
]
*)

StandaloneErrorHandler[TopLevelEvaluationFunction][value_, _] := (
  Message[General::uncaughtError];
  value
);
General::uncaughtError = "ThrowError occurred without a surrounding CatchError.";

StandaloneErrorHandler[TopLevelEvaluationFunction][msg:StandaloneErrorMessage[msgName_String, msgArgs___], tag_] := (
  Message[General::uncaughtErrorMessage];
  StandaloneErrorHandler[General][msg, tag];
);
General::uncaughtErrorMessage = "ThrowErrorMessage occurred without a surrounding CatchError.";
