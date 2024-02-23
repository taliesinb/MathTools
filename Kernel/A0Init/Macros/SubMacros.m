PrivateMacro[SubAll, SubNone, SubAuto, SubInherited, SubMissing, SubFailed]

SetHoldAll[SubAll, SubAuto, SubInherited, SubMissing, SubFailed]

DefineSimpleMacro[SubAll,          {SubAll      [rhs_] :> Rep[All       :> rhs], SubAll      [lhs_, rhs_] :> Rep[lhs, All        :> rhs]}];
DefineSimpleMacro[SubNone,         {SubNone     [rhs_] :> Rep[None      :> rhs], SubNone     [lhs_, rhs_] :> Rep[lhs, None       :> rhs]}];
DefineSimpleMacro[SubAuto,         {SubAuto     [rhs_] :> Rep[Auto      :> rhs], SubAuto     [lhs_, rhs_] :> Rep[lhs, Auto       :> rhs]}];
DefineSimpleMacro[SubInherited,    {SubInherited[rhs_] :> Rep[Inherited :> rhs], SubInherited[lhs_, rhs_] :> Rep[lhs, Inherited  :> rhs]}];
DefineSimpleMacro[SubMissing,      {SubMissing  [rhs_] :> Rep[_Missing  :> rhs], SubMissing  [lhs_, rhs_] :> Rep[lhs, _Missing   :> rhs]}];
DefineSimpleMacro[SubFailed,       {SubFailed   [rhs_] :> Rep[$Failed   :> rhs], SubFailed   [lhs_, rhs_] :> Rep[lhs, $Failed    :> rhs]}];
