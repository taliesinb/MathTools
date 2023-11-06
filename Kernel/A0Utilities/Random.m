PublicFunction[RandomVector]

RandomVector[] := RandomReal[{-1, 1}, 2];
RandomVector[n_Int] := RandomReal[{-1, 1}, {n, 2}];

PublicFunction[RandomUnitVector]

RandomUnitVector[] := ToPackedReal @ CosSin @ RandomReal[Tau];
RandomUnitVector[n_Int] := ToPackedReal @ CosSin @ RandomReal[Tau, n];


