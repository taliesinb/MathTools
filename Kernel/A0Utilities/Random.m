PublicFunction[RandomVector]

RandomVector[] := RandomReal[{-1, 1}, 2];
RandomVector[n_Integer] := RandomReal[{-1, 1}, {n, 2}];

PublicFunction[RandomUnitVector]

RandomUnitVector[] := ToPackedReal @ CosSin @ RandomReal[Tau];
RandomUnitVector[n_Integer] := ToPackedReal @ CosSin @ RandomReal[Tau, n];


