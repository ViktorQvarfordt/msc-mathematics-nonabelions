CirclePlus[matrixList__] := ArrayFlatten @ ReleaseHold @ DiagonalMatrix[Hold /@ Select[{matrixList}, # != {{}} &]];
id[0] := {{}};
id[n_] := IdentityMatrix[n];
reducedBurauMatrix[n_, i_] := Which[
  i == 1,
  {{-t, 1}, {0, 1}} ⊕ id[n - 3],
  1 < i < n - 1,
  id[i - 2] ⊕ {{1, 0, 0}, {t, -t, 1}, {0, 0, 1}} ⊕ id[n - i - 2],
  i == n - 1,
  id[n - 3] ⊕ {{1, 0}, {t, -t}}];
σ = reducedBurauMatrix;
σ[5,2] // MatrixForm
