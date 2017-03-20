(* FibString[n] returns a list all Fibonacci strings
   of length n, sorted by charge sectors.
   A Fibonacci string is a list of 1's and 2's with no recurring 1's. *)

FibStringRecurse[1] := {{1}, {2}};
FibStringRecurse[n_] := Flatten[
    Map[
        If[
            Last[#] == 2,
            {Flatten[{#, 1}],
            Flatten[{#, 2}]}, {Flatten[{#, 2}]}
        ] &,
        FibString[n - 1]],
    1
];

FibString[n_] := SortBy[FibStringRecurse[n], {First[#], Last[#]} &];

(* FibStringCS[n, csl, csr] returns FibString[n] restricted to left and right
   charge sectors given by csl and csr. *)
FibStringCS[n_, csl_, csr_] := Select[
    FibStringRecurse[n],
    First[#] == csl && Last[#] == csr &
];

(* σ[n,j] returns the representation of the j:th braid group generator
   in the fusion space of n Fibonacci anyons, including all possible
   left and right charge sectors. *)
σMeta[n_, j_, fusionStates_] := Map[
    Module[{labels, row, idx, idx2, fusionStateCopy},
        fusionStateCopy = #;
        labels = Take[#, {j, j + 2}];
        row = ConstantArray[0, Length[fusionStates]];
        idx = Position[fusionStates, #][[1]][[1]];
        Switch[labels,
            {1, 2, 1}, row[[idx]] = R1,
            {1, 2, 2}, row[[idx]] = Rτ,
            {2, 2, 1}, row[[idx]] = Rτ,
            {2, 1, 2}, (
                row[[idx]] = B11;
                fusionStateCopy[[j + 1]] = 2;
                idx2 = Position[fusionStates, fusionStateCopy][[1]][[1]];
                row[[idx2]] = B12
            ),
            {2, 2, 2}, (
                row[[idx]] = B22;
                fusionStateCopy[[j + 1]] = 1;
                idx2 = Position[fusionStates, fusionStateCopy][[1]][[1]];
                row[[idx2]] = B21;
            )
        ];
        row
    ] &, fusionStates];

σ[n_, j_] := σMeta[n, j, FibString[n + 1]];

σ[n_, j_, csl_, csr_] := σMeta[n, j, FibStringCS[n + 1, csl, csr]];

EigenvaluesArg[data_] := Sort[Rationalize[Arg[Eigenvalues[N[data]]]/Pi]];

(* U[p] returns σ₁σ₂⋯σₚσₚ₊₁σₚ⋯σ₂σ₁ on p+2 Fibonacci anyons. *)
U[p_] := Dot @@ Flatten[{
    Table[σ[p + 2, j], {j, 1, p + 1}],
    Table[σ[p + 2, j], {j, p, 1, -1}]
}, 1];
U[p_, csl_, csr_] := Dot @@ Flatten[{
    Table[σ[p + 2, j, csl, csr], {j, 1, p + 1}],
    Table[σ[p + 2, j, csl, csr], {j, p, 1, -1}]
}, 1];

(* Demo *)
Print[MatrixForm[σ[3, 1]]];
Print[σ[3, 1].σ[3, 2].σ[3, 1] == U[1]];
