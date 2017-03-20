(* PlotComplex[{a,b,c}] plots the complex numbers a,b,c on the complex unit circle. *)
PlotComplex[data_] := Module[{p},
    p = ListPlot[{Re[#], Im[#]} & /@ data,
        AxesOrigin -> {0, 0},
        PlotRange -> {{-1.2, 1.2}, {-1.2, 1.2}},
        AspectRatio -> 1,
        Frame -> True,
        PlotStyle -> Directive[Red, PointSize[.025]]
    ];
    Print[Show[p, Graphics[{Thickness[0.003], Circle[{0, 0}, 1]}]]]
];

(* This is to force conversion to polar form, implicitly assuming we are on the complex unit circle. *)
ComplexToPolar[z_] := E^(I Rationalize[Arg[z]/π]π);

(* Plug in values for the parameters. *)
R1 = Exp[4 Pi I/5];
Rτ = Exp[-3 Pi I/5];
R = ({
    {R1, 0},
    {0,  Rτ}
});
φ = GoldenRatio;
F = ({
    {φ^-1,      φ^(-1/2)},
    {φ^(-1/2), -φ^-1}
});
B = Inverse[F].R.F;
B11 = B[[1, 1]];
B12 = B[[1, 2]];
B21 = B[[2, 1]];
B22 = B[[2, 2]];

(*Count and plot eigenvalues. Play around with p in U[p].*)
ev = U[3] // N // Eigenvalues // ComplexToPolar; (* Computing without N[..] is super slow for large p. *)

Print["eigenvalues with mult.:\n",
    StringJoin[Map[ToString[#, TraditionalForm] <>
    " × " <>
    ToString[Count[ev, #]] <>
    ", " &, DeleteDuplicates[ev]]]
];

Print["det(Uₚ) = ", Times @@ ev];

PlotComplex[ev];

Print[MatrixForm[Table[Counts[Sort[Rationalize[Arg[Eigenvalues[N[U[p]]]]/π]]], {p, 2, 4}]]];

