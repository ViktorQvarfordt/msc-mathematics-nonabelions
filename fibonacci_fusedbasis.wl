Rel[a_, b_, c_] := Which[
    a == 1 || b == 1, 1,
    c == 1, R1,
    c == 2, Rτ
];

Bel[a_, b_, c_, d_, e_, f_] := Which[
    a == 1, Rel[b,c,d],
    b == 1, 1,
    c == 1, 1,
    d == 1, Rel[b,c,a],
    e == 1 && f == 1, B11,
    e == 1 && f == 2, B12,
    e == 2 && f == 1, B21,
    e == 2 && f == 2, B22,
    _, 1
];

t = 2;
UpFusedEl[a_, b_, c_, d_, e_] := Module[{output, fs},
    output = {};
    Do[
        If[
            Not[a == 1 && h != 2] &&
            Not[g == 1 && e != 2] &&
            Not[h == 1 && g != c] &&
            Not[g == 1 && h != c],
            (* Decide possible values for f. *)
            Which[
                a == 1 && c == 1 && g == 1, fs = {},
                a == 1 && c == 1 && g == 2, fs = {1},
                a == 1 && c == 2, fs = {2},
                a == 2 && c == 1, fs = {2},
                a == 2 && c == 2 && g == 1, fs = {2},
                a == 2 && c == 2 && g == 2 && d == 1, fs = {2},
                a == 2 && c == 2 && g == 2 && d == 2, fs = {1, 2}
            ];
            AppendTo[output, {Sum[Bel[a,c,t,d,b,f] Bel[f,t,t,e,d,g] Bel[a,t,c,g,f,h], {f, fs}], {a,h,c,g,e}}];
        ],
        {g, 2}, {h, 2}
    ];
    output
];

UpFusionStates = {
    {1,2,1,2,1},

    {1,2,1,2,2},

    {2,2,1,2,1},

    {1,2,2,2,1},

    {2,1,1,1,2},
    {2,2,1,2,2},

    {1,2,2,1,2},
    {1,2,2,2,2},

    {2,1,2,2,1},
    {2,2,2,2,1},

    {2,1,2,2,2},
    {2,2,2,1,2},
    {2,2,2,2,2}
};


UpFused[] := Map[
    Module[{row,idx},
        row = ConstantArray[0, Length[UpFusionStates]];
        Do[
            idx = Position[UpFusionStates, fs[[2]]][[1]][[1]];
            row[[idx]] = fs[[1]],
            {fs, UpFusedEl @@ #}
        ];
        row
    ] &,
    UpFusionStates
];

(* Print[MatrixForm[UpFusedEl[2,1,2,2,2]]]; *)

Print[UpFused[]//MatrixForm];

(* PlotComplex[Eigenvalues[N[UpFused[]]]]; *)
