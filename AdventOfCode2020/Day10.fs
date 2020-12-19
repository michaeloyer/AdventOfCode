#if INTERACTIVE
#else
module AdventOfCode2020.Day10
#endif

let example1 = [
    16
    10
    15
    5
    1
    11
    7
    19
    6
    12
    4
]
let example2 = [
    28
    33
    18
    42
    31
    14
    46
    20
    48
    47
    24
    23
    49
    45
    19
    38
    39
    11
    1
    32
    25
    35
    8
    17
    7
    9
    4
    2
    34
    10
    3
]
let puzzle = [
    165
    78
    151
    15
    138
    97
    152
    64
    4
    111
    7
    90
    91
    156
    73
    113
    93
    135
    100
    70
    119
    54
    80
    170
    139
    33
    123
    92
    86
    57
    39
    173
    22
    106
    166
    142
    53
    96
    158
    63
    51
    81
    46
    36
    126
    59
    98
    2
    16
    141
    120
    35
    140
    99
    121
    122
    58
    1
    60
    47
    10
    87
    103
    42
    132
    17
    75
    12
    29
    112
    3
    145
    131
    18
    153
    74
    161
    174
    68
    34
    21
    24
    85
    164
    52
    69
    65
    45
    109
    148
    11
    23
    129
    84
    167
    27
    28
    116
    110
    79
    48
    32
    157
    130
]

module Part1 =
    type State = { Ones: int; Threes: int; LastNumber: int }
    let solve list =
        list
        |> Seq.sort
        |> Seq.fold (
            fun state number ->
                match number - state.LastNumber with
                | 1 -> { state with Ones = state.Ones + 1; LastNumber = number }
                | 3 -> { state with Threes = state.Threes + 1; LastNumber = number }
                | _ -> failwith "Invalid Solution"
        ) { Ones = 0; Threes = 1; LastNumber = 0 }
        |> fun state -> state.Ones * state.Threes
    
module Part2 =
    type State = { Groups: int64 list list; CurrentList: int64 list; LastNumber: int64 }
    
    let solve arr =
        [0L] @ List.map int64 arr
        |> List.sort
        |> List.pairwise
        |> List.map (fun (a, b) -> b - a)
        |> List.fold
               (fun (list, count) number -> if number = 3L then (list @ [count], 1) else (list, count + 1))
               ([], 1)
        |> fun (list, count) -> list @ [count]
        |> Seq.map (function
            | 5 -> 7L
            | 4 -> 4L
            | 3 -> 2L
            | _ -> 1L)
        |> Seq.reduce (*)