﻿module AdventOfCode2020.Day3

let example =
    """
..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#
""" |> String.splitLines

let input =
    """
....#...##.#.........#....#....
#.......#...#...#.#............
#..#..#.#.##....#.#........#...
........##...................#.
........#...##...#.#.###.......
##............#...#.....#.##...
...........#....###...#.....#..
.......#......#..##..#.....#...
..#.#..#....#.........#...#..#.
.........##......#.....##..##..
........#.....#....#..##......#
..#..#.......#..............#..
.....#.#.......................
.#.#....#.........#............
.......#.....#.###.............
......##......#...........#..#.
.#...............##...#........
.....#..##........###.........#
#...........#..#............#..
.........#....#..#.#......#....
.......#.........#..##.........
.##.....#..................#...
....#............#.#....#.....#
..#....#...##....#...#.#...#...
..........#................#.#.
#...#.#.#.####..#.#..........#.
..#...#.##......#...........#..
..#.....#...#.......#......#..#
..............#.......#........
.#..#..........#.....#...#..#.#
#........#...#......#.......#..
#..................#...........
..#...#........#...#..#........
..............#.....#.....#..#.
#.#.......#..............##.##.
....#.#.....##....#...........#
......#....#...#..#.......#....
....#..#.#.....#..##.....#....#
...........#.......#.#.#.......
#.......#..##........#..#......
.........#.##..#..............#
...........#............###.#..
..#.....#.....##...#.........#.
....##............##........#..
.....###..........#......##....
#...##..#..#..........#........
....#.....#.......#..#.#...##..
.#....#........#.#.........#.#.
##...#.#.....#......#..........
.....##.....#....#.....###.#..#
..............#..###..#...#..#.
....#...#....#.............#.#.
.#.........#.....#........#.##.
....#.........#..........#.....
.......#........#.#.#..........
#........##....#.........#.....
..##..........#....#.#...#....#
#...#.#......#..##..........#.#
.....#..#...#..#...............
#...#..............#...........
.#...#....#..##.....#....#.#...
.#...#.......#...#..#.##....#..
#....#........#....#...#.......
#..#......#.....#.....#..##....
......#.#....##....##..#...#...
..#....#.#.###..............#..
.#.##.......#.#.#..#...#..#....
..#..........#.#....#..#.#....#
..........#...#...#..........#.
..........#.....#.#..#..#....##
.#.#...##...#...........####...
........##..#.#..........#.##.#
#......###...........#...#.....
..#.#....##.........##....#....
#....#.##..##..#..#.....#.....#
.##.....##....##....#.......#..
#...#.....##....#..........#...
............#.#.##....#....#...
....#............#.....#......#
....................#..........
..#....................#..#....
....#.....#........#..##...#...
#.....#.#....................##
.#....#.#.#...#..........#....#
....#...#......#...#.....##...#
.....#.........................
.......#..#.#...#...#...#.....#
...#......#.##.#...#..#...##.#.
...........................#..#
..#.#.....#........##..........
....#...##........#.#.#..#...##
..##.....#..###.........##.##..
.#..#.....#...#.............#..
#..............##...#....##....
.##......#.#............#......
.............##...#.#.......#..
.........#..#..#...............
........##......#....##........
...#.........#.#.#.............
#..........#......#......#..#..
.............##.#.#..#.#.#...#.
.....#.........#...............
..##.#..#.....##..#........#.#.
.#..........#.#.......#......##
.#........................#....
#....#....#...#..#......#......
........#.......#......#.....#.
.....#....##..#...###...#....#.
....#.........#....#......#....
.............#...#....#.......#
.....#.........#..#.#..........
.........#..#........#.#.#.....
......#.##......#....#.#.##.#..
.#...#.#...#...#.#......#....##
.#................#......#.....
#.#.#...............#..........
.....#.#.......#...#........#..
#...#.#.#.##..#...........#..#.
.............###.........#....#
.#.....#.......##....##.......#
....#...#.......#.##.....#.....
...........##.........#...#....
..............#.#..#.....#..#..
#.#...#..#.#.........#......#.#
#.##.....##....#........#.#.#.#
##.#.###.........##.......#..#.
#.....#.....................#..
.........##........#...........
.###........##....#...#........
....#.#........##...........#..
..........#.....#..........#..#
......#..............#......#..
.....#...#......#...#...#......
..........#.#..#....#...#..###.
#..##........#................#
..#............................
.....#.........#.#.............
........#...#.....#...##......#
..#........#................#..
......#....#..#......#.........
...........##....#..#.#........
.....#.............###.........
#............#......#..#.......
..#..#.................#..#..##
.......#......#.....#........#.
....................#..#.##...#
.#..##...............##...#....
...#...#....#........#.........
.....##...#.....###............
.###.........#........#.....##.
.............#...#.............
...#.#...............#..##..#.#
...#...............#..#.....#..
....#.#..................#...#.
..........#...........#.#...###
#...#......#................#..
...#.#.......#...#......#.##...
......#..........#.............
##.......#.##.#...........#....
......#...#.#.....#............
.#.....#.....#.....#.........#.
..................#............
.#.#.#.....#......#.##.........
.......#..##.##......#..#....#.
...#.#.#......#...#........#...
..#............#......#.......#
..#......#........#.........#..
..#..#.#.#.....#.............#.
..#.#..##......#...#...##......
.##...#....##.#.#...........#..
..............#..#...#....#....
.......#.#........#............
.....##..###........#..........
......................#........
..##....#....#.................
.##.#.###.#........#.##..#...#.
##................#...........#
....#..##.....##...............
.#.....#..#............#.....#.
#.........#..............#.....
...##.#......#...#.............
................#..............
...#.....#....##...#..#....#...
..............##..#...#.##..#..
......................#..#....#
.......#....#..#.##.........#.#
#...#........##.......#........
...##...............#.....#....
.##...##...#...................
.........##.#...#.........#....
............#............#..#..
.............................#.
....#.#....#...................
......#......#...#..##.........
#........#.#.#.#.#......#....#.
.#.........#.#...#......#..#.#.
..............#....##.........#
.#.......#..#....#.#.#....#....
...###.#.#..#...#....#....#....
#........#....................#
......#...##.###..#..##...#....
.....#........#.......#........
#..#...........#.#.............
....##.#...#..##............##.
#.#..##..#...#...#.....#.......
..#.............#.##..#...#.##.
.#.....##.#..#...#...........#.
....#...#....................##
....##......#.###......#......#
...#...#.........#..#.##....#..
#......#..#....###.........#...
#...........##.............#.#.
#..............##....#......#..
.........#...#.#...#...#.......
....#....#............#.......#
........#...#....#......##.....
..........#.#..#.........#.....
#........#.##....##......#.....
...#.......#...................
###...#...#..#.##....#.....#...
........##..........#.##..#....
.....#......#..#.....#.....#.#.
...#..#..##..###.....##.#......
#..#......##...#............#..
#............#....#..#.........
#........#.......#......#..##.#
...#.#.........#.#.............
#..............#..............#
#.#......#..........##.........
#..##...........#..##...#......
.....#.#.....#......#.....#.#.#
.#.##...#...##...........#....#
#.............#........#.......
..##.............#...#.........
....#.#......###....#..........
...#..#.....#..##.#....#...#.#.
.............##................
#.#............#........#..#.#.
.#......#.....#...........#....
...#.........#...........#.##..
.....#...#.....#..#..........#.
........#.#...............#.#..
.......#..#..#.....#.......##..
.#...#...#..#...##...#.........
..........##....#..#.##..#.....
....#.................#...#....
.........#...#......#....#....#
.........#..#...#.##........##.
#.#....##.......#.#............
##.......##..................#.
......#...#......##............
##.#...#.#...........#..#......
.........#.........#..#.#...#..
.#...#.......#.#...###.........
................#.#.....#......
..#...#.....#........#.........
.........##.###.#.#.....#...#..
#..#..........#....#.#...#...##
##.#.#....#..##.............#.#
.###....#..#...............##..
............#......#.#.#....#..
........#...#..#...#...........
##.........#................#..
...###...#.#..#...#..........##
...#......#......##........#...
.......#............#..........
.....#.....##....#.....###.....
.#...#...#.....#..#..#....#..#.
#.#........#..#.......##...#.##
.....#.....##..#.##........#..#
.....#...#...........#.........
..#....#.#...#..#....##...#...#
...........#...##.........#....
..#....#....##........#.####...
#.............#.#.............#
...................#.....#.#..#
.#....#.#.............#.#......
#...........#............#.#...
..#.........#.#....#.......##..
#....####......#...#......#....
....##....#...................#
....#.##....#.............#....
.........##........#.....#..#..
............#...#..............
............#..##....#.....##.#
............#.....#......#.....
........#..#........##.#.......
...#.#........#..............#.
............#.........#..#.#...
................#.............#
..##..........##......#.#......
..#..#.##....#.........#...#...
...........##...#.#.#..........
.#.#.......#.#...#.........#...
.........#..#........#..#.#....
..........##..#.##....#....#...
....#...............#.......#..
##..........##.................
....#.#.#.....#..........##.#..
..............#.##..........##.
##...............#...#..#......
..#..#..........#......#.......
#...#..##.#.#..................
....#....##......##.#...#....##
.#...#.#....##.............#..#
................#......###.....
..#..#.............#.#.......#.
..#..................#.......#.
.....#.......#....#.##...#.##..
.....##.......#......#..#......
#..#.......#........#..........
..#...#..#....#.........#......
#..#..#......##..#.##....####..
......##.#.....#..#.......#....
.##...#.....#..#...#.#.........
#.....#........###....#...#..#.
.#....#.#..#......#............
.........#..#..#.....#........#
..#.......#..........#..#......
......#.......##.#....#.#.#....
.#............#.....#.......#..
...#..#...............#........
.....#.........................
""" |> String.splitLines

let traverse down right lines =
    lines
    |> Seq.skip down
    |> Seq.indexed
    |> Seq.choose (fun (index, item) -> if index % down = 0 then Some(item) else None)
    |> Seq.fold (fun (position, count) line ->
        let position = (position + right) % String.length line
        let count = count + if line.[position] = '#' then 1 else 0
        (position, count)
    ) (0, 0)
    |> snd
        
let part1 sequence =
    traverse 1 3 sequence
    |> printfn "%i"

part1 example    
part1 input

let part2 sequence =
    seq {
        traverse 1 1 sequence
        traverse 1 3 sequence
        traverse 1 5 sequence
        traverse 1 7 sequence
        traverse 2 1 sequence
    }
    |> Seq.reduce (*)
    |> printfn "%i"

part2 example
part2 input