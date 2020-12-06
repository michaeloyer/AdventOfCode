module AdventOfCode2020.Utils

open System

let splitLines (str:string) = str.Split([|'\n'; '\r'|], StringSplitOptions.RemoveEmptyEntries)