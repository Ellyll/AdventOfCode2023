module Day06_A

open Microsoft.FSharp.Data.UnitSystems.SI
open System
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions

[<Measure>] type ms
[<Measure>] type mm

let regRaces = Regex(@"^\S*:\s*(.*)$")
let regWhiteSpace = Regex(@"\s+")    
let parse (lines: string array) =
    let raceTimes =
        regWhiteSpace.Replace(regRaces.Replace(lines[0], "$1"), " ").Split(' ')
        |> Array.map (fun t -> (int t)*1<ms>)
    let raceDistances =
        regWhiteSpace.Replace(regRaces.Replace(lines[1], "$1"), " ").Split(' ')
        |> Array.map (fun d -> (int d)*1<mm>)
    raceDistances
    |> Array.zip raceTimes

(*
 Distance: 9 Time: 7
 
 Hold for 1ms we get speed of 1mm/ms = 6ms at 1mm/ms = 6mm
 Hold for 2ms: speed: 2mm/ms, so 5ms @ 2mm/ms = 10ms*
 Hold for 3ms: speed: 3mm/ms, so 4ms @ 3mm/ms = 12ms*
 Hold for 4ms: speed: 4mm/ms, so 3ms @ 4mm/ms = 12ms*
 Hold for 5ms: speed: 5mm/ms, so 2ms @ 5mm/ms = 10ms*
 Hold for 6ms: speed: 6mm/ms, so 1ms @ 6mm/ms = 6ms

*)

let getNumberOfWaysToWin (time: int<ms>,distance: int<mm>) =
    seq { 1..(int(time)-1) }
    |> Seq.map (fun t -> t*1<ms>)
    |> Seq.choose (fun t ->
        let speed = (int t)*1<mm/ms>
        let travelTime = ((int time) - (int t))*1<ms>
        let d = travelTime * speed
        if d > distance then
            Some d
        else
            None
        )
    |> Seq.length

let getResult lines =
    let races = parse lines
    races
    |> Array.map getNumberOfWaysToWin
    |> Array.reduce (*)
  
    
let runTests () =
    let sampleLines =
        File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day06_data_test.txt"))

    [
        sampleLines, 288, "Sample data"
    ]
    |> Seq.fold (fun allTestsPassed (lines, expected, name) ->
        let actual = getResult lines
        if actual <> expected then
            printfn "Failed: %s (Expected=%i, Actual=%i)" name expected actual
            false
        else
            //printfn "Passed: %s" name
            allTestsPassed && true
        ) true

let getNow () =
    DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss")

let run () =
    printfn $"%s{getNow()} Running Day06_A..."

    let allTestsPassed = runTests ()
    if allTestsPassed then
        printfn "All tests passed!"
    else
        printfn "Tests failed!"

    let filename = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day06_data.txt")    
    let data = File.ReadAllLines(filename)

    let stopwatch = Stopwatch.StartNew()
    let result = data |> getResult
    stopwatch.Stop()
    printfn $"%s{getNow()} Day 06 Part 1:- Result: %i{result} (Time taken: %d{stopwatch.ElapsedMilliseconds}ms)"
    
    () 
