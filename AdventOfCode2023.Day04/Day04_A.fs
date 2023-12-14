module Day04_A

open System
open System.IO
open System.Text.RegularExpressions

let regLine = Regex(@"^(Card\s.*:\s+)(\d+.*)\s+\|\s+(\d+.*)$")
let parseLine (line: string) =
    if not (regLine.IsMatch(line)) then
        failwithf $"Invalid line: %s{line}"
    let winners =
        regLine.Replace(line, "$2").Split(" ")
        |> Array.filter (fun s -> s.Length > 0)
        |> Array.map int
        |> Set.ofArray
    let myNumbers =
        regLine.Replace(line, "$3").Split(" ")
        |> Array.filter (fun s -> s.Length > 0)
        |> Array.map int
        |> Set.ofArray
    winners, myNumbers

    
let getResult lines =
    lines
    |> Array.map parseLine
    |> Array.map (fun (winners, mine) ->
        (2.0 ** ((mine |> Set.intersect winners |> Set.count |> double) - 1.0))
        |> int
        )
    |> Array.sum

    
let runTests () =
    let sampleLines =
        File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day04_data_test.txt"))

    [
        sampleLines, 13, "Sample data"
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

let run () =
    printfn "Running Day04_A..."
    let filename = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day04_data.txt")    
    let data = File.ReadAllLines(filename)
    let allTestsPassed = runTests ()
    if allTestsPassed then
        printfn "All tests passed!"
    else
        printfn "Tests failed!"
    
    let result = data |> getResult      
    printfn $"Day 04 Part 1:- Result: %i{result}"
    
    () 
