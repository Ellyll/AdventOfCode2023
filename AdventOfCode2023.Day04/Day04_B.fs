module Day04_B

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

let getWinners cardId (data: (Set<int>*Set<int>) array)  =
    let winners, mine = data[cardId-1]
    let numberWon = mine |> Set.intersect winners |> Set.count
    if numberWon = 0 then
        []
    else
        [ (cardId+1)..(cardId+1+numberWon-1) ]
            
let getTotalWon cardIds data =
    let rec loop cIds =    
        cIds
        |> List.fold (fun state cId ->
            let cardsWon = getWinners cId data
            (loop cardsWon) @ cardsWon @ state
            ) []
    
    (loop cardIds |> List.length) + (List.length cardIds)
    
let getResult lines =
    let data =
        lines
        |> Array.map parseLine

    getTotalWon [ 1..(data |> Array.length) ] data

    
let runTests () =
    let sampleLines =
        File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day04_data_test.txt"))

    [
        sampleLines, 30, "Sample data"
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
    printfn "Running Day04_B..."
    let filename = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day04_data.txt")    
    let data = File.ReadAllLines(filename)
    let allTestsPassed = runTests ()
    if allTestsPassed then
        printfn "All tests passed!"
    else
        printfn "Tests failed!"
    
    let result = data |> getResult      
    printfn $"Day 04 Part 2:- Result: %i{result}"
    
    () 
