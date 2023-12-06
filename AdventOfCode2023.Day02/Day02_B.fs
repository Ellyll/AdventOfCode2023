module Day02_B

open System
open System.IO
open System.Text.RegularExpressions

type Cube =
    | Red
    | Green
    | Blue

type Reveal = Map<Cube,int>

type Game =
    {
        Id: int
        Revealed: Reveal list 
    }

let regGame = Regex(@"^Game\s+(\d+):(.*$)")
let parse (line: string) =
    if not (regGame.IsMatch(line)) then
        failwithf $"Unable to parse line: %s{line}"
    
    let gameId = regGame.Replace(line, "$1") |> int
    let reveals =
        regGame.Replace(line, "$2").Split(";")
        |> Array.map (fun rev ->
            rev.Split(",")
            |> Array.fold (fun map x ->
                let num, cube =
                    match x.Trim().Split(" ") with
                    | [| n ; "red" |] -> (int n), Red
                    | [| n ; "green" |] -> (int n), Green
                    | [| n ; "blue" |] -> (int n), Blue
                    | _ -> failwithf $"Invalid data: %s{x}"
                map |> Map.add cube num
                ) Map.empty
            )
        |> Array.toList        
    { Id = gameId ; Revealed = reveals }

let getPower maxReveal =
    maxReveal
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.reduce (*)

let getResult lines =
    let data =
        lines
        |> Array.map parse

    let emptyReveal = [ (Red, 0) ; (Green, 0) ; (Blue, 0) ] |> Map.ofList
    let cubes = emptyReveal |> Map.keys
    data
    |> Array.map (fun game ->
        // Get max of each colour and check against requirements
        let maxReveals =
            game.Revealed
            |> List.fold (fun mMax rev ->                           
                cubes
                |> Seq.fold (fun m cube ->
                    let n = rev |> Map.tryFind cube |> Option.defaultValue 0
                    let current = m |> Map.tryFind cube |> Option.defaultValue 0
                    if n > current then
                        m |> Map.remove cube |> Map.add cube n
                    else
                        m
                    ) mMax                
                ) emptyReveal
        maxReveals |> getPower
        )
    |> Array.sum
    
let runTests () =
    let sampleLines =
        File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day02_A_data_test.txt"))

    [
        sampleLines, 2286, "Sample data"
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
    printfn "Running Day02_B..."
    let filename = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day02_data.txt")    
    let data = File.ReadAllLines(filename)
    let allTestsPassed = runTests ()
    if allTestsPassed then
        printfn "All tests passed!"
    else
        printfn "Tests failed!"
    
    let result = data |> getResult      
    printfn $"Day 02 Part 2:- Result: %i{result}"
    
    () 
