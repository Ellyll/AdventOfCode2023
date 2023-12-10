module Day03_A

open System
open System.IO
open System.Text.RegularExpressions

type Item =
    | Digit of char
    | Symbol of char
    | Empty

let parseLine (line: string) =
    line
    |> Seq.map (fun c ->
        match c with
        | '.' -> Empty
        | _ when (Char.IsDigit c) ->
            Digit c //(System.Globalization.CharUnicodeInfo.GetDigitValue(c))
        | _ -> Symbol c
        )
        // | _ ->                     
        //     failwithf $"Unable to parse line: %s{line}, invalid character: %c{c}"
    |> Seq.toArray

let parseLines (lines: string seq) =
    let rows =
        lines
        |> Seq.map (parseLine)
        |> Seq.toArray
        
    let numberOfColumns = rows[0] |> Array.length
    let numberOfRows = rows |> Array.length
    Array2D.init numberOfRows numberOfColumns (fun row col -> rows[row][col])    
 
let hasAdjacentSymbol (r,c) data =
    let adjacents =
         [
             r-1, c-1 ; r-1, c ; r-1, c+1
             r, c-1 ; r, c+1
             r+1, c-1 ; r+1, c ; r+1, c+1
         ]
         |> List.filter (fun (row, col) ->
                // only include those in bounds
                row >= 0 &&
                col >= 0 &&
                row < (Array2D.length1 data) &&
                col < (Array2D.length2 data)
                )         
    adjacents
    |> List.exists (fun (row, col) ->
             match data[row,col] with
             | Symbol _ -> true
             | _ -> false
             )
    
let getResult lines =
    let data = lines |> parseLines
    let maxRowIndex = (data |> Array2D.length1) - 1
    let maxColumnIndex = (data |> Array2D.length2) - 1
    
    // Find location of all numbers
    let locationsOfNumbers =
        seq { 0..maxRowIndex }
        |> Seq.fold (fun found r ->
                seq { 0..maxColumnIndex }
                |> Seq.fold (fun (ns, buffer) c ->
                    match data[r,c] with
                    | Digit _ ->
                        match buffer with
                        | Some (loc,len) -> ns, Some (loc, len + 1)
                        | None -> ns, Some ((r,c), 1) // current location with length of 1                   
                    | _ ->
                        // flush buffer if it contains a number we've found
                        match buffer with
                        | None -> ns,None
                        | Some x -> x::ns,None
                    ) (found, None)
                    // number so far, either None or Some of starting co-rds of number and length so far
                |> function
                    | ns, Some x -> x::ns
                    | ns, None -> ns
            
            ) []

    let numbersWithSymbols =
        locationsOfNumbers
        |> List.filter (fun ((rStart,cStart),len) ->
            let digitLocations =
                seq { cStart..(cStart+len-1) }
                |> Seq.map (fun c -> rStart,c)
            digitLocations
            |> Seq.exists (fun (r,c) -> hasAdjacentSymbol (r,c) data)
            )
    
    let numbers =
        numbersWithSymbols
        |> List.map (fun ((rStart,cStart),len) ->
            let digits =
                seq { cStart..(cStart+len-1) }
                |> Seq.map (fun c ->
                    match data[rStart,c] with
                    | Digit d -> d
                    | x -> failwithf $"Invalid item %A{x} at [{rStart},{c}]"
                    )
                |> Seq.toArray
            String(digits) |> int                
            )
    
    numbers
    |> List.sum
  
    
let runTests () =
    let sampleLines =
        File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day03_data_test.txt"))

    [
        sampleLines, 4361, "Sample data"
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
    printfn "Running Day03_A..."
    let filename = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day03_data.txt")    
    let data = File.ReadAllLines(filename)
    let allTestsPassed = runTests ()
    if allTestsPassed then
        printfn "All tests passed!"
    else
        printfn "Tests failed!"
    
    let result = data |> getResult      
    printfn $"Day 03 Part 1:- Result: %i{result}"
    
    () 
