module Day01_A

open System
open System.IO

let run () =
    printfn "Running Day01_A..."
    let filename = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day01_data.txt")
    
    let data = File.ReadAllLines(filename)
    
    let valuesAsChars =
        data
        |> Array.map (fun line -> line |> Seq.filter (Char.IsDigit))
                        
    let values =
        valuesAsChars
        |> Array.map (fun cs ->
                        cs
                        |> Seq.map _.ToString()
                        |> Seq.toList
                        )

    let calibrationValues =
        values
        |> Array.map (fun xs -> (xs |> List.head) + (xs |> List.last)) // combine first and last digit
        |> Array.map (int)
        
    let result = calibrationValues |> Array.sum
    
    printfn $"Day 01 Part 1:- Result: %i{result}"
    
    () 
    
    
    

