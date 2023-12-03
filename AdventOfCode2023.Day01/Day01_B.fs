module Day01_B

open System
open System.IO

let getResult data =
     
    let digitsList =
        // digits e.g. ("1", "1") ; ("2", "2") etc.
        ([ 1..9 ] |> List.map _.ToString() |> List.map (fun d -> d,d))
        @
        // words
        [
            "one", "1"
            "two", "2"
            "three", "3"
            "four", "4"
            "five", "5"
            "six", "6"
            "seven", "7"
            "eight", "8"
            "nine", "9"
        ]
    let digitsMap = digitsList |> Map.ofList
    let digitKeys = digitsList |> List.map fst
    
    let getDigits (str: string) =
        let rec loop (remaining: string) (collected: string) : string =
            //printfn "Remaining: %s Collected: %s" remaining collected
            if remaining.Length = 0 then
                collected
            else
                let remaining' = remaining.Remove(0,1)
                let collected' =
                    match digitKeys |> List.tryFind remaining.StartsWith with
                    | Some digit -> collected + (digitsMap |> Map.find digit)
                    | None -> collected
                loop remaining' collected'
        
        loop str String.Empty
         
    let values =
        data
        |> Array.map getDigits  
    
    let first (s: string) = s.Substring(0,1)        
    let last (s: string) = s.Substring(s.Length - 1, 1)
    let firstAndLast (s: string) =
        if String.IsNullOrWhiteSpace(s) then
            "0"
        else
            (s |> first) + (s |> last)
    
    let calibrationValues =
        values
        |> Array.map firstAndLast // combine first and last digit
        |> Array.map int

    // data
    // |> Array.iteri (fun idx line ->
    //     printfn "%i %s %A %A" (idx+1) line values[idx] calibrationValues[idx]
    //     //printfn "%i" calibrationValues[idx]
    //     )
        
    calibrationValues |> Array.sum

let runTests () =
    let sampleLines =
        File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day01_B_data_test.txt"))

    [
        sampleLines, 281, "Sample data"
        [| "one" |], 11, "One word only"
        [| "onetwo" |], 12, "Two words only"
        [| "onexsfsdfs" |], 11, "Word at start"
        [| "xsftwosdfs" |], 22, "Word in middle"
        [| "xsfsdfsone" |], 11, "Word at end"
        [| "xsfeightwosdfs" |], 82, "Overlapping words"
        [| "1" |], 11, "One digit only"
        [| "12" |], 12, "Two digits only"
        [| "1xsfsdfs" |], 11, "Digit at start"
        [| "xsf2sdfs" |], 22, "Digit in middle"
        [| "xsfsdfs1" |], 11, "Digit at end"
        [| "onetwothreefourfivesixseveneightnine" |], 19, "All words"
        [| "123456789" |], 19, "All digits"
        [| "one2three4five6seven8nine" |], 19, "Words and digits"
        [| "1two3four5six7eight9" |], 19, "Digits and words"
        [| "" |], 0, "Empty line"
        [| "sdflkjhgq" |], 0, "No digits or words"
        [| "4pxzmslsevengdqqv" |], 47, "Should be 47"
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
    printfn "Running Day01_B..."
    let filename = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day01_data.txt")    
    let data = File.ReadAllLines(filename)
    let allTestsPassed = runTests ()
    if allTestsPassed then
        printfn "All tests passed!"
    else
        printfn "Tests failed!"
    
    let result = data |> getResult      
    printfn $"Day 01 Part 2:- Result: %i{result}"
    
    () 
