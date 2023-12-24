module Day05_B

open System
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions

type RangeMapping =
    {
        DestinationStart: int64
        SourceStart: int64
        Length: int64
    }

type CategoryMapping =
    {
        Source: string
        Destination: string
        RangeMappings: RangeMapping list
    }
    
 type Range =
    {
        Start: int64
        Length: int64
    }

let regCatMap = Regex(@"^([a-z]+)-to-([a-z]+) map:$")
    
let parse (lines: string array) =
    let seedRanges =
        lines[0].Replace("seeds: ", "").Split(' ')
        |> Array.map int64
        |> Array.chunkBySize 2
        |> Array.map (fun arr ->
            match arr with
            | [| s ; l |] -> { Start = s ; Length = l }
            | _ -> failwithf $"Invalid range: %A{arr}"
            )
    
    let categoryMappings =
        seq { 2..((Array.length lines) - 1) }
        |> Seq.fold (fun (catMaps,currCatMap) idx ->
            let line = lines[idx]
            // DO THE STUFFS!3
            match currCatMap with
            | Some curr ->
                if line.Length > 0 then
                    // add a range mapping
                    let rangeMapping =
                        match line.Split(' ') |> Array.map int64 with
                        | [| destination ; source ; length |] ->
                            {
                                DestinationStart = destination
                                SourceStart = source
                                Length = length
                            }
                        | _ -> failwithf $"Invalid line format: %s{line}"
                    let curr' = { curr with RangeMappings = curr.RangeMappings @ [ rangeMapping ] }
                    catMaps, Some curr'
                else
                    // empty line, add currentCatMap to list
                    curr::catMaps, None
            | None -> // do stuffs
                if not (regCatMap.IsMatch(line)) then
                    failwithf $"Invalid line format: %s{line}"            
                let curr =
                    {
                        Source = regCatMap.Replace(line, "$1")
                        Destination = regCatMap.Replace(line, "$2")
                        RangeMappings = [] 
                    }
                catMaps, Some curr                
            ) ([],None)
        |> function
            | maps, None -> maps
            | maps, Some m -> m::maps
        |> List.rev
    seedRanges, categoryMappings
    
let inRange n rangeMap =
    n >= rangeMap.SourceStart &&
    n < rangeMap.SourceStart + rangeMap.Length
    
let getSeedLocation seed categoryMappings =
    categoryMappings
    |> Seq.fold (fun curr catMap ->
        match catMap.RangeMappings |> List.tryFind (inRange curr) with
        | Some rangeMap ->
            let offset = rangeMap.DestinationStart - rangeMap.SourceStart
            curr + offset
        | None -> curr       
        ) seed
    
let getResult lines =
    // This is brute forcing it, not optimal - takes about 36 minutes on my Ryzen 5900X
    let seedRanges, categoryMappings = parse lines
    seedRanges
    |> Seq.collect (fun r -> seq { r.Start..(r.Start+r.Length-1L) } )
    |> Seq.map (fun seed -> getSeedLocation seed categoryMappings)
    |> Seq.min
  
    
let runTests () =
    let sampleLines =
        File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day05_data_test.txt"))

    [
        sampleLines, 46, "Sample data"
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
    printfn $"%s{getNow()} Running Day05_B..." 

    let allTestsPassed = runTests ()
    if allTestsPassed then
        printfn "All tests passed!"
    else
        printfn "Tests failed!"

    let filename = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day05_data.txt")    
    let data = File.ReadAllLines(filename)
    
    let stopwatch = Stopwatch.StartNew()
    let result = data |> getResult
    stopwatch.Stop()
    printfn $"%s{getNow()} Day 05 Part 2:- Result: %i{result} (Time taken: %d{stopwatch.ElapsedMilliseconds}ms)"
    
    () 
