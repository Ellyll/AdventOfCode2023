module Day07_B

open System
open System.Diagnostics
open System.IO

type CardType =
    {
        Label: char
        Strength: int
    }
    
type HandType =
    | FiveOfAKind = 7
    | FourOfAKind = 6
    | FullHouse = 5
    | ThreeOfAKind = 4
    | TwoPair = 3
    | OnePair = 2
    | HighCard = 1
 
let getHandTypeStrength (handType: HandType) =
        int handType       

 // A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, or 2.

let parseLine (line: string) =
    match line.Split(' ') with
    | [| cards ; bid |] ->
        let cardsParsed =
            cards
            |> Seq.map (fun c ->
                match c with
                | 'J' -> { Label = 'J' ; Strength = 1 }
                | '2' -> { Label = '2' ; Strength = 2 }
                | '3' -> { Label = '3' ; Strength = 3 }
                | '4' -> { Label = '4' ; Strength = 4 }
                | '5' -> { Label = '5' ; Strength = 5 }
                | '6' -> { Label = '6' ; Strength = 6 }
                | '7' -> { Label = '7' ; Strength = 7 }
                | '8' -> { Label = '8' ; Strength = 8 }
                | '9' -> { Label = '9' ; Strength = 9 }
                | 'T' -> { Label = 'T' ; Strength = 10 }
                | 'Q' -> { Label = 'Q' ; Strength = 11 }
                | 'K' -> { Label = 'K' ; Strength = 12 }
                | 'A' -> { Label = 'A' ; Strength = 13 }
                | _ -> failwithf $"Invalid character '%c{c}' in line: %s{line}"    
                )
            |> Seq.toList
        (cardsParsed, int bid) 
    | _ -> failwithf $"Invalid line: %s{line}"    

let parse (lines: string array) =
    lines |> Array.map parseLine

(*
    - Five of a kind, where all five cards have the same label: AAAAA
    - Four of a kind, where four cards have the same label and one card has a different label: AA8AA
    - Full house, where three cards have the same label, and the remaining two cards share a different label: 23332
    - Three of a kind, where three cards have the same label, and the remaining two cards are each different from any other card in the hand: TTT98
    - Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label: 23432
    - One pair, where two cards share one label, and the other three cards have a different label from the pair and each other: A23A4
    - High card, where all cards' labels are distinct: 23456
*)

let jokerPossibleCards =
    [
        //{ Label = 'J' ; Strength = 1 }
        { Label = '2' ; Strength = 2 }
        { Label = '3' ; Strength = 3 }
        { Label = '4' ; Strength = 4 }
        { Label = '5' ; Strength = 5 }
        { Label = '6' ; Strength = 6 }
        { Label = '7' ; Strength = 7 }
        { Label = '8' ; Strength = 8 }
        { Label = '9' ; Strength = 9 }
        { Label = 'T' ; Strength = 10 }
        { Label = 'Q' ; Strength = 11 }
        { Label = 'K' ; Strength = 12 }
        { Label = 'A' ; Strength = 13 }
    ]

let getHandType (hand: CardType list) =
    if Seq.length hand <> 5 then
        failwithf $"Invalid hand: %A{hand}"

    let j = { Label = 'J' ; Strength  = 1 }

    // get all possible types with jokers switching jokes out with other types
    
    let getHandTypeWithoutJoker (h: CardType list) =    
        let groups =
            h
            |> Seq.groupBy id
            |> Seq.map (fun (c,cs) -> c,(cs |> Seq.length))
            |> Seq.sortByDescending snd
            |> Seq.toList

        match groups with
        | [ (_,5) ] -> HandType.FiveOfAKind
        | [ (_,4) ; (_,1) ] -> HandType.FourOfAKind
        | [ (_,3) ; (_,2) ] -> HandType.FullHouse
        | [ (_,3) ; (_,1) ; (_,1) ] -> HandType.ThreeOfAKind
        | [ (_,2) ; (_,2) ; (_,1) ] -> HandType.TwoPair
        | [ (_,2) ; (_,1) ; (_,1) ; (_,1) ] -> HandType.OnePair
        | [ (_,1) ; (_,1) ; (_,1) ; (_,1) ; (_,1) ] -> HandType.HighCard
        | _ -> failwithf $"Invalid groups for hand: %A{hand}"

    let getAllPossibilitiesFromJoker h =
        if not (h |> List.contains j) then
            [ h ]
        else
            let withoutJ, js = h |> List.partition (fun c -> c <> j)
            let numOfJs = js |> List.length
            if withoutJ |> List.length = 0 then
                [ js ] // all jokers
            else
                withoutJ
                |> List.distinct
                |> List.map (fun c -> withoutJ @ (List.replicate numOfJs c))
    let possibilities = getAllPossibilitiesFromJoker hand
        
    possibilities
    |> List.map (fun h ->
        let handType = getHandTypeWithoutJoker h
        let typeStrength = getHandTypeStrength handType
        (h,  handType, typeStrength))
    |> List.sortByDescending (fun (_,_,strength) -> strength)
    |> List.head
    |> fun (_,t,_) -> t   

let compareHand hand1 hand2 =
    let typeStrength hand =
        hand |> getHandType |> getHandTypeStrength
        
    match (typeStrength hand1), (typeStrength hand2) with
    | a,b when a > b -> 1
    | a,b when a < b -> -1
    | a,b when a = b ->
        let rec loop remA remB =
            match remA,remB with
            | nextA::_,nextB::_ when nextA.Strength > nextB.Strength -> 1
            | nextA::_,nextB::_ when nextA.Strength < nextB.Strength -> -1
            | nextA::rA,nextB::rB when nextA = nextB ->
                // same, so loop again
                loop rA rB
            | _ -> 0 //failwithf $"Ran out of cards to compare: %A{hand1}, %A{hand2}"
        loop hand1 hand2
    | _ -> failwithf $"Error comparing typeStrength: %A{hand1}, %A{hand2}"
   
let getResult lines =        
    parse lines
    |> Array.sortWith (fun (a,_) (b,_) -> compareHand a b)
    |> Array.mapi (fun i (_,bid) -> int64 (bid*(i+1))) // Calc win amount
    |> Array.reduce (+)

    
let runTests () =
    let sampleLines =
        File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day07_data_test.txt"))

    [
        sampleLines, 5905, "Sample data"
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
    printfn $"%s{getNow()} Running Day07_B..."

    let allTestsPassed = runTests ()
    if allTestsPassed then
        printfn "All tests passed!"
        let filename = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day07_data.txt")    
        let data = File.ReadAllLines(filename)

        let stopwatch = Stopwatch.StartNew()
        let result = data |> getResult
        stopwatch.Stop()
        printfn $"%s{getNow()} Day 07 Part 2:- Result: %i{result} (Time taken: %d{stopwatch.ElapsedMilliseconds}ms)"
    else
        printfn "Tests failed!"

    () 
