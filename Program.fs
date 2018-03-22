let factorial n =
    List.fold (fun x acc -> acc * x) 1 [1..n]

let fibFunc num = 
    let rec fibRec f1 f2 curNum numOfFibb =
        if curNum = numOfFibb 
        then f1 
        else fibRec f2 (f1 + f2) (curNum + 1) numOfFibb  
    if (num >= 0 || (abs num) % 2 = 1) 
    then fibRec 0 1 0 (abs num)
    else -(fibRec 0 1 0 (abs num)) 

let reverseList list = 
    let rec reverse l1 l2 = 
        if List.length l1 = 0 
        then l2 
        else reverse (List.tail l1) (List.head l1 :: l2)
    reverse list []     


let twoAndPowList n m =
    let list = [System.Math.Pow(2.0, (n + m))]
    let rec massInit m list =
        if (m = 0.0)
        then list
        else massInit (m - 1.0) (list.Head / 2.0 :: list)
    massInit m list

[<EntryPoint>]
let main arg =
    let res1 = reverseList [1; 2; 4; 6; 77]
    let res2 = twoAndPowList -3.0 5.0
    let res5 = fibFunc -4
    let res3 = fibFunc -5
    let res4 = fibFunc 6
    0
