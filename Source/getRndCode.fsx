type codeColor =
    |Red | Green | Yellow | Purple | White | Black

type code = codeColor list

let numToCol n =
    match n with
    | 0 -> Red
    | 1 -> Green
    | 2 -> Yellow
    | 3 -> Purple
    | 4 -> White
    | 5 -> Black

let getRndCode ():code =
    let rnd = System.Random()
    [for i in 1..4 -> numToCol <| rnd.Next () % 6] 
 

    

  
