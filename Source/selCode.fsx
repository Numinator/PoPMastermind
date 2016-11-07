type codeColor =
    |Red | Green | Yellow | Purple | White | Black

type code = codeColor list

let stringToCol s =
    match s with
    | s when s="red" || s="r"    -> Some(Red)
    | s when s="green" || s="g"  -> Some(Green)
    | s when s="yellow" || s="y" -> Some(Yellow)
    | s when s="purple" || s="p" -> Some(Purple)
    | s when s="white" || s="w"  -> Some(White)
    | s when s="black" || s="b"  -> Some(Black)
    | _                          -> None

let rec selCode () =
    printfn "Possible colors: Red (r) | Green (g) | Yellow (y) | Purple (p) | White (w) | Black (b)"
    let colors = ["first color";"second color";"third color"; "fourth color"]
    let rec inputHelper i : code=
      if i > 4 then [] 
      else
        printfn "Input your %s" colors.[i]
        let c1 = (System.Console.ReadLine ()).ToLower ()
        match stringToCol c1 with
        | None -> inputHelper i
        | Some(c)    -> printfn "You have chosen %A" c;
                        [c] @ inputHelper (i+1)
    inputHelper 1
    
  