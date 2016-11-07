type player = Human | Computer

let rec askPlayerType () =
    printfn "Do you want to play as Human (H/h) or Computer (C/c)?"
    let p = (System.Console.ReadLine ()).ToLower ()
    if p = "h" or p = "human" then
        Human
    elif p = "c" or p = "computer" then
        Computer
    else
        printfn "Wrong input"
        askPlayerType ()
         
