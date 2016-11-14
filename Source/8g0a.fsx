(* The types *)
type codeColor = Red | Green | Yellow | Purple | White | Black

type code = codeColor list

type answer = int * int

type board = ( code * answer ) list

type player = Human | Computer
let mutable gameOver = false


(* Menu: askPlayerType *)
let rec askPlayerType () =
    printfn "Do you want to play as Human (H/h) or Computer (C/c)?"
    let p = (System.Console.ReadLine ()).ToLower ()
    if p = "h" || p = "human" then
        Human
    elif p = "c" || p = "computer" then
        Computer
    else
        printfn "Wrong input"
        askPlayerType ()
         
(* NumToCol: Helper function to getRndCode *)
let numToCol n =
    match n with
    | 0 -> Red
    | 1 -> Green
    | 2 -> Yellow
    | 3 -> Purple
    | 4 -> White
    | 5 -> Black

(* getRdnCode: Helper function to makeCode *)
let getRndCode ():code=
    let rnd = System.Random()
    let list = [numToCol (rnd.Next() % 6);numToCol (rnd.Next() % 6);numToCol (rnd.Next() % 6);numToCol (rnd.Next() % 6);]
//    [fun for _ in 1..4 -> (numToCol (rnd.Next() % 6))]
    list

(* stringToCol: Helper function to selCode *)
let stringToCol s =
    match s with
    | s when s="red" || s="r"    -> Some(Red)
    | s when s="green" || s="g"  -> Some(Green)
    | s when s="yellow" || s="y" -> Some(Yellow)
    | s when s="purple" || s="p" -> Some(Purple)
    | s when s="white" || s="w"  -> Some(White)
    | s when s="black" || s="b"  -> Some(Black)
    | _                          -> None

(* selCode: Helper function to makeCode *)
let selCode () =
    printfn "Possible colors: Red (r) | Green (g) | Yellow (y) | Purple (p) | White (w) | Black (b)"
    let colors = ["first color";"second color";"third color"; "fourth color"]
    let rec inputHelper i : code=
      if i > 3 then [] 
      else
        printfn "Input your %s" colors.[i]
        let c1 = (System.Console.ReadLine ()).ToLower ()
        match stringToCol c1 with
        | None -> inputHelper i
        | Some(c)    -> printfn "You have chosen %A" c;
                        [c] @ inputHelper (i+1)
    inputHelper 0

(*  Game: makeCode*)
let makeCode (p:player) =
  match p with
  | Human    -> selCode ()
  | Computer -> getRndCode ()

(* Game: guess *)
//guess skal modificeres til at kunne gøre noget med board - evt. foretage et bedre valg.
//evt. printBoard
let guess (p:player) (brd:board) : code =
  match p with
  | Human     -> selCode ()
  | Computer  -> getRndCode ()

(* validate: Part of the guess loop  *)
let validate (rc: code) (c: code) :answer =
  let mutable i   = 0
  let mutable b   = 0
  let mutable w   = 0
  let histC       = [|0;0;0;0;0;0|]
  let histRc      = [|0;0;0;0;0;0|]
  let mutable sum = 0
  for i=0 to 3 do 
    if rc.[i] = c.[i] then
      b <- b + 1
    
    match rc.[i] with
    | col when col = Red    -> histRc.[0]+1
    | col when col = Green  -> histRc.[1]+1
    | col when col = Yellow -> histRc.[2]+1
    | col when col = Purple -> histRc.[3]+1
    | col when col = White  -> histRc.[4]+1
    | col when col = Black  -> histRc.[5]+1
    
    match c.[i] with
    | col when col = Red    -> histC.[0]+1
    | col when col = Green  -> histC.[1]+1
    | col when col = Yellow -> histC.[2]+1
    | col when col = Purple -> histC.[3]+1
    | col when col = White  -> histC.[4]+1
    | col when col = Black  -> histC.[5]+1 
  
  for i=0 to 5 do
    sum <- sum + (min (histC.[i]) (histRc.[i]))
  
  w <- sum
  (b, w)
      

//validate (realCode, guess())

(* addGuess: Part of the guess loop *)
let addGuess (brd: board) (c: code) (a: answer):board=
  brd @ [(c,a)]
(* colPin: Helper function to printBoard *)
let colPin (c: codeColor) = 
  match c with
  | Red    -> "R"
  | Green  -> "G"
  | Yellow -> "Y"
  | Purple -> "P"
  | White  -> "W"
  | Black  -> "B"

(* printBoard: Part of the guess loop *)
let printBoard (brd: board) = 
  let edge:Printf.TextWriterFormat<_> = "-------------------------"
  let str:Printf.TextWriterFormat<_>  = "| %s | %s | %s | %s | %i - %i |"
  printfn edge
  for i in brd do
    let f j =  colPin <| (fst i).[j]
    let s = snd i
    printfn str (f 0) (f 1) (f 2) (f 3) (fst s) (snd s)
    printfn edge
  ()

(* isGameOver: Part of the guess loop *)
let isGameOver (a: answer) =
  if a = (4,0) then
    gameOver <- true 

let main () = 
    printfn "%A" list
    let p = askPlayerType ()
    let realCode = makeCode (p)
    let mutable board = []
    
    while not(gameOver) do 
      let currentGuess = guess (p) board
      let answer = validate (realCode) (currentGuess)
      board <- addGuess board currentGuess answer
      isGameOver (answer)
      printBoard (board)
main ()
