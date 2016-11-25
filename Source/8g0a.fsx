(* The types *)
type codeColor = Red | Green | Yellow | Purple | White | Black

type code = codeColor list

type answer = int * int

type board = ( code * answer ) list

type player = Human | Computer


(* Global varibles*)
let mutable GGameOver = false
let mutable GSelector  = 0
let mutable GCommit = false
let GCode = [|0; 0; 0; 0|]




/// <summary>
///    Converts the numbers 1-7 to its equivalent colour.
/// </summary>
/// <remarks>
///   The function cannot handle integer input larger than 5 or less than 0.
/// </remarks>
/// <param name="n">
///    An integer from 0 to 5, inclusive
/// </param name="n">
/// <returns>
///   Returns None if the inputted integer is anything but 1-7
/// </returns>
(* NumToCol: Helper function to getRndCode *)
let numToCol n =
    match n with
    | 0 -> Red
    | 1 -> Green
    | 2 -> Yellow
    | 3 -> Purple
    | 4 -> White
    | 5 -> Black

let colToNum c = 
  match c with
  | Red    -> 0
  | Green  -> 1
  | Yellow -> 2
  | Purple -> 3
  | White  -> 4
  | Black  -> 5



/// <summary>
///    Creates a pesudo random code of length 4.
/// </summary>
/// <remarks>
///   Takes unit as input.
/// </remarks>
/// <returns>
///   Returns a pesudo random code of length 4 of any colour type.
/// </returns>
(* getRdnCode: Helper function to makeCode *)
let getRndCode ():code=
    let rnd = System.Random()
    let list = [numToCol (rnd.Next() % 6);numToCol (rnd.Next() % 6);
                numToCol (rnd.Next() % 6);numToCol (rnd.Next() % 6);]
//    [fun for _ in 1..4 -> (numToCol (rnd.Next() % 6))]
    list


/// <summary>
///    Converts string to its equivalent colour.
/// </summary>
/// <remarks>
///   The function is case sensetive and compares with all lowercase
///   strings. The format of the string being compared to is: "colour"
///   || "c", where colour is any of the colours in colour-type
/// </remarks>
/// <param name="s">
///   A string.
/// </param name="s">
/// <returns>
///   Returns None if the input does not match the a colour; else a colour
///   encapsulated in Some.
/// </returns>
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



/// <summary>
///    Asks for at player-type from the user.
/// </summary>
/// <remarks>
///   Asks again recursivly if user gives wrong input. Takes unit as input.
/// </remarks>
/// <returns>
///   Returns a player type (either Human or Computer).
/// </returns>
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


/// <summary>
///    Ask the user for a change to the input code or selector possition. 
/// </summary>
/// <remarks>
///   Dependes on the global varibles GCommit and GSelector
/// </remarks>
/// <returns>
///   Returns a valid code.
/// </returns>
(* selCode: Helper function to makeCode *)
let selCode () : code =
    GSelector <- GSelector % 4
    let mutable bRun = true
    while bRun do
      let input = string <| (System.Console.ReadKey true).Key
      match input.ToLower() with
      | "c" -> GCommit <- true; bRun <- false
      | "j" -> GSelector <- (GSelector - 1) % 4; bRun <- false
      | "l" -> GSelector <- (GSelector + 1) % 4; bRun <- false
      | "i" -> GCode.[GSelector] <- (GCode.[GSelector] + 5) % 6; bRun <- false
      | "k" -> GCode.[GSelector] <- (GCode.[GSelector] + 1) % 6; bRun <- false
      | "r" -> GCode.[GSelector] <- 0; GSelector <- (GSelector + 1) % 4; bRun <- false
      | "g" -> GCode.[GSelector] <- 1; GSelector <- (GSelector + 1) % 4; bRun <- false
      | "y" -> GCode.[GSelector] <- 2; GSelector <- (GSelector + 1) % 4; bRun <- false
      | "p" -> GCode.[GSelector] <- 3; GSelector <- (GSelector + 1) % 4; bRun <- false
      | "w" -> GCode.[GSelector] <- 4; GSelector <- (GSelector + 1) % 4; bRun <- false
      | "b" -> GCode.[GSelector] <- 5; GSelector <- (GSelector + 1) % 4; bRun <- false
      | _   -> ()
    
    Array.map numToCol GCode |> Array.toList 
    


/// <summary>
///    Takes a player type and returns a code by the appropriate means.
/// </summary>
/// <remarks>
///   Uses selCode or getRndCode to get the code
/// </remarks>
/// <param name="p">
///    Player type of either Human or Computer
/// </param name="p">
/// <returns>
///   Returns a code.
/// </returns>
(*  Game: makeCode*)
let makeCode (p:player) =
  match p with
  | Human    -> selCode ()
  | Computer -> getRndCode ()


/// <summary>
///    Takes a player type and returns a code by the appropriate means.
/// </summary>
/// <remarks>
///   Uses selCode or getRndCode to get the code
/// </remarks>
/// <param name="p">
///    Player type of either Human or Computer
/// </param name="p">
/// <returns>
///   Returns a code.
/// </returns>
(* Game: guess *)
//guess skal modificeres til at kunne gøre noget med board - evt. foretage et bedre valg.
//evt. printBoard
let guess (p:player) (brd:board) : code =
  match p with
  | Human     -> selCode ()
  | Computer  -> getRndCode ()


/// <summary>
///    Takes 2 codes and generates an answer.
/// </summary>
/// <remarks>
///   The function is "symetric", so it does not matter in wich order the codes
///   are given.
/// </remarks>
/// <param name="rc">
///    A code, from either of the players.
/// </param name="rc">
/// <param name="c">
///    The other code from either the players.
/// </param name="c">
/// <returns>
///   Returns an anwser with no overlap in the white and black.
/// </returns>
(* validate: Part of the guess loop  *)
let validate (rc: code) (c: code) :answer =
  let mutable i   = 0
  let mutable b   = 0
  let histC       = [|0;0;0;0;0;0|]
  let histRc      = [|0;0;0;0;0;0|]
  let mutable sum = 0
  for i=0 to 3 do
    if rc.[i] = c.[i] then
      b <- b + 1

    match rc.[i] with
    | Red    -> histRc.[0] <- histRc.[0]+1
    | Green  -> histRc.[1] <- histRc.[1]+1
    | Yellow -> histRc.[2] <- histRc.[2]+1
    | Purple -> histRc.[3] <- histRc.[3]+1
    | White  -> histRc.[4] <- histRc.[4]+1
    | Black  -> histRc.[5] <- histRc.[5]+1

    match c.[i] with
    | Red    -> histC.[0] <- histC.[0]+1
    | Green  -> histC.[1] <- histC.[1]+1
    | Yellow -> histC.[2] <- histC.[2]+1
    | Purple -> histC.[3] <- histC.[3]+1
    | White  -> histC.[4] <- histC.[4]+1
    | Black  -> histC.[5] <- histC.[5]+1

  for i=0 to 5 do
    sum <- sum + (min (histC.[i]) (histRc.[i]))

  (b, sum - b)


/// <summary>
///    Appends a code and an answer to a board.
/// </summary>
/// <param name="brd">
///    The board where the answer and code gets appended to.
/// </param name="brd">
/// <param name="c">
///    The code that gets appended.
/// </param name="c">
/// <param name="a">
///    The answer that gets appended.
/// </param name="a">
/// <returns>
///   Returns a board where the code and answer have been appended to.
/// </returns>
(* addGuess: Part of the guess loop *)
let addGuess (brd: board) (c: code) (a: answer):board=
  brd @ [(c,a)]


/// <summary>
///   Takes a colour and returns the first letter of the colour name as a string
/// </summary>
/// <param name="c">
///    The colour used.
/// </param name="c">
/// <returns>
///   Returns a string that is the frist letter of the colour name.
/// </returns>
(* colPin: Helper function to printBoard *)
let colPin (c: codeColor) =
  match c with
  | Red    -> "\x1b[31;1m\u262d\x1b[37;0m"
  | Green  -> "\x1b[32;1m\u2623\x1b[37;0m"
  | Yellow -> "\x1b[33;1m\u2622\x1b[37;0m"
  | Purple -> "\x1b[35;1m\ud83c\udf03\x1b[37;0m" 
  | White  -> "\x1b[37;1m\u2620\x1b[37;0m" 
  | Black  -> "\x1b[30;1m\u26f0\x1b[37;0m" 

    



/// <summary>
///    Takes a board and makes a string in a formated way, that represents the board.
/// </summary>
/// <param name="brd">
///    The board that gets printed.
/// </param name="brd">
/// <returns>
///   Returns a string.
/// </returns>
(* printBoard: Part of the guess loop *)
let printBoard (brd: board) =
  let mutable brdStr = "" 
  let edge = "-------------------------"
  let str: Printf.StringFormat<_>  = "[| %s | %s | %s | %s | %i - %i |"
  brdStr <- edge + "\n"
  for i in brd do
    let f j =  colPin (fst i).[j]
    let s = snd i
    brdStr <- brdStr + (sprintf str (f 0) (f 1) (f 2) (f 3) (fst s) (snd s)) + "\n"
    brdStr <- brdStr + edge + "\n"
  brdStr


/// <summary>
///    Finds out if the game is over and changes GGameOver to true if it is.
/// </summary>
/// <remarks>
///   GGameOver is a global varible that must be present.
/// </remarks>
/// <param name="a">
///    Answer that is used to check if the game is over.
/// </param name="a">
/// <returns>
///   Returns unit.
/// </returns>
(* isGameOver: Part of the guess loop *)
let isGameOver (a: answer) =
  if a = (4, 0) then
    GGameOver <- true


let draw brd (c : code) sel =
  // CREATES STUFF TO BE DRAWN
  let header = "\
*****_____\n\                
*___|    _|_ ___   ______  __   ______ _____  ____    __ ____ ____   _ _____\n\
|    \\  /  |  _ \\ |   ____|  |_|   ___|     ||    \\  /  |    |    \\ | |     \\\n\
|     \\/   |     \\ `-.`-|_    _|   ___|     \\|     \\/   |    |     \\| |      \\\n\
|__/\\__/|__|__|\\__|______||__| |______|__|\\__|__/\\__/|__|____|__/\\____|______/\n\
****|_____|\n\
***********af Aiyu, Frederik & Rasmus\n\
\n"
  
  let mutable strCodeWithSel = "\n    "
  for i in 0 .. 3 do
    if i = sel then
      strCodeWithSel <- strCodeWithSel + "-> " + colPin(c.[i]) + " <-"
    else 
      strCodeWithSel <- strCodeWithSel + "  " + colPin(c.[i]) + "  "
  strCodeWithSel <- strCodeWithSel + "\n\n\
Possible colors: Red (r) | Green (g) | Yellow (y) | Purple (p) | White (w) |\n\
Black (b)\n\
Or use the IJKL-cluster as the arrow-keys  -  Press \"C\" to confirm selcection\n"

  
  //DRAWS WHAT HAS BEEN CREATED TO THE SCREEN
  System.Console.Clear()
  System.Console.Write header
  System.Console.Write (printBoard brd)
  System.Console.Write strCodeWithSel
  
let main () =
    System.Console.Title <- "Mastermind - Super Cool™ edition"
    let mutable brd : board = []
    draw brd [Red; Red; Red; Red] (GSelector % 4) 
    while not GGameOver do
      draw brd (selCode()) GSelector

    //printfn "%A" list
    // let p = askPlayerType ()
    // let realCode = makeCode (p)
    // let mutable board = []

    // while not(GGameOver) do
    //   let currentGuess = guess (p) board
    //   let answer = validate (realCode) (currentGuess)
    //   board <- addGuess board currentGuess answer
    //   isGameOver (answer)
    //   System.Console.Write (printBoard board)
main ()
