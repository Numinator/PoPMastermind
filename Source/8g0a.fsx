(* The types *)
type codeColor = Red | Green | Yellow | Purple | White | Black

type code = codeColor list

type answer = int * int

type board = ( code * answer ) list

type player = Human | Computer
let mutable gameOver = false


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
    let list = [numToCol (rnd.Next() % 6);numToCol (rnd.Next() % 6);numToCol (rnd.Next() % 6);numToCol (rnd.Next() % 6);]
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
///    Asks the user to input a code and returns it
/// </summary>
/// <remarks>
///   The function does not stop before the user have inputted a valid code. It
///   takes unit as input.
/// </remarks>
/// <returns>
///   Returns a valid code.
/// </returns>
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
  | Red    -> "R"
  | Green  -> "G"
  | Yellow -> "Y"
  | Purple -> "P"
  | White  -> "W"
  | Black  -> "B"



/// <summary>
///    Takes a board and prints it to the screen in a formated way.
/// </summary>
/// <param name="brd">
///    The board that gets printed.
/// </param name="brd">
/// <returns>
///   Returns unit.
/// </returns>
(* printBoard: Part of the guess loop *)
let printBoard (brd: board) =
  let edge:Printf.TextWriterFormat<_> = "-------------------------"
  let str:Printf.TextWriterFormat<_>  = "| %s | %s | %s | %s | %i - %i |"
  printfn edge
  for i in brd do
    let f j =  colPin (fst i).[j]
    let s = snd i
    printfn str (f 0) (f 1) (f 2) (f 3) (fst s) (snd s)
    printfn edge
  ()


/// <summary>
///    Finds out if the game is over and changes gameOver to true if it is.
/// </summary>
/// <remarks>
///   gameOver is a global varible that must be present.
/// </remarks>
/// <param name="a">
///    Answer that is used to check if the game is over.
/// </param name="a">
/// <returns>
///   Returns unit.
/// </returns>
(* isGameOver: Part of the guess loop *)
let isGameOver (a: answer) =
  if a = (4, _) then
    gameOver <- true

[<Entry Point>]
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
