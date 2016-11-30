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
let mutable GCode = [|0; 0; 0; 0|]
let GTries = 9
// Global variable GTestList is defined on line 60




/// <summary>
///    Converts the numbers 0-5 to its equivalent colour.
/// </summary>
/// <remarks>
///   The function cannot handle integer input larger than 5 or less than 0.
/// </remarks>
/// <param name="n">
///    An integer from 0 to 5, inclusive
/// </param name="n">
/// <returns>
///   Returns colour if the inputted integer is between 0-5
/// </returns>
(* NumToCol: Helper function to getRndCode *)
let numToCol n =
    match n with
    | 0 -> Red
    | 1 -> Green
    | 2 -> Yellow
    | 3 -> Purple
    | 4 -> White
    | _ -> Black


/// <summary>
///    The reverse function to numToCol
/// </summary>
let colToNum c =
  match c with
  | Red    -> 0
  | Green  -> 1
  | Yellow -> 2
  | Purple -> 3
  | White  -> 4
  | Black  -> 5

(*Yet another global variable*)
let mutable GTestList = [for i in 0..5 do
                         for j in 0..5 do
                          for k in 0..5 do
                           for l in 0..5 do
                            yield [numToCol i; numToCol j; numToCol k; numToCol l]]

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
    let list = [for _ in 1..4 -> (numToCol (rnd.Next() % 6))]
    list



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
///    Ask the user for a change to the input code or selector posistion.
/// </summary>
/// <remarks>
///   Dependes on the global varibles GCommit and GSelector. It keeps
///   taking and ignoring input until the user gives a valid one.
/// </remarks>
/// <returns>
///   Returns a valid code (i.e. with a length of 4).
/// </returns>
(* selCode: Helper function to makeCode *)
let selCode () : code =
    GSelector <- GSelector % 4
    let mutable bRun = true
    while bRun do
      let input = string <| (System.Console.ReadKey true).Key
      match input.ToLower() with
      | "c" -> GCommit <- true; bRun <- false
      | "j" -> GSelector <- (GSelector + 3) % 4; bRun <- false
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
///    Computes a smart guess.
/// </summary>
/// <remarks>
///   Dependes on the global varible GTestList. Uses an averege guess of ~6
///   guesses to guess the secret code.
/// </remarks>
/// <returns>
///   Returns a valid code (i.e. with a length of 4).
/// </returns>
let computeGuess () :code =
    GTestList.[0]


/// <summary>
///    Takes a player type and returns a code by the appropriate means.
/// </summary>
/// <remarks>
///   Uses selCode or computeGuess to get the code
/// </remarks>
/// <param name="p">
///    Player type of either Human or Computer
/// </param name="p">
/// <returns>
///   Returns a valid code (i.e. with a length of 4).
/// </returns>
(* Game: guess *)
let guess (p:player) (brd:board) : code =
  match p with
  | Human     -> selCode ()
  | Computer  -> computeGuess ()


/// <summary>
///    Takes 2 codes and generates an answer.
/// </summary>
/// <remarks>
///   The function is "symetric", so it does not matter in wich order the codes
///   are given.
/// </remarks>
/// <param name="rhsC">
///    A code, from either of the players playing the game.
/// </param name="rhsC">
/// <param name="lhsC">
///    The other code from either the players playing the game.
/// </param name="lhsC">
/// <returns>
///   Returns an anwser with no overlap in the whites and blacks.
/// </returns>
(* validate: Part of the guess loop  | hist for histogram*)
let validate (rhsC: code) (lhsC: code) :answer =
  let mutable i   = 0
  let mutable b   = 0
  let histLhsC       = [|0;0;0;0;0;0|]
  let histRhsC      = [|0;0;0;0;0;0|]
  let mutable sum = 0

  for i=0 to 3 do
    if rhsC.[i] = lhsC.[i] then
      b <- b + 1

    match rhsC.[i] with
    | Red    -> histRhsC.[0] <- histRhsC.[0]+1
    | Green  -> histRhsC.[1] <- histRhsC.[1]+1
    | Yellow -> histRhsC.[2] <- histRhsC.[2]+1
    | Purple -> histRhsC.[3] <- histRhsC.[3]+1
    | White  -> histRhsC.[4] <- histRhsC.[4]+1
    | Black  -> histRhsC.[5] <- histRhsC.[5]+1

    match lhsC.[i] with
    | Red    -> histLhsC.[0] <- histLhsC.[0]+1
    | Green  -> histLhsC.[1] <- histLhsC.[1]+1
    | Yellow -> histLhsC.[2] <- histLhsC.[2]+1
    | Purple -> histLhsC.[3] <- histLhsC.[3]+1
    | White  -> histLhsC.[4] <- histLhsC.[4]+1
    | Black  -> histLhsC.[5] <- histLhsC.[5]+1

  for i=0 to 5 do
    sum <- sum + (min (histLhsC.[i]) (histRhsC.[i]))

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
///   Takes a colour and returns the appropriate symbol with the same colour.
/// </summary>
/// <param name="c">
///    The colour used.
/// </param name="c">
/// <returns>
///   Returns a string.
/// </returns>
(* colPin: Helper function to sprintBoard *)
let colPin (c: codeColor) =
  match c with
  | Red    -> "\x1b[31;1m\u262d\x1b[37;0m"
  | Green  -> "\x1b[32;1m\u2623\x1b[37;0m"
  | Yellow -> "\x1b[33;1m\u2622\x1b[37;0m"
  | Purple -> "\x1b[35;1m\ud83c\udf03\x1b[37;0m"
  | White  -> "\x1b[37;1m\u2620\x1b[37;0m"
  | Black  -> "\x1b[30;1m\u26f0\x1b[37;0m"





/// <summary>
///    Takes a board and makes a string in a formated way, that represents the 
//     board.
/// </summary>
/// <param name="brd">
///    The board that gets printed.
/// </param name="brd">
/// <returns>
///   Returns a string.
/// </returns>
(* sprintBoard: Part of the guess loop *)
let sprintBoard (brd: board) =
  let mutable brdStr = ""
  let edge = "-------------------------"
  let str: Printf.StringFormat<_>  = "| %s | %s | %s | %s |\x1b[30;1m %i \x1b[37;0m-\x1b[37;1m %i \x1b[37;0m|"
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
///    A tuple of type answer * int, used to check if the game is over
/// </param name="a">
/// <returns>
///   Returns unit, but has the side effect of changing the global variable
//    GGameOver to false.
/// </returns>
(* isGameOver: Part of the guess loop *)
let isGameOver (a: answer * int) =
  if fst a = (4, 0) || snd a > GTries then
    GGameOver <- true

/// <summary>
///    Draws the board and selection bar to the console terminal.
/// </summary>
/// <remarks>
///   The function is called for its side effects.
/// </remarks>
/// <param name="brd">
///    The board which get drawn
/// </param name="brd">
/// <param name="c">
///    The code that gets drawn in the selection bar
/// </param name="c">
/// <param name="sel">
///    Selectors position in the selection bar
/// </param name="sel">
/// <returns>
///   Returns the code "c" unchanged.
/// </returns>
(* addGuess: Part of the guess loop *)
let draw brd (c : code) sel (* sel for selector *) =
  // CREATES STUFF TO BE DRAWN
  let header =
   "\n"+
   "     _____\n"+
   " ___|    _|_ ___   ______  __   ______ _____  ____    __ ____ ____   _ _____\n"+
   "|    \\  /  |  _ \\ |   ____|  |_|   ___|     ||    \\  /  |    |    \\ | |     \\\n"+
   "|     \\/   |     \\ `-.`-|_    _|   ___|     \\|     \\/   |    |     \\| |      \\\n"+
   "|__/\\__/|__|__|\\__|______||__| |______|__|\\__|__/\\__/|__|____|__/\\____|______/\n"+
   "    |_____|\n"+
   "             af Aiyu, Frederik & Rasmus\n"+
   "\n"

  let mutable strCodeWithSel = "\n    "
  for i in 0 .. 3 do
    if i = sel then
      strCodeWithSel <- strCodeWithSel + "->" + colPin(c.[i]) + " <-"
    else
      strCodeWithSel <- strCodeWithSel + "  " + colPin(c.[i]) + "   "
  strCodeWithSel <- strCodeWithSel + "\n\n\
Possible colors: Red (r) | Green (g) | Yellow (y) | Purple (p) | White (w) |\n\
Black (b)\n\
Or use the IJKL-cluster as arrow-keys  -  Press \"C\" to confirm selection...\n"


  //DRAWS WHAT HAS BEEN CREATED TO THE SCREEN
  System.Console.Clear()
  System.Console.Write header
  System.Console.Write (sprintBoard brd)
  System.Console.Write strCodeWithSel

  //RETURN THE CODE USED FOR FUTHER USE BY OTHER FUNCTIONS
  c

/// <summary>
///    Draws a game overscreen with some game statistics to the console terminal
/// </summary>
/// <remarks>
///   It gets the number of tries by taking the length of brd.
/// </remarks>
/// <param name="brd">
///    The board for which the game statistics get extraced from.
/// </param name="brd">
/// <returns>
///   Returns unit.
/// </returns>
let gameOverScreen brd =
  let lstLen = List.length brd // equals the number of tries used
  let header =
   "\n\
   '     .::::                                             .::::\n\
   ' .:    .::                                         .::    .::\n\
   '.::           .::    .::: .:: .::    .::         .::        .::.::     .::   .::    .: .:::\n\
   '.::         .::  .::  .::  .:  .:: .:   .::      .::        .:: .::   .::  .:   .::  .::\n\
   '.::   .::::.::   .::  .::  .:  .::.::::: .::     .::        .::  .:: .::  .::::: .:: .::\n\
   ' .::    .: .::   .::  .::  .:  .::.:               .::     .::    .:.::   .:         .::\n\
   '  .:::::     .:: .:::.:::  .:  .::  .::::            .::::         .::      .::::   .:::\n"
  let winStatusStr = if lstLen > GTries then "lose" else "win"


  System.Console.Clear()
  System.Console.Write header
  System.Console.Write ("\nYou " + winStatusStr + " the game!\n")
  System.Console.Write ("You used " + string lstLen + " tries!\n")

  ()

/// <summary>
///   Asks a human player for the secret code in a graphical way.
/// </summary>
/// <remarks>
///   Calls selCode with wrapper code around to achive goal.
/// </remarks>
/// <returns>
///   Returns the inputed code.
/// </returns>
let selSecretCode () : code =
  let mutable c : code = []
  draw [] [Red; Red; Red; Red] GSelector |> ignore
  while not GCommit do
    c <- draw [] (selCode ()) GSelector
    System.Console.Write "You are currently selecting the secret code...\n"
  
  //REMOVES THE SELECTED SECRET CODE FROM THE SCREEN
  System.Console.Clear()

  //RESETS GCode and GCommit
  GCode <- [|0; 0; 0; 0|]
  GCommit <- false

  c


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
  | Human    -> selSecretCode ()
  | Computer -> getRndCode ()


let main () =
    // INIT
    System.Console.Title <- "Mastermind - Super Cool™ edition"
    let mutable brd : board = []

    // GETS PLAYERS
    let p1 = askPlayerType ()
    let p2 = askPlayerType ()

    // GETS THE SECRET CODE
    let cSecret = makeCode p1

    //GAME LOGIC (GAME LOOP)
    draw brd [Red; Red; Red; Red] GSelector |> ignore
    while not GGameOver do
       let c = draw brd (guess p2 brd) GSelector
       if GCommit || p2 = Computer then
         GCommit <- false
         let answr = validate c cSecret
         brd <- addGuess brd c answr

         GTestList <- List.filter(fun x -> (validate c x) = answr) 
                      GTestList.[1..GTestList.Length-1] //All but the first elem

         // SHOWS THE UPDATED BOARD /W COMMITED GUESS TO THE USER
         draw brd c GSelector |> ignore

         isGameOver (answr, (List.length brd))

    // GAME MUST BE OVER BY THIS POINT IN THE CODE, THERFORE:
    gameOverScreen brd

    ()
main ()
