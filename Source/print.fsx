// Includes
open System.IO

//Types
type codeColor = 
| Red | Green | Yellow | Purple | White | Black

type code          = codeColor list

type answer        = int * int

type board         = ( code * answer ) list

type player        = Human | Computer

//code
let colPin (c: codeColor) = 
  match c with
  | Red    -> "R"
  | Green  -> "G"
  | Yellow -> "Y"
  | Purple -> "P"
  | White  -> "W"
  | Black  -> "B"





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
     
