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
  let edge         = "-----------------"
  let mutable str  = "|*|*|*|*| n - n |"
  Console.WriteLine edge
  for i in brd do
    
