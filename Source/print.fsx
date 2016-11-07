// Includes
open System.IO

//Types
type codeColor =
Red | Green | Yellow | Purple | White | Black

type code = codeColor list

type answer = int * int

type board = ( code * answer ) list

type player = Human | Computer

//code
let colPin (c : codeColor) =
  match c with
  | Red -> "\e[31m*\e[31m"
  | Green -> "\e[32m*\e[32m"

let printBoard brd =
  let edge = "-----------------"
  let mutable str = "|*|*|*|*| n - n |"
  Console.WriteLine edge
  for i in brd do
    
