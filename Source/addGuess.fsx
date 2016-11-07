type codeColor =
  |Red | Green | Yellow | Purple | White | Black

type code = codeColor list

type answer = int * int

type board = ( code * answer ) list

type player = Human | Computer

let addGuess (brd: board) (c: code) (a: answer) =
