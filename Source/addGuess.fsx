type codeColor =
  |Red | Green | Yellow | Purple | White | Black

type code = codeColor list

type answer = int * int

type board = ( code * answer ) list

type player = Human | Computer

// [([R, G, Y, P], (2,2)); ([R, G, Y, P], (2,2))]
let addGuess (brd: board) (c: code) (a: answer) =
  