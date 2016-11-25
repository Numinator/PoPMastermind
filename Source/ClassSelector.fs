type Selector() =
  member private this.Number = 0
  member private this.Max = 0
  member public this.SetSize n = this.Max <- n
