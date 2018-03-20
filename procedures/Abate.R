
Abate <- function(Cond, Amount, Scale, Income){
  Scale_Rows <- nrow(Scale)
  for (Row in 2:Scale_Rows){
    Amount <- Amount - Cond * pmax(0, pmin(Scale[Row, 1], Income) - Scale[Row - 1, 1]) * Scale[Row - 1, 2]
  }
  Amount <- Amount - Cond * pmax(0, Income - Scale[Scale_Rows, 1]) * Scale[Scale_Rows, 2]
  return(pmax(0, Amount))
}
attr(Abate, "utility") <- T
