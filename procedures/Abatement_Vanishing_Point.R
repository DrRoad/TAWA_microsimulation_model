
Abatement_Vanishing_Point <- function(Scale, Amount){
  n <- nrow(Scale)
  for (Th in 1:(n-1)){
    if (Scale[Th, 2] == 0) next
    if (Scale[Th, 1] + Amount/Scale[Th, 2] > Scale[Th + 1, 1]){
      Amount = Amount - Scale[Th, 2]*(Scale[Th + 1, 1] - Scale[Th, 1])
    } else {
      return(Scale[Th, 1] + Amount/Scale[Th, 2])
    }
  }
  return(Scale[n, 1] + Amount/Scale[n, 2])
}
attr(Abatement_Vanishing_Point, "utility") <- T
