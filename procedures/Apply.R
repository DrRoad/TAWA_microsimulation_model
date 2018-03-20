
Apply <- function(Amount, Tax_Scale){
  Tax <- 0
  n <- nrow(Tax_Scale)
  for (Th in 1:(n-1)){
    Tax <- Tax + (pmin(pmax(Amount, Tax_Scale[Th, 1]), Tax_Scale[Th + 1, 1]) - Tax_Scale[Th, 1])*Tax_Scale[Th, 2]
  }
  return(Tax + pmax(0, Amount - Tax_Scale[n, 1])*Tax_Scale[n, 2])
}
attr(Apply, "utility") <- T
