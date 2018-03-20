
Net_From_Gross <- function(Amount, Tax_Scale){
  return(Amount - Apply(Amount, Tax_Scale))
}
attr(Net_From_Gross, "utility") <- T
