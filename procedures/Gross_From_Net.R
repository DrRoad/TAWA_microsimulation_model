Gross_From_Net <- function(Amount, Net_Thresholds, Tax_Scale){
  Levels <- length(Net_Thresholds)
  Old <- 0
  Gross <- 0*Amount
  for (Th in 2:Levels){
    Gross <- Gross + (Amount < Net_Thresholds[Th] & Amount >= Net_Thresholds[Th - 1])*
      (Tax_Scale[Th-1, 1] + (Amount - Old) / (1 - Tax_Scale[Th - 1, 2]))
    Old <- Net_Thresholds[Th]
  }
  return(Gross + (Amount > Net_Thresholds[Levels]) * 
           (Tax_Scale[Levels, 1] + (Amount - Old) / (1 - Tax_Scale[Levels, 2])))
}
attr(Gross_From_Net, "utility") <- T
