
DisposableIncome <- function(Data, Periods = 1){
   Data[, P_Income_Disposable := P_Income_Total
                                 + P_FamilyAssistance_Total
                                 + P_TaxCredit_IETC
                                 - P_Income_TaxPayable
                                 - P_ACC_LevyPayable]
}
attr(DisposableIncome, "output") <- 
    c("P_Income_Disposable"
     )
attr(DisposableIncome, "input")  <- 
    c("P_ACC_LevyPayable",
      "P_Income_Total",
      "P_Income_TaxPayable",
      "P_FamilyAssistance_Total",
      "P_TaxCredit_IETC"
     )
