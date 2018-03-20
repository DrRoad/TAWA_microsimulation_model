
CalculateWfFIncome <- function(Data, Periods = 1){
   Data[, P_Income_WfFIncome := P_Income_PrivateTaxCredit + P_Benefits_All_Taxable]
   Data[, P_Income_WfFIncome := pmax(P_Income_WfFIncome, 0)]
}
attr(CalculateWfFIncome, "output") <- 
    c("P_Income_WfFIncome"
     )
attr(CalculateWfFIncome, "input")  <- 
    c("P_Income_PrivateTaxCredit",
      "P_Benefits_All_Taxable"
     )
