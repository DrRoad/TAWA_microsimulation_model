
CalculateTaxableIncome <- function(Data, Periods = 1){
  Data[, ':=' (P_Income_Taxable = pmax(0, P_Income_PrivateTaxable + P_Benefits_All_Taxable),
               P_Income_NonTaxable = P_Income_PrivateNonTaxable + P_Benefits_All_NonTaxable)]
  Data[, P_Income_Total :=  P_Income_PrivateTaxable + P_Benefits_All_Taxable + P_Income_NonTaxable ]
}
attr(CalculateTaxableIncome, "output") <- c("P_Income_Taxable",
                                            "P_Income_NonTaxable",
                                            "P_Income_Total")
attr(CalculateTaxableIncome, "input")  <- c("P_Income_PrivateTaxable",
                                            "P_Benefits_All_Taxable",
                                            "P_Income_PrivateNonTaxable",
                                            "P_Benefits_All_NonTaxable")
