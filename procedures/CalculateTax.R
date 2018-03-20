
CalculateTax <- function(Data, 
                         Tax_BaseScale, 
                         Periods = 1){
  Data[, ':='(Temporary_variable_gross_inc = P_Income_Taxable)]
  Data[, P_Income_GrossTax := Apply(P_Income_Taxable, Tax_BaseScale)]
  Data[, ':='(P_Income_TaxPayable = pmax(0, P_Income_GrossTax),
               P_Income_Rebates  = 0)]
  Data[, ':='(Temporary_variable_gross_inc = NULL)]
}
attr(CalculateTax, "output") <- c("P_Income_GrossTax",
                                  "P_Income_TaxPayable",
                                  "P_Income_Rebates")
attr(CalculateTax, "input")  <- c("P_Income_Taxable")
