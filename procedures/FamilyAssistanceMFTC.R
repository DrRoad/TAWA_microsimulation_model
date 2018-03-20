
FamilyAssistanceMFTC <- function(Data, 
                                 FamilyAssistance_MFTC_Rates_MinimumIncome, 
                                 Tax_BaseScale, 
                                 Periods = 1){
  Data[F_FamilyAssistance_WorkingVariables_MFTC_WeeksEligible  == 52.2, 
       Temporary_variable_tax := 
         Apply(P_FamilyAssistance_WorkingVariables_MFTC_TaxableSpecifiedIncome, 
                     Tax_BaseScale)]
  Data[F_FamilyAssistance_WorkingVariables_MFTC_WeeksEligible  == 52.2, 
       Temporary_variable_net_specified_income := 
         sum(P_FamilyAssistance_WorkingVariables_MFTC_TaxableSpecifiedIncome - Temporary_variable_tax),
       by = F_ID]
  Data[(P_Attributes_Carer == 0) | (F_FamilyAssistance_WorkingVariables_MFTC_WeeksEligible==0), 
       P_FamilyAssistance_MFTC_Amount := 0]
  Data[(P_Attributes_Carer == 1) & (F_FamilyAssistance_WorkingVariables_MFTC_WeeksEligible>0),
        P_FamilyAssistance_MFTC_Amount := pmax(0, FamilyAssistance_MFTC_Rates_MinimumIncome - 
                                                    Temporary_variable_net_specified_income)]
  Data[, Temporary_variable_net_specified_income := NULL]
  Data[, Temporary_variable_tax := NULL]
}
attr(FamilyAssistanceMFTC, "output") <-
  c("P_FamilyAssistance_MFTC_Amount")
attr(FamilyAssistanceMFTC, "input")  <- 
  c("F_ID",
    "P_Attributes_Carer",
    "F_FamilyAssistance_WorkingVariables_MFTC_WeeksEligible",
    "P_FamilyAssistance_WorkingVariables_MFTC_TaxableSpecifiedIncome")
