
FamilyAssistanceIWTC <- function(Data, 
                                 FamilyAssistance_IWTC_Rates_UpTo3Children, 
                                 FamilyAssistance_IWTC_Rates_SubsequentChildren, 
                                 Periods = 1){
  Data[, ':='(
    Temporary_variable_Student = sum(P_Income_Raw_StudentAllowance) > 0,
    Temporary_variable_Number_Dependents = sum(P_Attributes_Dependent)),
       by = F_ID]
  Data[,
       P_FamilyAssistance_IWTC_Unabated := 0] 
  Data[(P_Attributes_Carer == 1) & 
         (F_FamilyAssistance_WorkingVariables_FullTimeByPeriod == 1) & 
         (F_Benefits_OnIncomeTestedBenefitByPeriod == 0) & 
         (Temporary_variable_Student == FALSE),
       P_FamilyAssistance_IWTC_Unabated := 
         FamilyAssistance_IWTC_Rates_UpTo3Children + 
         FamilyAssistance_IWTC_Rates_SubsequentChildren*pmax(0, Temporary_variable_Number_Dependents-3)]
  Data[, Temporary_variable_Student := NULL]
  Data[, Temporary_variable_Number_Dependents := NULL]
}
attr(FamilyAssistanceIWTC, "output") <- 
  c("P_FamilyAssistance_IWTC_Unabated")
attr(FamilyAssistanceIWTC, "input")  <- 
  c("F_ID", 
    "P_Attributes_Carer",
    "F_Benefits_OnIncomeTestedBenefitByPeriod",
    "F_FamilyAssistance_WorkingVariables_FullTimeByPeriod",
    "P_Attributes_Dependent",
    "P_Income_Raw_StudentAllowance")
