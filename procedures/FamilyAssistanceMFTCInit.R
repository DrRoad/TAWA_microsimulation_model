
FamilyAssistanceMFTCInit <- function(Data,
                                     Periods = 1){
  weeks_in_period = 52.2/Periods
  Data[, ':=' 
       (F_FamilyAssistance_WorkingVariables_MFTC_WeeksEligible = 0, 
       P_FamilyAssistance_WorkingVariables_MFTC_TaxableSpecifiedIncome = 0,
       F_FamilyAssistance_WorkingVariables_MFTC_Eligible = 0)]
  Data[(F_FamilyAssistance_WorkingVariables_FullTimeWagedByPeriod == 1) & 
        (P_Attributes_Dependent == 0) & 
        (F_Benefits_OnIncomeTestedBenefitByPeriod == 0), 
       ':=' 
       (F_FamilyAssistance_WorkingVariables_MFTC_WeeksEligible = weeks_in_period, 
       P_FamilyAssistance_WorkingVariables_MFTC_TaxableSpecifiedIncome = P_Income_PrivateTaxCredit,
       F_FamilyAssistance_WorkingVariables_MFTC_Eligible = 1)]
  Data[,
       ':=' 
       (F_FamilyAssistance_WorkingVariables_MFTC_Eligible = 
         max(F_FamilyAssistance_WorkingVariables_MFTC_Eligible),
       F_FamilyAssistance_WorkingVariables_MFTC_WeeksEligible = 
         max(F_FamilyAssistance_WorkingVariables_MFTC_WeeksEligible))
       , 
       by = F_ID]
}
attr(FamilyAssistanceMFTCInit, "output") <- 
  c("F_FamilyAssistance_WorkingVariables_MFTC_WeeksEligible",
    "P_FamilyAssistance_WorkingVariables_MFTC_TaxableSpecifiedIncome",
    "F_FamilyAssistance_WorkingVariables_MFTC_Eligible")
attr(FamilyAssistanceMFTCInit, "input")  <- 
  c("F_ID",
    "F_Benefits_OnIncomeTestedBenefitByPeriod",
    "F_FamilyAssistance_WorkingVariables_FullTimeWagedByPeriod",
    "P_Income_PrivateTaxCredit",
    "P_Attributes_Dependent")
