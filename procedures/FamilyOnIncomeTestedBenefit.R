
FamilyOnIncomeTestedBenefit <- function(Data, Periods = 1){
    Data[, F_Benefits_OnIncomeTestedBenefitByPeriod := 
                    max(P_Benefits_CoreBenefits_CalculatedRecipientByPeriod), 
           by = F_ID]
    Data[, cntOfFamMembersOnSuper := sum(P_Super_Amount_Gross > 0),
           by = F_ID]
    Data[(F_Attributes_OnSuper == 1) & (cntOfFamMembersOnSuper == 2), 
         F_Benefits_OnIncomeTestedBenefitByPeriod := 1]
    Data[, cntOfFamMembersOnSuper := NULL]
}
attr(FamilyOnIncomeTestedBenefit, "output") <- 
    c("F_Benefits_OnIncomeTestedBenefitByPeriod"
     )
attr(FamilyOnIncomeTestedBenefit, "input")  <- 
    c("F_ID",
      "P_Benefits_CoreBenefits_CalculatedRecipientByPeriod",
      "F_Attributes_OnSuper",
      "P_Super_Amount_Gross"
     )
