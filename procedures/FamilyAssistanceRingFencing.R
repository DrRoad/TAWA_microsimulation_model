
FamilyAssistanceRingFencing <- function(Data, Periods = 1){
    Data[, F_FamilyAssistance_WorkingVariables_RingFencedByPeriod :=
           F_Benefits_OnIncomeTestedBenefitByPeriod]
}
attr(FamilyAssistanceRingFencing, "output") <- 
    c("F_FamilyAssistance_WorkingVariables_RingFencedByPeriod"
     )
attr(FamilyAssistanceRingFencing, "input")  <- 
    c("F_Benefits_OnIncomeTestedBenefitByPeriod"
     )
