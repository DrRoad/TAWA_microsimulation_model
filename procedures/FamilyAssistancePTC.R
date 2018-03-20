
FamilyAssistancePTC <- function(Data, FamilyAssistance_PTC_RatePerChild, FamilyAssistance_PTC_Weeks, FamilyAssistance_PTC_IncludeBeneficiaries, Periods = 1){
    periodsOfPTC = as.integer(ceiling(FamilyAssistance_PTC_Weeks/52.2*24-1e-8))
    AllJobs = seq(1,9)
    Data[, temp_famOnNZSuper := sum(P_Super_Amount_Gross)>0, by=F_ID]
    Data[, temp_famOnStudentAllowance := sum(P_Income_Raw_StudentAllowance)>0, by=F_ID]   
    Data[, temp_famOnACCWeeks := 0]
    for (job in AllJobs){
       Data[, temp_ACCWeeks := 1*.SD,
           .SDcols = paste0('P_Jobs_Calculated_', job, '_ACCWeeks')]
       Data[, temp_famOnACCWeeks := pmax(temp_famOnACCWeeks, temp_ACCWeeks)]  
    }
    Data[, temp_famOnACCWeeks := max(temp_famOnACCWeeks), by=F_ID]
    Data[, temp_famOnPPL := sum(P_FamilyAssistance_ParentalLeave_Amount_Calculated) > 0,
                             by=F_ID]
    if (FamilyAssistance_PTC_IncludeBeneficiaries==T) { 
        Data[, temp_famIsEligible:= ((temp_famOnNZSuper==F) &
                                     (temp_famOnStudentAllowance==F) &
                                     (temp_famOnACCWeeks<13) &
                                     (temp_famOnPPL==F))]    
    } 
    else {  
        Data[, temp_famIsEligible:= ((F_Benefits_OnIncomeTestedBenefitByPeriod==F) &
                                     (temp_famOnNZSuper==F) &
                                     (temp_famOnStudentAllowance==F) &
                                     (temp_famOnACCWeeks<13) &
                                     (temp_famOnPPL==F))]    
    }
    Data[, temp_child_payment := 0]
    Data[(temp_famIsEligible==T) &
         (P_Attributes_Dependent==T) &
         (P_Attributes_Age==0) & 
         (P_Attributes_BirthPeriod<(25 - periodsOfPTC/2)),
         temp_child_payment := FamilyAssistance_PTC_RatePerChild]
    Data[(temp_famIsEligible==T) &
         (P_Attributes_Dependent==T) &
         (P_Attributes_Age==0) & 
         (P_Attributes_BirthPeriod>=(25 - periodsOfPTC/2)),
         temp_child_payment := FamilyAssistance_PTC_RatePerChild/2]
    Data[(temp_famIsEligible==T) &
         (P_Attributes_Dependent==T) &
         (P_Attributes_Age==1) & 
         (P_Attributes_BirthPeriod>=(25 - periodsOfPTC/2)),
         temp_child_payment := FamilyAssistance_PTC_RatePerChild/2]
    Data[, P_FamilyAssistance_PTC_Unabated := sum(temp_child_payment), by=F_ID]
    Data[P_Attributes_Carer==0, P_FamilyAssistance_PTC_Unabated := 0]
    Data[, `:=` (temp_famOnNZSuper = NULL,
                 temp_famOnStudentAllowance = NULL,
                 temp_famOnACCWeeks = NULL,
                 temp_ACCWeeks = NULL,
                 temp_famOnPPL = NULL,
                 temp_famIsEligible = NULL,
                 temp_child_payment = NULL)]
}
attr(FamilyAssistancePTC, "output") <- 
    c("P_FamilyAssistance_PTC_Unabated"
     )
attr(FamilyAssistancePTC, "input")  <- 
    c(
    "P_Super_Amount_Gross",
    "P_FamilyAssistance_ParentalLeave_Amount_Calculated",
    "P_Jobs_Calculated_1_ACCWeeks",
    "P_Jobs_Calculated_2_ACCWeeks",
    "P_Jobs_Calculated_3_ACCWeeks",
    "P_Jobs_Calculated_4_ACCWeeks",
    "P_Jobs_Calculated_5_ACCWeeks",
    "P_Jobs_Calculated_6_ACCWeeks",
    "P_Jobs_Calculated_7_ACCWeeks",
    "P_Jobs_Calculated_8_ACCWeeks",
    "P_Jobs_Calculated_9_ACCWeeks",
    "P_Attributes_Age",
    "P_Income_Raw_StudentAllowance",
    "P_Attributes_BirthPeriod",
    "F_ID",
    "P_Attributes_Carer",
    "P_Attributes_Dependent",
    "F_Benefits_OnIncomeTestedBenefitByPeriod")
