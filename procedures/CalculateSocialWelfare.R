
CalculateSocialWelfare <- function(Data, Periods = 1){
  Data[, P_Benefits_All_SuppAssist := 
         P_Benefits_Special_Amount_RawSurveyGross + 
         P_Benefits_Disability_Amount_RawSurvey + 
         P_Benefits_Accommodation_Abated +
         P_Benefits_WinterEnergy]
  Data[, P_Benefits_All_OtherBenefits := 
         P_Benefits_Orphans_Amount_RawSurveyGross + 
         P_Benefits_Misc_Amount_RawSurvey + 
         P_Pensions_WarDisability_Amount_RawSurvey + 
         P_Pensions_OtherNewZealandPensions_Amount_RawSurvey]
  Data[, P_Benefits_All_Taxable := 
         P_Benefits_CoreBenefits_GrossAmount + 
         P_Super_Amount_Gross + 
         P_Income_Raw_StudentAllowance + 
         P_Benefits_YP_ImputedAmount + 
         P_Benefits_YPP_ImputedAmount]
  Data[, P_Benefits_All_NonTaxable := 
         P_Benefits_All_SuppAssist + 
         P_Benefits_All_OtherBenefits]
  Data[, P_Benefits_All_SWTotal := 
         P_Benefits_All_Taxable + 
         P_Benefits_All_NonTaxable]
}
attr(CalculateSocialWelfare, "output") <- 
  c("P_Benefits_All_SuppAssist",
    "P_Benefits_All_OtherBenefits",
    "P_Benefits_All_Taxable",
    "P_Benefits_All_NonTaxable",
    "P_Benefits_All_SWTotal")
attr(CalculateSocialWelfare, "input") <- 
  c("P_Benefits_CoreBenefits_GrossAmount",
    "P_Super_Amount_Gross",
    "P_Benefits_Accommodation_Abated",
    "P_Benefits_WinterEnergy",
    "P_Pensions_OtherNewZealandPensions_Amount_RawSurvey",
    "P_Benefits_Special_Amount_RawSurveyGross",
    "P_Benefits_Disability_Amount_RawSurvey",
    "P_Benefits_Orphans_Amount_RawSurveyGross",
    "P_Benefits_Misc_Amount_RawSurvey",
    "P_Pensions_WarDisability_Amount_RawSurvey",
    "P_Income_Raw_StudentAllowance",
    "P_Benefits_YP_ImputedAmount",
    "P_Benefits_YPP_ImputedAmount")
