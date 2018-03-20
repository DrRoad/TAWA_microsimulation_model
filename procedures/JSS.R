
JSS <- function(
    Data, 
    Benefits_JSS_Rate_Couple, 
    Benefits_JSS_Rate_SoleParent, 
    Benefits_JSS_Rate_WomanAlone, 
    Benefits_JSS_Rate_Single, 
    Benefits_JSS_Rate_SingleYoung, 
    Benefits_JSS_Rate_SingleAtHome, 
    Benefits_JSS_Rate_CoupleParent,
    modelyear = 2015,
    Periods = 1){
    weeks_in_year = 52.2
    Data[, P_Benefits_JSS_Amount_Unabated := 0]
    Data[P_Benefits_Eligibility_JSS_RecipientByPeriod == 1,
         P_Benefits_JSS_Amount_Unabated := weeks_in_year * 
                                            Benefits_JSS_Rate_SingleYoung]
    Data[(P_Benefits_Eligibility_JSS_RecipientByPeriod == 1) &
         (P_Relationships_ToHHhead == 3),
         P_Benefits_JSS_Amount_Unabated := weeks_in_year * 
                                            Benefits_JSS_Rate_SingleAtHome]                     
    Data[(P_Benefits_Eligibility_JSS_RecipientByPeriod == 1) &
         (P_Attributes_Age > 19),
         P_Benefits_JSS_Amount_Unabated := weeks_in_year * 
                                            Benefits_JSS_Rate_SingleYoung]
    Data[(P_Benefits_Eligibility_JSS_RecipientByPeriod == 1) &
         (P_Attributes_Age > 24),
         P_Benefits_JSS_Amount_Unabated := weeks_in_year * 
                                            Benefits_JSS_Rate_Single]
    Data[(P_Benefits_Eligibility_JSS_RecipientByPeriod == 1) &
         (F_Counts_Dependents > 0),
         P_Benefits_JSS_Amount_Unabated := weeks_in_year * 
                                            Benefits_JSS_Rate_SoleParent]
    Data[(P_Benefits_Eligibility_JSS_RecipientByPeriod == 1) &
         (F_Counts_Dependents > 0) &
         (F_Children_AgeOfYoungest < 14),
         P_Benefits_JSS_Amount_Unabated := weeks_in_year * 
                                            Benefits_JSS_Rate_SoleParent]                                      
    Data[(P_Benefits_Eligibility_JSS_RecipientByPeriod == 1) &
         (F_Attributes_IsCouple == 1),
         P_Benefits_JSS_Amount_Unabated := weeks_in_year * 
                                            Benefits_JSS_Rate_Couple]
    Data[(P_Benefits_Eligibility_JSS_RecipientByPeriod == 1) &
         (F_Attributes_IsCouple == 1) &
         (F_Counts_Dependents > 0) &
         (modelyear >= 2017),
         P_Benefits_JSS_Amount_Unabated := weeks_in_year * 
                                            Benefits_JSS_Rate_CoupleParent]
    Data[(P_Benefits_Eligibility_JSS_RecipientByPeriod == 1) &
         (P_Attributes_Age < 16),
         P_Benefits_JSS_Amount_Unabated := 0]
    Data[(P_Benefits_Eligibility_JSS_RecipientByPeriod == 1) &
         (P_Attributes_Age <= 17) &
         (F_Attributes_IsCouple == 0),
         P_Benefits_JSS_Amount_Unabated := 0]
}
attr(JSS, "output") <- 
    c("P_Benefits_JSS_Amount_Unabated"
     )
attr(JSS, "input")  <- 
    c("P_Benefits_Eligibility_JSS_RecipientByPeriod",
      "F_Attributes_IsCouple",
      "P_Attributes_Age",
      "F_Counts_Dependents",
      "F_Children_AgeOfYoungest",
      "P_Relationships_ToHHhead"
     )
