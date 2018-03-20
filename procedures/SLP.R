
SLP <- function(
    Data, 
    Benefits_SLP_Rate_Couple, 
    Benefits_SLP_Rate_SoleParent, 
    Benefits_SLP_Rate_SingleYoung, 
    Benefits_SLP_Rate_Single, 
    Benefits_SLP_Rate_CoupleParent,
    modelyear = 2015,
    Periods = 1){
    weeks_in_year = 52.2  
    if (nrow(Data[(P_Benefits_Eligibility_SLP_RecipientByPeriod == 1) & 
                  (P_Attributes_Age <= 15),]) != 0) {
        stop("errSLPAge")
    }  
    Data[, P_Benefits_SLP_Amount_Unabated := 0]
    Data[(P_Benefits_Eligibility_SLP_RecipientByPeriod == 1),
         P_Benefits_SLP_Amount_Unabated := weeks_in_year *  
                                            Benefits_SLP_Rate_Single]
    Data[(P_Benefits_Eligibility_SLP_RecipientByPeriod == 1) &
         (P_Attributes_Age > 16) &
         (P_Attributes_Age < 18),
         P_Benefits_SLP_Amount_Unabated := weeks_in_year *  
                                            Benefits_SLP_Rate_SingleYoung]
    Data[(P_Benefits_Eligibility_SLP_RecipientByPeriod == 1) &
         (F_Counts_Dependents > 0),
         P_Benefits_SLP_Amount_Unabated := weeks_in_year *  
                                            Benefits_SLP_Rate_SoleParent]
    Data[(P_Benefits_Eligibility_SLP_RecipientByPeriod == 1) &
         (F_Attributes_IsCouple == 1),
         P_Benefits_SLP_Amount_Unabated := weeks_in_year *  
                                            Benefits_SLP_Rate_Couple]
    Data[(P_Benefits_Eligibility_SLP_RecipientByPeriod == 1) &
         (F_Attributes_IsCouple == 1) &
         (F_Counts_Dependents > 0) &
         (modelyear >= 2017),
         P_Benefits_SLP_Amount_Unabated := weeks_in_year *  
                                            Benefits_SLP_Rate_CoupleParent]
}
attr(SLP, "output") <- 
    c("P_Benefits_SLP_Amount_Unabated")
attr(SLP, "input")  <- 
    c("P_Benefits_Eligibility_SLP_RecipientByPeriod",
      "F_Attributes_IsCouple",
      "P_Attributes_Age",
      "F_Counts_Dependents"
      )
