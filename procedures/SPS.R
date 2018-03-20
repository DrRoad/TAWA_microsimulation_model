
SPS <- function(Data, Benefits_SPS_Rate, Periods = 1){
   number_weeks_in_period = 52.2/Periods
   Data[P_Benefits_Eligibility_SPS_RecipientByPeriod == 1, 
        P_Benefits_SPS_Amount_Unabated := Benefits_SPS_Rate*number_weeks_in_period]
   Data[P_Benefits_Eligibility_SPS_RecipientByPeriod == 0, 
        P_Benefits_SPS_Amount_Unabated := 0]
}
attr(SPS, "output") <- c("P_Benefits_SPS_Amount_Unabated")
attr(SPS, "input")  <- c("P_Benefits_Eligibility_SPS_RecipientByPeriod")
