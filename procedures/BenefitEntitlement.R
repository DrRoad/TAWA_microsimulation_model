
BenefitEntitlement <- function(Data, 
                               Periods = 1){
  Data[,
       ':=' (P_Benefits_Eligibility_JSS_RecipientByPeriod = P_Benefits_Processed_JSS_RecipientByPeriod,
             P_Benefits_Eligibility_SLP_RecipientByPeriod = P_Benefits_Processed_SLP_RecipientByPeriod,
             P_Benefits_Eligibility_SPS_RecipientByPeriod = P_Benefits_Processed_SPS_RecipientByPeriod)]
}
attr(BenefitEntitlement, "output") <- 
  c("P_Benefits_Eligibility_JSS_RecipientByPeriod",
    "P_Benefits_Eligibility_SLP_RecipientByPeriod",
    "P_Benefits_Eligibility_SPS_RecipientByPeriod")
attr(BenefitEntitlement, "input")  <- 
  c("P_Benefits_Processed_JSS_RecipientByPeriod",
    "P_Benefits_Processed_SLP_RecipientByPeriod",
    "P_Benefits_Processed_SPS_RecipientByPeriod")
