
CalculateWFF <- function(Data, Periods = 1){
   Data[, P_FamilyAssistance_Total := P_FamilyAssistance_FTC_Abated +
                                      P_FamilyAssistance_IWTC_Abated +
                                      P_FamilyAssistance_MFTC_Amount +
                                      P_FamilyAssistance_PTC_Abated +
                                      P_FamilyAssistance_BestStart]
    Data[, P_FamilyAssistance_Total := sum(P_FamilyAssistance_Total)/F_Counts_Adults,
         by=F_ID]
    Data[P_Attributes_Dependent==1, P_FamilyAssistance_Total := 0]
}
attr(CalculateWFF, "output") <- 
    c("P_FamilyAssistance_Total"
     )
attr(CalculateWFF, "input")  <- 
    c("P_FamilyAssistance_FTC_Abated",
      "P_FamilyAssistance_IWTC_Abated",
      "P_FamilyAssistance_MFTC_Amount",
      "P_FamilyAssistance_PTC_Abated",
      "P_FamilyAssistance_BestStart",
      "F_Counts_Adults",
      "P_Attributes_Dependent",
      "F_ID"
     )
