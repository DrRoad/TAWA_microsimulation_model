
CalculateCoreBenefits <- function(Data, Benefits_SLP_AbatementScale, Tax_BaseScale, Benefits_JSS_AbatementScale, 
                                  Benefits_SPS_AbatementScale, Benefits_JSS_CoupleAbatementScale, 
                                  Benefits_SLP_CoupleAbatementScale, Periods = 1, Fast_Join = 1){  
  if (!Fast_Join) Data[, Family_Income := sum((P_Attributes_PrincipalEarner | P_Attributes_SpouseOfPrincipal)*P_Income_PrivateChargeable), by = .(F_ID, Period)]
  else {
    Data[, Family_Income := (P_Attributes_PrincipalEarner | P_Attributes_SpouseOfPrincipal)*P_Income_PrivateChargeable]
    Temp <- Data[, .(Family_Income = sum(Family_Income)), keyby = .(F_ID, Period)] 
    Data[, Family_Income := Temp[["Family_Income"]][Periods*(F_ID - 1) + Period]]
  }
  NIT <- c(0)
  for (Row in 2:nrow(Tax_BaseScale)){
    NIT <- c(NIT, tail(NIT, 1) + (Tax_BaseScale[Row, 1] - Tax_BaseScale[Row - 1, 1]) * (1 - Tax_BaseScale[Row - 1, 2]))
  }
  Data[, P_Benefits_SLP_Amount_Abated := 
         Abate(P_Benefits_SLP_Amount_Unabated > 0 & !F_Attributes_IsCouple,
               Periods*P_Benefits_SLP_Amount_Unabated,
               Benefits_SLP_AbatementScale, Periods*Family_Income)/Periods]
  Data[, P_Benefits_SLP_Amount_Abated := 
         Abate(P_Benefits_SLP_Amount_Unabated > 0 & F_Attributes_IsCouple,
               Periods*P_Benefits_SLP_Amount_Abated,
               Benefits_SLP_CoupleAbatementScale, Periods*Family_Income)/Periods]
  Data[, P_Benefits_SLP_Amount_Gross := Gross_From_Net(Periods*P_Benefits_SLP_Amount_Abated, NIT, Tax_BaseScale)/Periods]
  Data[, P_Benefits_SLP_Amount_Tax := P_Benefits_SLP_Amount_Gross - P_Benefits_SLP_Amount_Abated]
  Data[, P_Benefits_SLP_CalculatedRecipientByPeriod := 1*(P_Benefits_SLP_Amount_Abated > 0)]
  Data[, P_Benefits_SPS_Amount_Abated := 
         Abate(P_Benefits_SPS_Amount_Unabated > 0,
               Periods*P_Benefits_SPS_Amount_Unabated,
               Benefits_SPS_AbatementScale, Periods*Family_Income)/Periods]
  Data[, P_Benefits_SPS_Amount_Gross := Gross_From_Net(Periods*P_Benefits_SPS_Amount_Abated, NIT, Tax_BaseScale)/Periods]
  Data[, P_Benefits_SPS_Amount_Tax := P_Benefits_SPS_Amount_Gross - P_Benefits_SPS_Amount_Abated]
  Data[, P_Benefits_SPS_CalculatedRecipientByPeriod := 1*(P_Benefits_SPS_Amount_Abated > 0)]
  Data[, P_Benefits_JSS_Amount_Abated := 
         Abate(P_Benefits_JSS_Amount_Unabated > 0 & F_Attributes_IsCouple,
               Periods*P_Benefits_JSS_Amount_Unabated,
               Benefits_JSS_CoupleAbatementScale, Periods*Family_Income)/Periods]
  Data[, P_Benefits_JSS_Amount_Abated := 
         Abate(P_Benefits_JSS_Amount_Unabated > 0 & !F_Attributes_IsCouple & F_Counts_Dependents > 0,
               Periods*P_Benefits_JSS_Amount_Abated,
               Benefits_SPS_AbatementScale, Periods*Family_Income)/Periods]
  Data[, P_Benefits_JSS_Amount_Abated := 
         Abate(P_Benefits_JSS_Amount_Unabated > 0 & !F_Attributes_IsCouple & F_Counts_Dependents == 0,
               Periods*P_Benefits_JSS_Amount_Abated,
               Benefits_JSS_AbatementScale, Periods*Family_Income)/Periods]
  Data[P_Super_OnSuper != 0, P_Benefits_JSS_Amount_Abated := 0]
  Data[, P_Benefits_JSS_Amount_Gross := Gross_From_Net(Periods*P_Benefits_JSS_Amount_Abated, NIT, Tax_BaseScale)/Periods]
  Data[, P_Benefits_JSS_Amount_Tax := P_Benefits_JSS_Amount_Gross - P_Benefits_JSS_Amount_Abated]
  Data[, P_Benefits_JSS_CalculatedRecipientByPeriod := 1*(Data$P_Benefits_JSS_Amount_Abated > 0)]
  Data[, P_Benefits_CoreBenefits_UnabatedAmount := P_Benefits_JSS_Amount_Unabated + P_Benefits_SLP_Amount_Unabated + P_Benefits_SPS_Amount_Unabated]
  Data[, P_Benefits_CoreBenefits_AbatedAmount := P_Benefits_JSS_Amount_Abated + P_Benefits_SLP_Amount_Abated + P_Benefits_SPS_Amount_Abated]
  Data[, P_Benefits_CoreBenefits_GrossAmount := P_Benefits_JSS_Amount_Gross + P_Benefits_SLP_Amount_Gross + P_Benefits_SPS_Amount_Gross]
  Data[, P_Benefits_CoreBenefits_TaxAmount := P_Benefits_JSS_Amount_Tax + P_Benefits_SLP_Amount_Tax + P_Benefits_SPS_Amount_Tax]
  Data[, P_Benefits_CoreBenefits_CalculatedRecipientByPeriod := 
         1*(P_Benefits_SLP_CalculatedRecipientByPeriod |  P_Benefits_SPS_CalculatedRecipientByPeriod | P_Benefits_JSS_CalculatedRecipientByPeriod)]
  Data[, Family_Income := NULL]
}
attr(CalculateCoreBenefits, "output") <- c("P_Benefits_CoreBenefits_CalculatedRecipientByPeriod","P_Benefits_CoreBenefits_UnabatedAmount",
                                           "P_Benefits_CoreBenefits_AbatedAmount","P_Benefits_CoreBenefits_GrossAmount",
                                           "P_Benefits_CoreBenefits_TaxAmount","P_Benefits_JSS_Amount_Abated","P_Benefits_SLP_Amount_Abated",
                                           "P_Benefits_SPS_Amount_Abated","P_Benefits_JSS_Amount_Gross","P_Benefits_SLP_Amount_Gross",
                                           "P_Benefits_SPS_Amount_Gross","P_Benefits_JSS_Amount_Tax","P_Benefits_SLP_Amount_Tax","P_Benefits_SPS_Amount_Tax",
                                           "P_Benefits_JSS_CalculatedRecipientByPeriod","P_Benefits_SLP_CalculatedRecipientByPeriod",
                                           "P_Benefits_SPS_CalculatedRecipientByPeriod")
attr(CalculateCoreBenefits, "input")  <- c("F_Attributes_IsCouple", "P_Income_PrivateChargeable",
                                           "P_Benefits_JSS_Amount_Unabated", "P_Benefits_SPS_Amount_Unabated", "P_Benefits_SLP_Amount_Unabated",
                                           "P_Attributes_SpouseOfPrincipal", "P_Attributes_PrincipalEarner","F_Counts_Dependents", "F_ID", "Period", "P_Super_OnSuper")
