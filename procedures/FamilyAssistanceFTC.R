
FamilyAssistanceFTC <- function(Data, FamilyAssistance_FTC_Rates_FirstChild16to18, FamilyAssistance_FTC_Rates_FirstChild0to15, FamilyAssistance_FTC_Rates_SecondChild16to18, FamilyAssistance_FTC_Rates_SecondChild13to15, FamilyAssistance_FTC_Rates_SecondChild0to12, Periods = 1){
    Data[, Temp_NumChildAge16to18 := F_People_NumChildAge16to18]
    Data[, Temp_NumChildAge13to15 := F_People_NumChildAge13to15]    
    Data[, Temp_NumChildAge0to12 := F_People_NumChildAge0to12]
    Data[, P_FamilyAssistance_FTC_Unabated := 
            FamilyAssistance_FTC_Rates_FirstChild16to18 * (Temp_NumChildAge16to18 > 0)]
    Data[, Temp_NumChildAge16to18 := pmax(Temp_NumChildAge16to18 - 1L, 0L)]
    Data[F_People_NumChildAge16to18 == 0,
         P_FamilyAssistance_FTC_Unabated := 
            FamilyAssistance_FTC_Rates_FirstChild0to15 * (Temp_NumChildAge13to15 > 0)]
    Data[F_People_NumChildAge16to18 == 0, 
         Temp_NumChildAge13to15 := pmax(Temp_NumChildAge13to15 - 1L, 0L)]
    Data[(F_People_NumChildAge16to18 == 0) & (F_People_NumChildAge13to15 == 0),
         P_FamilyAssistance_FTC_Unabated := 
            FamilyAssistance_FTC_Rates_FirstChild0to15 * (Temp_NumChildAge0to12 > 0)]
    Data[(F_People_NumChildAge16to18 == 0) & (F_People_NumChildAge13to15 == 0), 
         Temp_NumChildAge0to12 := pmax(Temp_NumChildAge0to12 - 1L, 0L)]
    Data[, P_FamilyAssistance_FTC_Unabated := P_FamilyAssistance_FTC_Unabated +
               FamilyAssistance_FTC_Rates_SecondChild16to18 * Temp_NumChildAge16to18 +
               FamilyAssistance_FTC_Rates_SecondChild13to15 * Temp_NumChildAge13to15 +
               FamilyAssistance_FTC_Rates_SecondChild0to12 * Temp_NumChildAge0to12]
    Data[P_Attributes_Carer == 0, P_FamilyAssistance_FTC_Unabated := 0]
    Data[, `:=` (Temp_NumChildAge16to18 = NULL,
                 Temp_NumChildAge13to15 = NULL,
                 Temp_NumChildAge0to12 = NULL)]
}
attr(FamilyAssistanceFTC, "output") <- 
    c("P_FamilyAssistance_FTC_Unabated"
     )
attr(FamilyAssistanceFTC, "input")  <- 
    c("F_People_NumChildAge16to18",
      "F_People_NumChildAge13to15",
      "F_People_NumChildAge0to12",
      "P_Attributes_Carer"
    )
