
SuperSpell <- function(Data, Super_EligibleAge, Periods = 1){
    Data[, Sharing := 1]
    Data[H_Counts_Families > 1, Sharing := 2]
    Data[, IndependentlyQualifies := 0]
    Data[(P_Attributes_Age >= Super_EligibleAge) &
         (P_Super_Amount_Raw_Gross > 0), 
         IndependentlyQualifies := 1]
    Data[, `:=` (P_Super_OnSuper = 0,
                 F_Attributes_OnSuper = 0)]
    Data[(P_Attributes_PrincipalEarner == 1) &
         (F_Attributes_IsCouple == 0) &
         (IndependentlyQualifies == 1),
         `:=` (F_Attributes_OnSuper = 2,
               P_Super_OnSuper = 2)]    
    Data[(P_Attributes_PrincipalEarner == 1) &
         (F_Attributes_IsCouple == 0) &
         (IndependentlyQualifies == 1) &
         (Sharing == 2),
         P_Super_OnSuper := 1] 
    Data[(F_Attributes_IsCouple == 0), 
         F_Attributes_OnSuper := sum(F_Attributes_OnSuper),
         by = F_ID]        
    Data[(P_Attributes_PrincipalEarner == 1) &
         (F_Attributes_IsCouple == 1) &
         (IndependentlyQualifies == 1),
         P_Super_OnSuper := 2]
    Data[(P_Attributes_SpouseOfPrincipal == 1) &
         (F_Attributes_IsCouple == 1) &
         (IndependentlyQualifies == 1),
         P_Super_OnSuper := 2]         
    Data[(F_Attributes_IsCouple == 1), 
         F_Attributes_OnSuper := sum(P_Super_OnSuper) / 2, 
         by = F_ID]
    Data[, c("Sharing", "IndependentlyQualifies") := NULL]
}
attr(SuperSpell, "output") <- 
    c("F_Attributes_OnSuper",
      "P_Super_OnSuper"
     )
attr(SuperSpell, "input")  <- 
    c(
      "F_ID",
      "P_Attributes_PrincipalEarner",
      "P_Attributes_SpouseOfPrincipal",
      "F_Attributes_IsCouple",
      "P_Attributes_Age",
      "P_Super_Amount_Raw_Gross",
      "H_Counts_Families"
     )
