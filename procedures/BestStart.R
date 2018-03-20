
BestStart <- function(Data, 
                      FamilyAssistance_BestStart_Abatement_AbatementScale,
                      FamilyAssistance_BestStart_Rates_Age0, 
                      FamilyAssistance_BestStart_Rates_Age1or2,
                      modelyear,
                      Periods = 1){
  Data[, F_FamilyAssistance_ParentalLeave_Amount_Calculated := 
       sum(P_FamilyAssistance_ParentalLeave_Amount_Calculated), by=F_ID]
  Data[, F_Num_Age0 := sum(P_Attributes_Age==0), by=F_ID]
  Data[, F_Num_Age1 := sum(P_Attributes_Age==1), by=F_ID]
  Data[, F_Num_Age2 := sum(P_Attributes_Age==2), by=F_ID]
  Data[, P_FamilyAssistance_BestStart0 := 0]
  Data[, P_FamilyAssistance_BestStart12 := 0]
  Data[(F_FamilyAssistance_ParentalLeave_Amount_Calculated == 0) & (modelyear >= 2019), P_FamilyAssistance_BestStart0 := P_FamilyAssistance_BestStart0 + FamilyAssistance_BestStart_Rates_Age0 * F_Num_Age0]
  Data[(F_FamilyAssistance_ParentalLeave_Amount_Calculated > 0) & (modelyear == 2019), P_FamilyAssistance_BestStart0 := P_FamilyAssistance_BestStart0 + (7/12) * FamilyAssistance_BestStart_Rates_Age0 * F_Num_Age0]
  Data[(F_FamilyAssistance_ParentalLeave_Amount_Calculated > 0) & (modelyear == 2020), P_FamilyAssistance_BestStart0 := P_FamilyAssistance_BestStart0 + (7/12) * FamilyAssistance_BestStart_Rates_Age0 * F_Num_Age0]
  Data[(F_FamilyAssistance_ParentalLeave_Amount_Calculated > 0) & (modelyear >= 2021), P_FamilyAssistance_BestStart0 := P_FamilyAssistance_BestStart0 + (1/2) * FamilyAssistance_BestStart_Rates_Age0 * F_Num_Age0]
  Data[(modelyear >= 2020), P_FamilyAssistance_BestStart12 := P_FamilyAssistance_BestStart12 + FamilyAssistance_BestStart_Rates_Age1or2 * F_Num_Age1]
  Data[(modelyear >= 2021), P_FamilyAssistance_BestStart12 := P_FamilyAssistance_BestStart12 + FamilyAssistance_BestStart_Rates_Age1or2 * F_Num_Age2]
  Data[, P_FamilyAssistance_BestStart0 := P_FamilyAssistance_BestStart0 * (P_Attributes_PrincipalEarner==1)]
  Data[, P_FamilyAssistance_BestStart12 := P_FamilyAssistance_BestStart12 * (P_Attributes_PrincipalEarner==1)]
  Data[, F_Income_WfFIncome := sum(P_Income_WfFIncome * (P_Attributes_Dependent == 0)), by=F_ID]
  Data[, P_FamilyAssistance_BestStart12_Abated :=
              Abate(T,
                    P_FamilyAssistance_BestStart12,
                    FamilyAssistance_BestStart_Abatement_AbatementScale, Data[,F_Income_WfFIncome])]
  Data[, P_FamilyAssistance_BestStart := P_FamilyAssistance_BestStart0 + P_FamilyAssistance_BestStart12_Abated] 
  Data[, F_FamilyAssistance_ParentalLeave_Amount_Calculated := NULL]
  Data[, F_Num_Age0 := NULL]
  Data[, F_Num_Age1 := NULL]
  Data[, F_Num_Age2 := NULL]
  Data[, P_FamilyAssistance_BestStart0 := NULL]
  Data[, P_FamilyAssistance_BestStart12 := NULL]
  Data[, P_FamilyAssistance_BestStart12_Abated := NULL]
  Data[, F_Income_WfFIncome := NULL]
}
attr(BestStart, "output") <- 
  c("P_FamilyAssistance_BestStart") 
attr(BestStart, "input")  <- 
  c("F_ID", 
    "P_Attributes_PrincipalEarner",
    "P_FamilyAssistance_ParentalLeave_Amount_Calculated",
    "P_Attributes_Age",
    "P_Income_WfFIncome",
    "P_Attributes_Dependent"
    )