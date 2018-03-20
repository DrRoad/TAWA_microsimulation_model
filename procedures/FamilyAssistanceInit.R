
FamilyAssistanceInit <- function(Data, FamilyAssistance_Abatement_AbatementScale, Periods = 1){
  Data[, ":=" 
       (F_People_NumChildAge16to18 = sum((P_Attributes_Age>=16) &
                                           (P_Attributes_Age<=18) &
                                           (P_Attributes_Dependent == 1))/Periods,
       F_People_NumChildAge13to15 = sum((P_Attributes_Age>=13) &
                                          (P_Attributes_Age<=15) &
                                          (P_Attributes_Dependent == 1))/Periods,
       F_People_NumChildAge0to12 = sum((P_Attributes_Age<=12) &
                                         (P_Attributes_Dependent == 1))/Periods,
       Temporary_variable_Dependents = sum( (P_Attributes_Dependent == 1))/Periods,
       Temporary_variable_TotalWffIncome = 
         sum(P_Income_WfFIncome*(P_Attributes_Dependent==0))),
       by = .(F_ID)]
  Data[, ":=" 
       (F_FamilyAssistance_WorkingVariables_AbateAmount = 
         Apply(Temporary_variable_TotalWffIncome, FamilyAssistance_Abatement_AbatementScale)/Periods,
       P_Attributes_Carer = as.numeric(Temporary_variable_Dependents > 0 & 
                                         (P_Attributes_SpouseOfPrincipal | 
                                            ((F_Attributes_IsCouple == 0) & 
                                               (P_Attributes_PrincipalEarner)))))]
  Data[, ":=" 
       (Temporary_variable_TotalWffIncome = NULL, 
       Temporary_variable_Dependents = NULL)]
}
attr(FamilyAssistanceInit, "output") <- 
  c("P_Attributes_Carer",
    "F_People_NumChildAge16to18",
    "F_People_NumChildAge13to15",
    "F_People_NumChildAge0to12",
    "F_FamilyAssistance_WorkingVariables_AbateAmount")
attr(FamilyAssistanceInit, "input")  <- 
  c("F_ID", 
    "F_Attributes_IsCouple",
    "P_Income_WfFIncome",
    "P_Attributes_PrincipalEarner",
    "P_Attributes_SpouseOfPrincipal",
    "P_Attributes_Age",
    "P_Attributes_Dependent")
