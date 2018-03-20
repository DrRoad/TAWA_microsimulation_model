
FamilyAssistanceAbatement <- function(Data, 
                                      FamilyAssistance_PTC_Weeks,
                                      FamilyAssistance_PTC_AbateSeparately, 
                                      FamilyAssistance_PTC_AbatementCliffFace, 
                                      Periods = 1){
  periodsOfPTC = as.integer(ceiling(FamilyAssistance_PTC_Weeks/52.2*24-1e-8))
  Data[F_FamilyAssistance_WorkingVariables_RingFencedByPeriod == 0,
       ':='(P_FamilyAssistance_FTC_Abated = pmax(0.0,
                                                 P_FamilyAssistance_FTC_Unabated -
                                                   F_FamilyAssistance_WorkingVariables_AbateAmount),
            Temporary_variable_remaining_abatement_FTC = pmax(0.0,
                                                          F_FamilyAssistance_WorkingVariables_AbateAmount-
                                                            P_FamilyAssistance_FTC_Unabated))]
  Data[F_FamilyAssistance_WorkingVariables_RingFencedByPeriod == 0,
       ':='(P_FamilyAssistance_IWTC_Abated = pmax(0.0,
                                                  P_FamilyAssistance_IWTC_Unabated - Temporary_variable_remaining_abatement_FTC),
            Temporary_variable_remaining_abatement_IWTC = pmax(0.0,
                                                          Temporary_variable_remaining_abatement_FTC-
                                                            P_FamilyAssistance_IWTC_Unabated))]
  if (FamilyAssistance_PTC_AbateSeparately == FALSE){
    Data[, Temporary_number_PTC_periods := 0]
    Data[(P_Attributes_Dependent==T) &
           (P_Attributes_Age==0) & 
           (P_Attributes_BirthPeriod<(25 - periodsOfPTC/2)), Temporary_number_PTC_periods := periodsOfPTC]
    Data[(P_Attributes_Dependent==T) &
           (P_Attributes_Age==0) & 
           (P_Attributes_BirthPeriod>=(25 - periodsOfPTC/2)), Temporary_number_PTC_periods := periodsOfPTC/2]
    Data[(P_Attributes_Dependent==T) &
           (P_Attributes_Age==1) & 
           (P_Attributes_BirthPeriod>=(25 - periodsOfPTC/2)), Temporary_number_PTC_periods := periodsOfPTC/2]   
    Data[, Temporary_number_PTC_periods := max(Temporary_number_PTC_periods), by = F_ID]
    Data[(F_FamilyAssistance_WorkingVariables_RingFencedByPeriod == 0),
         ':=' (P_FamilyAssistance_PTC_Abated = 
                 pmax(0, P_FamilyAssistance_PTC_Unabated - 
                                                 Temporary_variable_remaining_abatement_IWTC*Temporary_number_PTC_periods/24))]
    Data[, ':='(Temporary_number_PTC_periods = NULL)]
  } else {
    Data[F_FamilyAssistance_WorkingVariables_RingFencedByPeriod == 0, 
         Temporary_variable_TotalWFFIncome := sum(P_Income_WfFIncome*(P_Attributes_Dependent==0)), 
         by = .(F_ID, Period)]
    Data[F_FamilyAssistance_WorkingVariables_RingFencedByPeriod == 0, 
         P_FamilyAssistance_PTC_Abated :=  
           P_FamilyAssistance_PTC_Unabated*(Temporary_variable_TotalWFFIncome<FamilyAssistance_PTC_AbatementCliffFace/Periods)]
    Data[, ':='(Temporary_variable_TotalWFFIncome = NULL)]
  }
  Data[F_FamilyAssistance_WorkingVariables_RingFencedByPeriod == 1, 
       ':='(P_FamilyAssistance_IWTC_Abated = as.numeric(P_FamilyAssistance_IWTC_Unabated),
            P_FamilyAssistance_FTC_Abated = as.numeric(P_FamilyAssistance_FTC_Unabated),
            P_FamilyAssistance_PTC_Abated = as.numeric(P_FamilyAssistance_PTC_Unabated))]
  Data[, ':='(Temporary_variable_remaining_abatement_FTC = NULL,
              Temporary_variable_remaining_abatement_IWTC = NULL)]
}
attr(FamilyAssistanceAbatement, "output") <- 
  c("P_FamilyAssistance_IWTC_Abated",
    "P_FamilyAssistance_FTC_Abated",
    "P_FamilyAssistance_PTC_Abated")
attr(FamilyAssistanceAbatement, "input")  <- 
  c("F_ID",
    "Period",
    "F_FamilyAssistance_WorkingVariables_RingFencedByPeriod",
    "P_FamilyAssistance_FTC_Unabated",
    "P_FamilyAssistance_IWTC_Unabated",
    "P_FamilyAssistance_PTC_Unabated",
    "P_Income_WfFIncome",
    "F_FamilyAssistance_WorkingVariables_AbateAmount",
    "P_Attributes_Dependent",
    "P_Attributes_Age",
    "P_Attributes_BirthPeriod")
