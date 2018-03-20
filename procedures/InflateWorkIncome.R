
InflateWorkIncome <- function(Data, Periods = 1,
                              inflator_InflationMultiplier_earnings,
                              inflator_InflationMultiplier_selfemp){
   AllJobs = seq(1,9)
   for (job in AllJobs){
        Data[, multiplier := 1.0]
        Data[, WageOrSelfEmployed := .SD,
             .SDcols = paste0('P_Jobs_', job, '_WageOrSelfEmployed')]
        Data[WageOrSelfEmployed == 'se', 
             multiplier := inflator_InflationMultiplier_selfemp]
        Data[WageOrSelfEmployed == 'ws', 
             multiplier := inflator_InflationMultiplier_earnings]
        Data[, paste0('P_Income_Raw_Jobs_', job, '_GrossAmountInflated') :=
             multiplier * .SD,
             .SDcols = paste0('P_Income_Raw_Jobs_', job, '_GrossAmount')]
   }             
   Data[, P_Income_Raw_Jobs_All_GrossAmountInflated := rowSums(.SD),
        .SDcols = paste0('P_Income_Raw_Jobs_', seq(1,9), '_GrossAmountInflated')]
   Data[, F_Attributes_IsCouple := max(P_Relationships_IsCouple), by=F_ID]
   Data[, `:=` (WageOrSelfEmployed = NULL, multiplier = NULL)]
}
attr(InflateWorkIncome, "output") <- 
    c("P_Income_Raw_Jobs_1_GrossAmountInflated",
      "P_Income_Raw_Jobs_2_GrossAmountInflated",
      "P_Income_Raw_Jobs_3_GrossAmountInflated",
      "P_Income_Raw_Jobs_4_GrossAmountInflated",
      "P_Income_Raw_Jobs_5_GrossAmountInflated",
      "P_Income_Raw_Jobs_6_GrossAmountInflated",
      "P_Income_Raw_Jobs_7_GrossAmountInflated",
      "P_Income_Raw_Jobs_8_GrossAmountInflated",
      "P_Income_Raw_Jobs_9_GrossAmountInflated",
      "P_Income_Raw_Jobs_All_GrossAmountInflated",
      "F_Attributes_IsCouple")
attr(InflateWorkIncome, "input")  <- c("F_ID",
                                       "P_Relationships_IsCouple",
                                       "P_Income_Raw_Jobs_1_GrossAmount",
                                       "P_Income_Raw_Jobs_2_GrossAmount",
                                       "P_Income_Raw_Jobs_3_GrossAmount",
                                       "P_Income_Raw_Jobs_4_GrossAmount",
                                       "P_Income_Raw_Jobs_5_GrossAmount",
                                       "P_Income_Raw_Jobs_6_GrossAmount",
                                       "P_Income_Raw_Jobs_7_GrossAmount",
                                       "P_Income_Raw_Jobs_8_GrossAmount",
                                       "P_Income_Raw_Jobs_9_GrossAmount",
                                       "P_Jobs_1_WageOrSelfEmployed",
                                       "P_Jobs_2_WageOrSelfEmployed",
                                       "P_Jobs_3_WageOrSelfEmployed",
                                       "P_Jobs_4_WageOrSelfEmployed",
                                       "P_Jobs_5_WageOrSelfEmployed",
                                       "P_Jobs_6_WageOrSelfEmployed",
                                       "P_Jobs_7_WageOrSelfEmployed",
                                       "P_Jobs_8_WageOrSelfEmployed",
                                       "P_Jobs_9_WageOrSelfEmployed")
