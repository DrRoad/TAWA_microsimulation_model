
ProcessPeriodData <- function(Data, Periods = 1){
  weeks_in_year = 52.2
  AllJobs = seq(1,9)
  Data[, `:=` (P_Benefits_Processed_SLP_RecipientByPeriod = 
                  P_Benefits_SLP_RecipientByPeriod,
               P_Benefits_Processed_SPS_RecipientByPeriod =
                  P_Benefits_SPS_RecipientByPeriod,
               P_Benefits_Processed_JSS_RecipientByPeriod =
                  P_Benefits_JSS_RecipientByPeriod)]
  Data[, ':=' (P_Income_Processed_Jobs_All_AmountByPeriod = P_Income_Raw_Jobs_All_AmountByPeriod,
               P_Jobs_Processed_All_HoursWorkedByPeriod = P_Jobs_All_HoursWorkedByPeriod)]
  Data[, (paste0("P_Jobs_Processed_", AllJobs, "_ACCWeeks")) := 0]
  Data[, (paste0("P_Jobs_Processed_", AllJobs, "_PPLWeeks")) := 0]
  Data[, (paste0("P_Jobs_Processed_", AllJobs, "_WeeksOnLeaveWithoutPay")) := 0]
  Data[, "P_Work_Processed_NumJobsOverPreviousYear" := rowSums(.SD == 24), .SDcol = paste0("P_Jobs_", AllJobs, "_EndPeriod")]
  Data[, TemporaryVar_hours_worked_per_week := 0]
  for (job in AllJobs){
    Data[, TemporaryVar_current_job := (.SD == 24), .SDcols = paste0("P_Jobs_", job, "_EndPeriod") ]
    Data[, TemporaryVar_fulltime_job := (.SD == 1), .SDcols = paste0("P_Jobs_", job, "_FullTime") ]
    Data[,
         c(paste0("P_Jobs_Processed_", job, "_StartPeriod"),
           paste0("P_Jobs_Processed_", job, "_EndPeriod"),
           paste0("P_Jobs_Processed_", job, "_NumPeriodsWorked"),
           paste0("P_Jobs_Processed_", job, "_FullTimeEquivWeeks")) :=
           list(1 * TemporaryVar_current_job,
                24 * TemporaryVar_current_job,
                24 * TemporaryVar_current_job,
                weeks_in_year * TemporaryVar_current_job * TemporaryVar_fulltime_job )]
    Data[,
         TemporaryVar_hours_worked_per_week := 
           TemporaryVar_hours_worked_per_week + TemporaryVar_current_job*.SD,
         .SDcols = paste0("P_Jobs_", job, "_HoursPerWeek")]
  }
  Data[, ':=' (P_Work_Processed_FullTimeWeeks = 0, 
               P_Work_Processed_PartTimeWeeks = 0, 
               P_Work_Processed_WeeksNotWorking = 0)]
  Data[TemporaryVar_hours_worked_per_week >= 30, P_Work_Processed_FullTimeWeeks := weeks_in_year]
  Data[(TemporaryVar_hours_worked_per_week < 30) & (TemporaryVar_hours_worked_per_week > 0), 
       P_Work_Processed_PartTimeWeeks := weeks_in_year]
  Data[TemporaryVar_hours_worked_per_week == 0, P_Work_Processed_WeeksNotWorking := weeks_in_year]    
  Data[, TemporaryVar_hours_worked_per_week := NULL]
  Data[, TemporaryVar_current_job := NULL]
  Data[, TemporaryVar_fulltime_job := NULL]
  Data[, ':=' (P_Work_Processed_NumPreviousJobs = 0,
               P_Jobs_Processed_All_FullTimeEquivWeeks = weeks_in_year * P_Jobs_All_CurrentFullTime,
               P_Super_Processed_StartPeriod = ceiling(P_Super_StartPeriod/24))]
  CurrentJobs = seq(1,3)
  Data[, (paste0("P_Jobs_Processed_", CurrentJobs, "_FullTime")) := .SD, 
       .SDcols =  paste0("P_Jobs_", CurrentJobs, "_FullTime")]
  Data[, (paste0("P_Jobs_Processed_", CurrentJobs, "_HoursPerWeek")) := .SD, 
       .SDcols = paste0("P_Jobs_", CurrentJobs, "_HoursPerWeek")]
  Data[, (paste0("P_Jobs_Processed_", CurrentJobs, "_WageOrSelfEmployed")) := .SD, 
       .SDcols = paste0("P_Jobs_", CurrentJobs, "_WageOrSelfEmployed")]
  Data[, ':=' (P_Income_Processed_ACCReceived = 0,
               P_FamilyAssistance_ParentalLeave_Amount_Processed = P_FamilyAssistance_ParentalLeave_Amount_RawSurvey)]
  PastJobs = seq(4,9)
  Data[, (paste0("P_Jobs_Processed_", PastJobs, "_FullTime")) := 0]
  Data[, (paste0("P_Jobs_Processed_", PastJobs, "_HoursPerWeek")) := 0]
  Data[, (paste0("P_Jobs_Processed_", PastJobs, "_WageOrSelfEmployed")) := ""]
  Data[, (paste0("P_Income_Processed_Jobs_", PastJobs, "_GrossAmountInflated")) := 0]
  Data[, (paste0("P_Income_Processed_Redundancy_Job", PastJobs)) := 0]
  Data[, ':=' (P_Income_Processed_OtherIrregular = 0,
               P_Income_Processed_OtherIrregularOverseas = 0,
               P_Income_Processed_OtherNonTaxableLumpSum = 0,
               P_Income_Processed_LifeInsuranceLumpSum = 0)]
  Data[, paste0("P_Income_Processed_Jobs_", CurrentJobs, "_GrossAmountInflated") := 0 ]
  for (job in CurrentJobs)
  {
    Data[, TemporaryVariable_EndPeriod := .SD, .SDcols = paste0("P_Jobs_", job, "_EndPeriod")] 
    Data[, TemporaryVariable_StartPeriod := .SD, .SDcols = paste0("P_Jobs_", job, "_StartPeriod")]    
    Data[, TemporaryVariable_PeriodsNotWorking := 
           (24/weeks_in_year) * rowSums(.SD), 
         .SDcols = c(paste0("P_Jobs_", job, "_ACCWeeks"), 
                     paste0("P_Jobs_", job, "_PPLWeeks"), 
                     paste0("P_Jobs_", job, "_WeeksOnLeaveWithoutPay"))]
    Data[, TemporaryVariable_PeriodsWorking := 
           TemporaryVariable_EndPeriod - 
           TemporaryVariable_StartPeriod + 1 - 
           TemporaryVariable_PeriodsNotWorking]
    Data[,
          paste0("P_Income_Processed_Jobs_", job, "_GrossAmountInflated") :=
            .SD/(TemporaryVariable_PeriodsWorking)*24*
           (TemporaryVariable_EndPeriod == 24) *(TemporaryVariable_PeriodsWorking > 0), 
         .SDcols = paste0("P_Income_Raw_Jobs_", job, "_GrossAmountInflated") ]
    Data[, TemporaryVariable_PeriodsWorking := NULL] 
    Data[, TemporaryVariable_PeriodsNotWorking := NULL]
    Data[, TemporaryVariable_EndPeriod := NULL] 
    Data[, TemporaryVariable_StartPeriod := NULL]
  }
}
attr(ProcessPeriodData, "output") <- c("P_Jobs_Processed_1_ACCWeeks","P_Jobs_Processed_2_ACCWeeks","P_Jobs_Processed_3_ACCWeeks","P_Jobs_Processed_4_ACCWeeks","P_Jobs_Processed_5_ACCWeeks","P_Jobs_Processed_6_ACCWeeks","P_Jobs_Processed_7_ACCWeeks","P_Jobs_Processed_8_ACCWeeks","P_Jobs_Processed_9_ACCWeeks",
                                       "P_Jobs_Processed_1_PPLWeeks","P_Jobs_Processed_2_PPLWeeks","P_Jobs_Processed_3_PPLWeeks","P_Jobs_Processed_4_PPLWeeks","P_Jobs_Processed_5_PPLWeeks","P_Jobs_Processed_6_PPLWeeks","P_Jobs_Processed_7_PPLWeeks","P_Jobs_Processed_8_PPLWeeks","P_Jobs_Processed_9_PPLWeeks",
                                       "P_Jobs_Processed_1_EndPeriod","P_Jobs_Processed_2_EndPeriod","P_Jobs_Processed_3_EndPeriod","P_Jobs_Processed_4_EndPeriod","P_Jobs_Processed_5_EndPeriod","P_Jobs_Processed_6_EndPeriod","P_Jobs_Processed_7_EndPeriod","P_Jobs_Processed_8_EndPeriod","P_Jobs_Processed_9_EndPeriod",
                                       "P_Jobs_Processed_1_FullTimeEquivWeeks","P_Jobs_Processed_2_FullTimeEquivWeeks","P_Jobs_Processed_3_FullTimeEquivWeeks","P_Jobs_Processed_4_FullTimeEquivWeeks","P_Jobs_Processed_5_FullTimeEquivWeeks","P_Jobs_Processed_6_FullTimeEquivWeeks","P_Jobs_Processed_7_FullTimeEquivWeeks","P_Jobs_Processed_8_FullTimeEquivWeeks","P_Jobs_Processed_9_FullTimeEquivWeeks",
                                       "P_Jobs_Processed_1_NumPeriodsWorked","P_Jobs_Processed_2_NumPeriodsWorked","P_Jobs_Processed_3_NumPeriodsWorked","P_Jobs_Processed_4_NumPeriodsWorked","P_Jobs_Processed_5_NumPeriodsWorked","P_Jobs_Processed_6_NumPeriodsWorked","P_Jobs_Processed_7_NumPeriodsWorked","P_Jobs_Processed_8_NumPeriodsWorked","P_Jobs_Processed_9_NumPeriodsWorked",
                                       "P_Jobs_Processed_1_StartPeriod","P_Jobs_Processed_2_StartPeriod","P_Jobs_Processed_3_StartPeriod","P_Jobs_Processed_4_StartPeriod","P_Jobs_Processed_5_StartPeriod","P_Jobs_Processed_6_StartPeriod","P_Jobs_Processed_7_StartPeriod","P_Jobs_Processed_8_StartPeriod","P_Jobs_Processed_9_StartPeriod",
                                       "P_Jobs_Processed_1_WeeksOnLeaveWithoutPay","P_Jobs_Processed_2_WeeksOnLeaveWithoutPay","P_Jobs_Processed_3_WeeksOnLeaveWithoutPay","P_Jobs_Processed_4_WeeksOnLeaveWithoutPay","P_Jobs_Processed_5_WeeksOnLeaveWithoutPay","P_Jobs_Processed_6_WeeksOnLeaveWithoutPay","P_Jobs_Processed_7_WeeksOnLeaveWithoutPay","P_Jobs_Processed_8_WeeksOnLeaveWithoutPay","P_Jobs_Processed_9_WeeksOnLeaveWithoutPay",
                                       "P_Jobs_Processed_All_FullTimeEquivWeeks",
                                       "P_Super_Processed_StartPeriod",
                                       "P_Work_Processed_FullTimeWeeks",
                                       "P_Work_Processed_NumJobsOverPreviousYear",
                                       "P_Work_Processed_NumPreviousJobs",
                                       "P_Work_Processed_PartTimeWeeks",
                                       "P_Work_Processed_WeeksNotWorking",
                                       "P_Benefits_Processed_JSS_RecipientByPeriod",
                                       "P_Benefits_Processed_SLP_RecipientByPeriod",
                                       "P_Benefits_Processed_SPS_RecipientByPeriod",
                                       "P_Income_Processed_Jobs_All_AmountByPeriod",
                                       "P_Jobs_Processed_All_HoursWorkedByPeriod",
                                       "P_Income_Processed_ACCReceived",
                                       "P_FamilyAssistance_ParentalLeave_Amount_Processed",
                                       "P_Income_Processed_Redundancy_Job4","P_Income_Processed_Redundancy_Job5","P_Income_Processed_Redundancy_Job6","P_Income_Processed_Redundancy_Job7","P_Income_Processed_Redundancy_Job8","P_Income_Processed_Redundancy_Job9",
                                       "P_Income_Processed_OtherIrregular",
                                       "P_Income_Processed_OtherIrregularOverseas",
                                       "P_Income_Processed_OtherNonTaxableLumpSum","P_Income_Processed_LifeInsuranceLumpSum",
                                       "P_Income_Processed_Jobs_1_GrossAmountInflated","P_Income_Processed_Jobs_2_GrossAmountInflated","P_Income_Processed_Jobs_3_GrossAmountInflated","P_Income_Processed_Jobs_4_GrossAmountInflated","P_Income_Processed_Jobs_5_GrossAmountInflated","P_Income_Processed_Jobs_6_GrossAmountInflated","P_Income_Processed_Jobs_7_GrossAmountInflated","P_Income_Processed_Jobs_8_GrossAmountInflated","P_Income_Processed_Jobs_9_GrossAmountInflated",
                                       "P_Jobs_Processed_1_FullTime","P_Jobs_Processed_2_FullTime","P_Jobs_Processed_3_FullTime","P_Jobs_Processed_4_FullTime","P_Jobs_Processed_5_FullTime","P_Jobs_Processed_6_FullTime","P_Jobs_Processed_7_FullTime","P_Jobs_Processed_8_FullTime","P_Jobs_Processed_9_FullTime",
                                       "P_Jobs_Processed_1_HoursPerWeek","P_Jobs_Processed_2_HoursPerWeek","P_Jobs_Processed_3_HoursPerWeek","P_Jobs_Processed_4_HoursPerWeek","P_Jobs_Processed_5_HoursPerWeek","P_Jobs_Processed_6_HoursPerWeek","P_Jobs_Processed_7_HoursPerWeek","P_Jobs_Processed_8_HoursPerWeek","P_Jobs_Processed_9_HoursPerWeek",
                                       "P_Jobs_Processed_1_WageOrSelfEmployed","P_Jobs_Processed_2_WageOrSelfEmployed","P_Jobs_Processed_3_WageOrSelfEmployed","P_Jobs_Processed_4_WageOrSelfEmployed","P_Jobs_Processed_5_WageOrSelfEmployed","P_Jobs_Processed_6_WageOrSelfEmployed","P_Jobs_Processed_7_WageOrSelfEmployed","P_Jobs_Processed_8_WageOrSelfEmployed","P_Jobs_Processed_9_WageOrSelfEmployed")
attr(ProcessPeriodData, "input")  <- c("P_Income_Raw_Jobs_1_GrossAmountInflated","P_Income_Raw_Jobs_2_GrossAmountInflated","P_Income_Raw_Jobs_3_GrossAmountInflated","P_Income_Raw_Jobs_4_GrossAmountInflated","P_Income_Raw_Jobs_5_GrossAmountInflated","P_Income_Raw_Jobs_6_GrossAmountInflated","P_Income_Raw_Jobs_7_GrossAmountInflated","P_Income_Raw_Jobs_8_GrossAmountInflated","P_Income_Raw_Jobs_9_GrossAmountInflated",
                                       "P_Jobs_1_ACCWeeks","P_Jobs_2_ACCWeeks","P_Jobs_3_ACCWeeks","P_Jobs_4_ACCWeeks","P_Jobs_5_ACCWeeks","P_Jobs_6_ACCWeeks","P_Jobs_7_ACCWeeks","P_Jobs_8_ACCWeeks","P_Jobs_9_ACCWeeks",
                                       "P_Jobs_1_PPLWeeks","P_Jobs_2_PPLWeeks","P_Jobs_3_PPLWeeks","P_Jobs_4_PPLWeeks","P_Jobs_5_PPLWeeks","P_Jobs_6_PPLWeeks","P_Jobs_7_PPLWeeks","P_Jobs_8_PPLWeeks","P_Jobs_9_PPLWeeks",
                                       "P_Jobs_1_EndPeriod","P_Jobs_2_EndPeriod","P_Jobs_3_EndPeriod","P_Jobs_4_EndPeriod","P_Jobs_5_EndPeriod","P_Jobs_6_EndPeriod","P_Jobs_7_EndPeriod","P_Jobs_8_EndPeriod","P_Jobs_9_EndPeriod",
                                       "P_Jobs_1_FullTimeEquivWeeks","P_Jobs_2_FullTimeEquivWeeks","P_Jobs_3_FullTimeEquivWeeks","P_Jobs_4_FullTimeEquivWeeks","P_Jobs_5_FullTimeEquivWeeks","P_Jobs_6_FullTimeEquivWeeks","P_Jobs_7_FullTimeEquivWeeks","P_Jobs_8_FullTimeEquivWeeks","P_Jobs_9_FullTimeEquivWeeks",
                                       "P_Jobs_1_NumPeriodsWorked","P_Jobs_2_NumPeriodsWorked","P_Jobs_3_NumPeriodsWorked","P_Jobs_4_NumPeriodsWorked","P_Jobs_5_NumPeriodsWorked","P_Jobs_6_NumPeriodsWorked","P_Jobs_7_NumPeriodsWorked","P_Jobs_8_NumPeriodsWorked","P_Jobs_9_NumPeriodsWorked",
                                       "P_Jobs_1_StartPeriod","P_Jobs_2_StartPeriod","P_Jobs_3_StartPeriod","P_Jobs_4_StartPeriod","P_Jobs_5_StartPeriod","P_Jobs_6_StartPeriod","P_Jobs_7_StartPeriod","P_Jobs_8_StartPeriod","P_Jobs_9_StartPeriod",
                                       "P_Jobs_1_WeeksOnLeaveWithoutPay","P_Jobs_2_WeeksOnLeaveWithoutPay","P_Jobs_3_WeeksOnLeaveWithoutPay","P_Jobs_4_WeeksOnLeaveWithoutPay","P_Jobs_5_WeeksOnLeaveWithoutPay","P_Jobs_6_WeeksOnLeaveWithoutPay","P_Jobs_7_WeeksOnLeaveWithoutPay","P_Jobs_8_WeeksOnLeaveWithoutPay","P_Jobs_9_WeeksOnLeaveWithoutPay",
                                       "P_Super_StartPeriod",
                                       "P_Jobs_1_FullTime","P_Jobs_2_FullTime","P_Jobs_3_FullTime","P_Jobs_4_FullTime","P_Jobs_5_FullTime","P_Jobs_6_FullTime","P_Jobs_7_FullTime","P_Jobs_8_FullTime","P_Jobs_9_FullTime",
                                       "P_Jobs_All_CurrentFullTime",
                                       "P_Jobs_1_HoursPerWeek","P_Jobs_2_HoursPerWeek","P_Jobs_3_HoursPerWeek","P_Jobs_4_HoursPerWeek","P_Jobs_5_HoursPerWeek","P_Jobs_6_HoursPerWeek","P_Jobs_7_HoursPerWeek","P_Jobs_8_HoursPerWeek","P_Jobs_9_HoursPerWeek",
                                       "P_Jobs_1_WageOrSelfEmployed","P_Jobs_2_WageOrSelfEmployed","P_Jobs_3_WageOrSelfEmployed","P_Jobs_4_WageOrSelfEmployed","P_Jobs_5_WageOrSelfEmployed","P_Jobs_6_WageOrSelfEmployed","P_Jobs_7_WageOrSelfEmployed","P_Jobs_8_WageOrSelfEmployed","P_Jobs_9_WageOrSelfEmployed",
                                       "P_Benefits_JSS_RecipientByPeriod",
                                       "P_Benefits_SLP_RecipientByPeriod",
                                       "P_Benefits_SPS_RecipientByPeriod",
                                       "P_Income_Raw_Jobs_All_AmountByPeriod",
                                       "P_Jobs_All_HoursWorkedByPeriod",
                                       "P_FamilyAssistance_ParentalLeave_Amount_RawSurvey")
