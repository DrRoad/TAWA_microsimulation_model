
FamilyAssistanceFullTimeWaged <- function(Data, FamilyAssistance_FullTimeWorkingHours_Couple, FamilyAssistance_FullTimeWorkingHours_Single, Periods = 1){
   AllJobs = seq(1,9)
   Data[, temp_jobHours := 0]
   Data[, temp_wsjobHours := 0]
   for (job in AllJobs){
        Data[, temp_is_wsjob := 1L*(.SD=='ws'),
               .SDcols = paste0('P_Jobs_Calculated_', job, '_WageOrSelfEmployed')]
        Data[, temp_job := 1*.SD,
               .SDcols = paste0('P_Jobs_Calculated_', job, '_HoursPerWeek')]
        Data[, temp_wsjob := temp_is_wsjob * temp_job]
        Data[, temp_jobHours := temp_jobHours + temp_job]
        Data[, temp_wsjobHours := temp_wsjobHours + temp_wsjob]
   }
   Data[P_Attributes_Dependent == 1, `:=` (temp_jobHours = 0,
                                           temp_wsjobHours = 0)]
   Data[, `:=` (temp_jobHours = sum(temp_jobHours),
                temp_wsjobHours = sum(temp_wsjobHours)), by=F_ID]
   Data[, temp_minFullTimeWork := FamilyAssistance_FullTimeWorkingHours_Single]
   Data[F_Attributes_IsCouple==1, 
        temp_minFullTimeWork := FamilyAssistance_FullTimeWorkingHours_Couple]
   Data[, F_FamilyAssistance_WorkingVariables_FullTimeByPeriod := 
          1L*(temp_jobHours >= temp_minFullTimeWork)]
   Data[, F_FamilyAssistance_WorkingVariables_FullTimeWagedByPeriod := 
          1L*(temp_wsjobHours >= temp_minFullTimeWork)]
   Data[, `:=`(temp_is_wsjob = NULL,
               temp_job = NULL,
               temp_wsjob = NULL,
               temp_jobHours = NULL,
               temp_wsjobHours = NULL,
               temp_minFullTimeWork = NULL)]
}
attr(FamilyAssistanceFullTimeWaged, "output") <- 
    c("F_FamilyAssistance_WorkingVariables_FullTimeWagedByPeriod",   
      "F_FamilyAssistance_WorkingVariables_FullTimeByPeriod"
     )
attr(FamilyAssistanceFullTimeWaged, "input")  <- 
    c("F_Attributes_IsCouple",
      "P_Jobs_Calculated_1_WageOrSelfEmployed",
      "P_Jobs_Calculated_2_WageOrSelfEmployed",
      "P_Jobs_Calculated_3_WageOrSelfEmployed",
      "P_Jobs_Calculated_4_WageOrSelfEmployed",
      "P_Jobs_Calculated_5_WageOrSelfEmployed",
      "P_Jobs_Calculated_6_WageOrSelfEmployed",
      "P_Jobs_Calculated_7_WageOrSelfEmployed",
      "P_Jobs_Calculated_8_WageOrSelfEmployed",
      "P_Jobs_Calculated_9_WageOrSelfEmployed",
      "P_Jobs_Calculated_1_HoursPerWeek",
      "P_Jobs_Calculated_2_HoursPerWeek",
      "P_Jobs_Calculated_3_HoursPerWeek",
      "P_Jobs_Calculated_4_HoursPerWeek",
      "P_Jobs_Calculated_5_HoursPerWeek",
      "P_Jobs_Calculated_6_HoursPerWeek",
      "P_Jobs_Calculated_7_HoursPerWeek",
      "P_Jobs_Calculated_8_HoursPerWeek",
      "P_Jobs_Calculated_9_HoursPerWeek",
      "P_Attributes_Dependent",
      "F_ID"
     )
