
CalculateIncomeVariables <- function(
    Data, 
    PIE_Percent_Investment, 
    PIE_Percent_Dividend, 
    Behaviour_Earnings_Add_WageSalary_Principal, 
    Behaviour_Earnings_Add_WageSalary_Spouse, 
    Periods = 1){
    AllJobs = seq(1,9)
    CurrentJobs = seq(1,3)
    PastJobs = seq(4,9)
    Data[, wsCol := 0]
    Data[, seCol := 0]
    Data[, sennCol := 0]
    Data[, redCol := 0]
    Data[, wagesCol := 0]
    for (job in AllJobs){
        Data[, c("StartPeriod",
                 "WageOrSelfEmployed",
                 "GrossAmountInflated") := .SD, 
               .SDcols = c(paste0("P_Jobs_Calculated_", job, "_StartPeriod"),
                           paste0("P_Jobs_Calculated_", job, "_WageOrSelfEmployed"),
                           paste0("P_Income_Calculated_Jobs_", job, "_GrossAmountInflated"))]
        if (nrow(Data[(StartPeriod > 0) & 
                      (WageOrSelfEmployed == "ws") & 
                      (GrossAmountInflated < 0),]) != 0) {
            stop("errWageSalaryAmountIsNegative")
        }        
        Data[(StartPeriod > 0) & (WageOrSelfEmployed == "ws"),                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
             wsCol := wsCol + GrossAmountInflated]
        Data[(StartPeriod > 0) & (WageOrSelfEmployed == "se"),
             seCol := seCol + GrossAmountInflated]
        Data[(StartPeriod > 0) & (WageOrSelfEmployed == "se") & 
             (GrossAmountInflated >= 0),
             sennCol := sennCol + GrossAmountInflated]      
        if (job %in% PastJobs) {
            Data[(StartPeriod > 0) & (WageOrSelfEmployed == "ws"),
                 redCol := redCol + .SD,
                 .SDcols = paste0("P_Income_Processed_Redundancy_Job", job)]
        }
    }
    Data[, c("StartPeriod",
             "WageOrSelfEmployed",
             "GrossAmountInflated") := NULL]        
    Data[, wagesCol := wsCol + redCol]
    Data[, pieCol := P_Income_Raw_PIEIncome + 
                     P_Income_Raw_InterestIncomeNZ * PIE_Percent_Investment +
                     P_Income_Raw_DividendNZ * PIE_Percent_Dividend +
                     P_Income_Raw_OtherInvestment * PIE_Percent_Investment]
    Data[, capitalIncCol := P_Income_Raw_RentNZ + 
                            P_Income_Raw_InterestIncomeNZ * 
                                (1-PIE_Percent_Investment) +
                            P_Income_Raw_DividendNZ * 
                                (1-PIE_Percent_Dividend) +
                            P_Income_Raw_OtherInvestment *
                                (1-PIE_Percent_Investment) +
                            P_Income_Raw_DividendOverseas +
                            P_Income_Raw_InterestOverseas +
                            P_Income_Raw_OtherInvestmentOverseas]
    Data[, otherTaxableCol := P_Income_Raw_Hobby +
                              P_Income_Raw_IncomeProtectionInsurance +
                              P_Income_Raw_Trust +
                              P_Income_Raw_OtherRegularNZ +
                              P_Income_Raw_ACCDependent +
                              P_Income_Calculated_ACCReceived +
                              P_Income_Raw_CasualIncome +
                              P_Income_Raw_TrustOverseas +
                              P_Income_Raw_BenefitOverseas +
                              P_Income_Raw_PensionOverseas +
                              P_FamilyAssistance_ParentalLeave_Amount_Calculated]
    Data[, nonTaxableCol := P_Maintenance_Amount_RawSurvey +
                        P_Maintenance_Overseas_RawSurvey +
                        P_Income_Raw_RegularJobSuper +
                        P_Income_Raw_RegularJobSuperOverseas +
                        P_Income_Processed_LifeInsuranceLumpSum + 
                        P_Income_Processed_OtherNonTaxableLumpSum +
                        P_Income_Raw_WarPensionOverseas +
                        P_Income_Raw_OtherStudentBursary +
                        P_Income_Processed_OtherIrregularOverseas +
                        P_Income_Processed_OtherIrregular]
    Data[, validPersonType := (P_Attributes_PrincipalEarner +
                               P_Attributes_SpouseOfPrincipal +
                               P_Attributes_Dependent)] 
    if (Data[, sum(1-validPersonType)] != 0) {
        stop("errPersonType")
    }
    Data[,
         `:=` (additionalWageSalary = 0)]   
    Data[P_Attributes_PrincipalEarner == 1,
         `:=` (additionalWageSalary =
                    Behaviour_Earnings_Add_WageSalary_Principal)]     
    Data[P_Attributes_SpouseOfPrincipal == 1,
         `:=` (additionalWageSalary =
                    Behaviour_Earnings_Add_WageSalary_Spouse)] 
    Data[, `:=` (wsCol = additionalWageSalary + wsCol,
                 wagesCol = additionalWageSalary + wagesCol)]
    Data[, `:=` (P_Income_WageSalary = wagesCol,
                 P_Income_SelfEmployed = seCol,
                 P_Income_CapitalNonPIEIncome = capitalIncCol,
                 P_Income_CapitalPIEIncome = pieCol,
                 P_Income_OtherTaxable = otherTaxableCol)]
    Data[, P_Income_PrivateTaxable := wagesCol + seCol + capitalIncCol +
                                      pieCol + otherTaxableCol]
    Data[, P_Income_PrivateNonTaxable := nonTaxableCol]
    Data[, P_Income_PrivateIncome := P_Income_PrivateTaxable + nonTaxableCol]
    Data[, P_Income_PrivateTaxCredit := wagesCol + sennCol + capitalIncCol + 
                                        pieCol + otherTaxableCol +
                                        P_Maintenance_Amount_RawSurvey -
                                        P_Maintenance_Paid_RawSurvey]  
    Data[, rebatein := P_Income_Raw_Hobby + 
                       P_Income_Raw_IncomeProtectionInsurance +
                       P_Income_Raw_ACCDependent +
                       P_Income_Calculated_ACCReceived +
                       P_Income_Raw_CasualIncome + 
                       P_Income_Raw_BenefitOverseas +
                       P_FamilyAssistance_ParentalLeave_Amount_Calculated]
    Data[, P_Income_PrivateRebate := wagesCol + seCol + rebatein]   
    Data[, chargein := P_Income_Raw_RegularJobSuper +
                       P_Income_Raw_RegularJobSuperOverseas +
                       P_Income_Raw_WarPensionOverseas +
                       P_Income_Raw_OtherStudentBursary]
    Data[, P_Income_PrivateChargeable := wsCol + sennCol + capitalIncCol +
                                         pieCol + otherTaxableCol + chargein]
    Data[, P_Income_LeviableForACC := sennCol]
    Data[wsCol > 0, P_Income_LeviableForACC := P_Income_LeviableForACC + wsCol]
    Data[, c("wsCol",
             "seCol",
             "sennCol",
             "redCol",
             "wagesCol",
             "pieCol",
             "capitalIncCol",
             "otherTaxableCol",
             "nonTaxableCol",
             "validPersonType",
             "additionalWageSalary",
             "rebatein",
             "chargein") := NULL]
}
attr(CalculateIncomeVariables, "output") <- 
    c("P_Income_PrivateIncome",
      "P_Income_PrivateTaxable",
      "P_Income_WageSalary",
      "P_Income_SelfEmployed",
      "P_Income_CapitalNonPIEIncome",
      "P_Income_CapitalPIEIncome",
      "P_Income_OtherTaxable",
      "P_Income_PrivateNonTaxable",
      "P_Income_PrivateRebate",
      "P_Income_PrivateChargeable",
      "P_Income_PrivateTaxCredit",
      "P_Income_LeviableForACC")
attr(CalculateIncomeVariables, "input")  <- 
    c("P_Income_Calculated_Jobs_1_GrossAmountInflated",
      "P_Income_Calculated_Jobs_2_GrossAmountInflated",
      "P_Income_Calculated_Jobs_3_GrossAmountInflated",
      "P_Income_Calculated_Jobs_4_GrossAmountInflated",
      "P_Income_Calculated_Jobs_5_GrossAmountInflated",
      "P_Income_Calculated_Jobs_6_GrossAmountInflated",
      "P_Income_Calculated_Jobs_7_GrossAmountInflated",
      "P_Income_Calculated_Jobs_8_GrossAmountInflated",
      "P_Income_Calculated_Jobs_9_GrossAmountInflated",
      "P_FamilyAssistance_ParentalLeave_Amount_Calculated",
      "P_Income_Processed_LifeInsuranceLumpSum",
      "P_Income_Processed_OtherNonTaxableLumpSum",
      "P_Income_Processed_OtherIrregularOverseas",
      "P_Income_Processed_OtherIrregular",
      "P_Income_Processed_Redundancy_Job4",
      "P_Income_Processed_Redundancy_Job5",
      "P_Income_Processed_Redundancy_Job6",
      "P_Income_Processed_Redundancy_Job7",
      "P_Income_Processed_Redundancy_Job8",
      "P_Income_Processed_Redundancy_Job9",
      "P_Jobs_Calculated_1_StartPeriod",
      "P_Jobs_Calculated_2_StartPeriod",
      "P_Jobs_Calculated_3_StartPeriod",
      "P_Jobs_Calculated_4_StartPeriod",
      "P_Jobs_Calculated_5_StartPeriod",
      "P_Jobs_Calculated_6_StartPeriod",
      "P_Jobs_Calculated_7_StartPeriod",
      "P_Jobs_Calculated_8_StartPeriod",
      "P_Jobs_Calculated_9_StartPeriod",
      "P_Jobs_Calculated_1_WageOrSelfEmployed",
      "P_Jobs_Calculated_2_WageOrSelfEmployed",
      "P_Jobs_Calculated_3_WageOrSelfEmployed",
      "P_Jobs_Calculated_4_WageOrSelfEmployed",
      "P_Jobs_Calculated_5_WageOrSelfEmployed",
      "P_Jobs_Calculated_6_WageOrSelfEmployed",
      "P_Jobs_Calculated_7_WageOrSelfEmployed",
      "P_Jobs_Calculated_8_WageOrSelfEmployed",
      "P_Jobs_Calculated_9_WageOrSelfEmployed",
      "P_Income_Calculated_ACCReceived",
      "P_Income_Raw_PIEIncome",
      "P_Income_Raw_DividendNZ",
      "P_Income_Raw_OtherInvestment",
      "P_Income_Raw_RentNZ",
      "P_Income_Raw_DividendOverseas",
      "P_Income_Raw_InterestOverseas",
      "P_Income_Raw_OtherInvestmentOverseas",
      "P_Income_Raw_Hobby",
      "P_Income_Raw_IncomeProtectionInsurance",
      "P_Income_Raw_Trust",
      "P_Income_Raw_OtherRegularNZ",
      "P_Income_Raw_ACCDependent",
      "P_Income_Raw_CasualIncome",
      "P_Income_Raw_TrustOverseas",
      "P_Income_Raw_BenefitOverseas",
      "P_Income_Raw_PensionOverseas",
      "P_Maintenance_Amount_RawSurvey",
      "P_Income_Raw_RegularJobSuper",
      "P_Income_Raw_RegularJobSuperOverseas",
      "P_Income_Raw_WarPensionOverseas",
      "P_Income_Raw_OtherStudentBursary",
      "P_Income_Raw_InterestIncomeNZ",
      "P_Maintenance_Paid_RawSurvey",
      "P_Maintenance_Overseas_RawSurvey",
      "P_Attributes_PrincipalEarner",
      "P_Attributes_SpouseOfPrincipal",
      "P_Attributes_Dependent"
    )
