
IndepEarnerTaxCredit <- function(Data, IETC_AbatementScale, IETC_MinimalIncome, IETC_PerYear, FamilyAssistance_PTC_Weeks, Periods = 1){   
    periodsOfPTC = as.integer(ceiling(FamilyAssistance_PTC_Weeks/52.2*24-1e-8))   
    Data[, temp_periods_PTC := 0]
    Data[(P_Attributes_Dependent==T) &
           (P_Attributes_Age==0) & 
           (P_Attributes_BirthPeriod<(25 - periodsOfPTC/2)), 
           temp_periods_PTC := periodsOfPTC]
    Data[(P_Attributes_Dependent==T) &
           (P_Attributes_Age==0) & 
           (P_Attributes_BirthPeriod>=(25 - periodsOfPTC/2)), 
           temp_periods_PTC := periodsOfPTC/2]
    Data[(P_Attributes_Dependent==T) &
           (P_Attributes_Age==1) & 
           (P_Attributes_BirthPeriod>=(25 - periodsOfPTC/2)), 
           temp_periods_PTC := periodsOfPTC/2]   
    Data[, temp_periods_PTC := max(temp_periods_PTC), by = F_ID]
    Data[, temp_periods_PTC := 2*ceiling(temp_periods_PTC/2), by = F_ID]
    Data[, temp_F_FamilyAssistance_PTC_Abated := sum(P_FamilyAssistance_PTC_Abated), 
         by=F_ID]
    Data[temp_F_FamilyAssistance_PTC_Abated == 0, temp_periods_PTC := 0]
    Data[, temp_bensupwff := P_Benefits_CoreBenefits_GrossAmount+
                       P_Super_Amount_Gross+
                       P_FamilyAssistance_Total]
    Data[, temp_bensupwff := 1L*(temp_bensupwff == 0)]                      
    Data[, temp_F_FamilyAssistance_Total := sum(P_FamilyAssistance_Total), by=F_ID]
    Data[(temp_periods_PTC>0) 
         & abs(temp_F_FamilyAssistance_Total-
               temp_F_FamilyAssistance_PTC_Abated)<1e-8, temp_bensupwff := 1L]
    Data[, P_TaxCredit_IETC := 0]
    Data[(P_Income_PrivateTaxCredit >= IETC_MinimalIncome-1e-8) &
         (temp_bensupwff==1L), 
         P_TaxCredit_IETC  := 
           Abate(T, IETC_PerYear, IETC_AbatementScale, 
                 Data[(P_Income_PrivateTaxCredit >= IETC_MinimalIncome-1e-8) &
                      (temp_bensupwff==1L),
                      P_Income_PrivateTaxCredit])*((24-temp_periods_PTC)/24)]                      
    if (Data[,sum((P_Attributes_PrincipalEarner+
                  P_Attributes_SpouseOfPrincipal+
                  P_Attributes_Dependent)!=1)]>0) {
        stop("errPersonType")
    }  
    Data[(P_Income_Raw_BenefitOverseas + 
          P_Income_Raw_PensionOverseas) > 0, P_TaxCredit_IETC := 0]
    Data[, `:=` (temp_bensupwff = NULL,
                 temp_periods_PTC = NULL,
                 temp_F_FamilyAssistance_Total = NULL,
                 temp_F_FamilyAssistance_PTC_Abated = NULL)]
}
attr(IndepEarnerTaxCredit, "output") <- 
    c("P_TaxCredit_IETC"
     )
attr(IndepEarnerTaxCredit, "input")  <- 
    c("P_Benefits_CoreBenefits_GrossAmount",
      "P_Super_Amount_Gross",
      "P_FamilyAssistance_Total",
      "P_Income_PrivateTaxCredit",
      "P_Income_Raw_BenefitOverseas",
      "P_Income_Raw_PensionOverseas",
      "P_Attributes_Dependent",
      "P_Attributes_PrincipalEarner",
      "P_Attributes_SpouseOfPrincipal",
      "F_ID",
      "P_Attributes_Age",
      "P_Attributes_BirthPeriod",
      "P_FamilyAssistance_PTC_Abated"
     )
