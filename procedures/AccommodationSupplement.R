
AccommodationSupplement <- function(Data, Accommodation_ChildExpenseWeight, Accommodation_NZSIncomeLimit_Single, 
                                    Accommodation_NZSIncomeLimit_SoleParent1Dep, Accommodation_NZSIncomeLimit_SoleParent2_Deps, 
                                    Accommodation_NZSIncomeLimit_Couple, Accommodation_MaxRate_CoupleDeps_Single2_Deps_Rent_Area1, 
                                    Accommodation_MaxRate_CoupleDeps_Single2_Deps_Rent_Area2, Accommodation_MaxRate_CoupleDeps_Single2_Deps_Rent_Area3, 
                                    Accommodation_MaxRate_CoupleDeps_Single2_Deps_Rent_Area4, Accommodation_MaxRate_CoupleNoDeps_Single1Dep_Rent_Area1, 
                                    Accommodation_MaxRate_CoupleNoDeps_Single1Dep_Rent_Area2, Accommodation_MaxRate_CoupleNoDeps_Single1Dep_Rent_Area3, 
                                    Accommodation_MaxRate_CoupleNoDeps_Single1Dep_Rent_Area4, Accommodation_MaxRate_SingleNoDeps_Rent_Area1, 
                                    Accommodation_MaxRate_SingleNoDeps_Rent_Area2, Accommodation_MaxRate_SingleNoDeps_Rent_Area3, 
                                    Accommodation_MaxRate_SingleNoDeps_Rent_Area4, Accommodation_MaxRate_CoupleDeps_Single2_Deps_Mortgage_Area1, 
                                    Accommodation_MaxRate_CoupleDeps_Single2_Deps_Mortgage_Area2, 
                                    Accommodation_MaxRate_CoupleDeps_Single2_Deps_Mortgage_Area3, 
                                    Accommodation_MaxRate_CoupleDeps_Single2_Deps_Mortgage_Area4, 
                                    Accommodation_MaxRate_CoupleNoDeps_Single1Dep_Mortgage_Area1, 
                                    Accommodation_MaxRate_CoupleNoDeps_Single1Dep_Mortgage_Area2, 
                                    Accommodation_MaxRate_CoupleNoDeps_Single1Dep_Mortgage_Area3, 
                                    Accommodation_MaxRate_CoupleNoDeps_Single1Dep_Mortgage_Area4, 
                                    Accommodation_MaxRate_SingleNoDeps_Mortgage_Area1, Accommodation_MaxRate_SingleNoDeps_Mortgage_Area2, 
                                    Accommodation_MaxRate_SingleNoDeps_Mortgage_Area3, Accommodation_MaxRate_SingleNoDeps_Mortgage_Area4, 
                                    Accommodation_BaseRateThreshold_Rent, Accommodation_BaseRateThreshold_Mortgage, Accommodation_PaymentPercentage, 
                                    Accommodation_AbatementRate, Super_Rates_Single, Super_Rates_SingleSharing, Super_Rates_MarriedPerson, 
                                    Super_Rates_NQSCouple, FamilyAssistance_FTC_Rates_FirstChild0to15, Benefits_BenefitSystem, 
                                    Benefits_JSS_Rate_Single, Benefits_JSS_Rate_Couple, Benefits_JSS_Rate_SingleAtHome, Benefits_JSS_Rate_SingleYoung, 
                                    Benefits_JSS_Rate_SoleParent, Benefits_JSS_Rate_WomanAlone, Benefits_SLP_Rate_Single, Benefits_SLP_Rate_Couple, 
                                    Benefits_SLP_Rate_SingleYoung, Benefits_SLP_Rate_SoleParent, Benefits_SPS_Rate, Benefits_JSS_AbatementScale, 
                                    Benefits_SPS_AbatementScale, Benefits_JSS_Rate_CoupleParent, Benefits_SLP_Rate_CoupleParent, UseAccomSup, 
                                    Tax_BaseScale, Periods = 1, modelyear = 2015){
  Data[, Mortgage := H_Mortgage_InterestPaid + H_Mortgage_PrinciplePaid]
  Data[, Rent := H_Renting_AnnualRent]
  Data[, Expense_Type := 1*(Mortgage == 0 & Rent > 0 & H_Renting_LandlordType != 4) + 
         2*(Mortgage > 0 & Rent == 0 & H_Renting_LandlordType != 4) + 
         (Mortgage > 0 & Rent > 0 & H_Renting_TenureType %in% c(11,31) & 
            H_Renting_LandlordType == 8 & H_Renting_LandlordType != 4)*2 +
         (Mortgage > 0 & Rent > 0 & H_Renting_TenureType == 21 & 
            H_Renting_LandlordType %in% c(1:6) & H_Renting_LandlordType != 4)*1]
  Data[, Weekly_Housing := (Expense_Type == 1)*Rent/52.2*Periods + 
         (Expense_Type == 2)*Mortgage/52.2*Periods]
  Data[, HH_Exp_Weight := H_Counts_Adults + Accommodation_ChildExpenseWeight*H_Counts_DependentKids]
  Data[, Fam_Exp_Weight := F_Counts_Adults + Accommodation_ChildExpenseWeight*F_Counts_Dependents]
  Data[, Fam_Weekly_Housing := Weekly_Housing*Fam_Exp_Weight/HH_Exp_Weight]
  Data[, Benefit_Type := 1*P_Benefits_JSS_CalculatedRecipientByPeriod +
         2*P_Benefits_SPS_CalculatedRecipientByPeriod +
         3*P_Benefits_SLP_CalculatedRecipientByPeriod]
  Data[, Student := (!P_Attributes_Dependent) & (P_Attributes_FullOrPartTimeEducation == 1) & 
         (Benefit_Type == 0)]
  Data[, Benefit := 1*(Benefit_Type > 0)]
  Data[, Student_Fam := max(Student), by = F_ID]
  Data[, Benefit_Family := max(Benefit), by = F_ID]
  Data[, NZS_Ind := F_Attributes_OnSuper > 0]
  Data[, NZS_Income_Limit := 0]
  Data[NZS_Ind & F_Attributes_IsCouple, NZS_Income_Limit := 52.2*Accommodation_NZSIncomeLimit_Couple/Periods]
  Data[NZS_Ind & !F_Attributes_IsCouple & F_Counts_Dependents >= 2,
       NZS_Income_Limit := 52.2*Accommodation_NZSIncomeLimit_SoleParent2_Deps/Periods]
  Data[NZS_Ind & !F_Attributes_IsCouple & F_Counts_Dependents == 1,
       NZS_Income_Limit := 52.2*Accommodation_NZSIncomeLimit_SoleParent1Dep/Periods]
  Data[NZS_Ind & !F_Attributes_IsCouple & F_Counts_Dependents == 0,
       NZS_Income_Limit := 52.2*Accommodation_NZSIncomeLimit_Single/Periods]
  Data[, Assessable_Income := P_Income_PrivateChargeable*(!P_Attributes_Dependent)]
  Data[, Assessable_Income := sum(Assessable_Income), by = F_ID]
  Data[, Over_NZS_Limit := NZS_Ind & (Assessable_Income > NZS_Income_Limit)]
  Data[, NQS_Potential := P_Attributes_Dependent | (P_Super_Amount_Gross > 0 & F_Attributes_OnSuper == 1 & F_Attributes_IsCouple)]
  Data[, NQS := min(NQS_Potential), by = F_ID]
  Data[, Base_Rate := NQS*Net_From_Gross(52.2*Super_Rates_NQSCouple/2, Tax_BaseScale)/52.2]
  Data[(NQS == 0) & Benefit_Type == 2, Base_Rate := Benefits_SPS_Rate]
  Data[(NQS == 0) & Benefit_Type < 2, Base_Rate := Benefits_JSS_Rate_Couple]
  Data[(NQS == 0) & (Benefit_Type < 2) & (F_Counts_Dependents > 0) & (modelyear >= 2017), Base_Rate := Benefits_JSS_Rate_CoupleParent]
  Data[(NQS == 0) & Benefit_Type == 3, Base_Rate := Benefits_SLP_Rate_Couple]
  Data[(NQS == 0) & (Benefit_Type == 3) & (F_Counts_Dependents > 0) & (modelyear >= 2017), Base_Rate := Benefits_SLP_Rate_CoupleParent]
  Data[(NQS == 0) & P_Super_OnSuper == 2, Base_Rate := Net_From_Gross(52.2*Super_Rates_MarriedPerson, Tax_BaseScale)/52.2]
  Data[, Single := !F_Attributes_IsCouple]
  Data[Single & Benefit_Type < 2 & F_Counts_Dependents >= 1, 
       Base_Rate := Benefits_JSS_Rate_SoleParent]
  Data[Single & Benefit_Type < 2 & F_Counts_Dependents == 0, 
       Base_Rate := Benefits_JSS_Rate_Single]
  Data[Single & Benefit_Type == 2, Base_Rate := Benefits_SPS_Rate]
  Data[Single & Benefit_Type == 3 & F_Counts_Dependents >= 1, 
       Base_Rate := Benefits_SLP_Rate_SoleParent]
  Data[Single & Benefit_Type == 3 & F_Counts_Dependents == 0, 
       Base_Rate := Benefits_SLP_Rate_Single]
  Data[Single & P_Super_OnSuper == 2, 
       Base_Rate := Net_From_Gross(52.2*Super_Rates_Single, Tax_BaseScale)/52.2]
  Data[Single & P_Super_OnSuper == 1, 
       Base_Rate := Net_From_Gross(52.2*Super_Rates_SingleSharing, Tax_BaseScale)/52.2]
  Data[, Base_Rate := 
         52.2/Periods*(Base_Rate + (F_Counts_Dependents > 0)*FamilyAssistance_FTC_Rates_FirstChild0to15/((1+F_Attributes_IsCouple)*52))]
  Data[, Base_Rate := Base_Rate*(!P_Attributes_Dependent)]
  Data[, F_Benefits_Accommodation_BaseRate := sum(Base_Rate), by = F_ID]
  Data[, F_Benefits_Accommodation_BaseRate := 
         F_Benefits_Accommodation_BaseRate*(Expense_Type > 0)]
  Data[, F_Benefits_Accommodation_BaseRate := 
         F_Benefits_Accommodation_BaseRate*(Student_Fam == 0)]
  Data[, F_Benefits_Accommodation_BaseRate := 
         F_Benefits_Accommodation_BaseRate*(!Over_NZS_Limit)]
  Data[, Max_Rate := 0]
  Data[Expense_Type == 1, Max_Rate := 
         (H_Attributes_ASArea == 1) * Accommodation_MaxRate_SingleNoDeps_Rent_Area1 +
         (H_Attributes_ASArea == 2) * Accommodation_MaxRate_SingleNoDeps_Rent_Area2 +
         (H_Attributes_ASArea == 3) * Accommodation_MaxRate_SingleNoDeps_Rent_Area3 +
         (H_Attributes_ASArea == 4) * Accommodation_MaxRate_SingleNoDeps_Rent_Area4]
  Data[Expense_Type == 2, Max_Rate := 
         (H_Attributes_ASArea == 1) * Accommodation_MaxRate_SingleNoDeps_Mortgage_Area1 +
         (H_Attributes_ASArea == 2) * Accommodation_MaxRate_SingleNoDeps_Mortgage_Area2 +
         (H_Attributes_ASArea == 3) * Accommodation_MaxRate_SingleNoDeps_Mortgage_Area3 +
         (H_Attributes_ASArea == 4) * Accommodation_MaxRate_SingleNoDeps_Mortgage_Area4]
  Data[, AS_Type_2 := (F_Attributes_IsCouple & F_Counts_Dependents == 0) | 
         ((!F_Attributes_IsCouple) & F_Counts_Dependents == 1)]
  Data[AS_Type_2 & Expense_Type == 1, Max_Rate := 
         (H_Attributes_ASArea == 1) * Accommodation_MaxRate_CoupleNoDeps_Single1Dep_Rent_Area1 +
         (H_Attributes_ASArea == 2) * Accommodation_MaxRate_CoupleNoDeps_Single1Dep_Rent_Area2 +
         (H_Attributes_ASArea == 3) * Accommodation_MaxRate_CoupleNoDeps_Single1Dep_Rent_Area3 +
         (H_Attributes_ASArea == 4) * Accommodation_MaxRate_CoupleNoDeps_Single1Dep_Rent_Area4]
  Data[AS_Type_2 & Expense_Type == 2, Max_Rate := 
         (H_Attributes_ASArea == 1) * Accommodation_MaxRate_CoupleNoDeps_Single1Dep_Mortgage_Area1 +
         (H_Attributes_ASArea == 2) * Accommodation_MaxRate_CoupleNoDeps_Single1Dep_Mortgage_Area2 +
         (H_Attributes_ASArea == 3) * Accommodation_MaxRate_CoupleNoDeps_Single1Dep_Mortgage_Area3 +
         (H_Attributes_ASArea == 4) * Accommodation_MaxRate_CoupleNoDeps_Single1Dep_Mortgage_Area4]
  Data[, AS_Type_3 := (F_Attributes_IsCouple & F_Counts_Dependents >= 1) | 
         ((!F_Attributes_IsCouple) & F_Counts_Dependents >= 2)]
  Data[AS_Type_3 & Expense_Type == 1, Max_Rate := 
         (H_Attributes_ASArea == 1) * Accommodation_MaxRate_CoupleDeps_Single2_Deps_Rent_Area1 +
         (H_Attributes_ASArea == 2) * Accommodation_MaxRate_CoupleDeps_Single2_Deps_Rent_Area2 +
         (H_Attributes_ASArea == 3) * Accommodation_MaxRate_CoupleDeps_Single2_Deps_Rent_Area3 +
         (H_Attributes_ASArea == 4) * Accommodation_MaxRate_CoupleDeps_Single2_Deps_Rent_Area4]
  Data[AS_Type_3 & Expense_Type == 2, Max_Rate := 
         (H_Attributes_ASArea == 1) * Accommodation_MaxRate_CoupleDeps_Single2_Deps_Mortgage_Area1 +
         (H_Attributes_ASArea == 2) * Accommodation_MaxRate_CoupleDeps_Single2_Deps_Mortgage_Area2 +
         (H_Attributes_ASArea == 3) * Accommodation_MaxRate_CoupleDeps_Single2_Deps_Mortgage_Area3 +
         (H_Attributes_ASArea == 4) * Accommodation_MaxRate_CoupleDeps_Single2_Deps_Mortgage_Area4]
  Data[, Base_Rate_Threshold := Accommodation_BaseRateThreshold_Rent*(Expense_Type == 1) +
         Accommodation_BaseRateThreshold_Mortgage*(Expense_Type == 2)]
  Data[, Annual_Housing := 52.2*Fam_Weekly_Housing/Periods]
  Data[, Max_Rate := 52.2*Max_Rate/Periods]
  Data[, Base_Rate_Threshold := Base_Rate_Threshold]
  Data[, AS_Entitlement := pmax(0, (Annual_Housing - F_Benefits_Accommodation_BaseRate * Base_Rate_Threshold) * Accommodation_PaymentPercentage)]
  Data[, F_Benefits_Accommodation_Unabated := pmin(Max_Rate, AS_Entitlement)]
  Data[, F_Benefits_Accommodation_Unabated := 
         F_Benefits_Accommodation_Unabated*(Expense_Type > 0)]
  Data[, F_Benefits_Accommodation_Unabated := 
         F_Benefits_Accommodation_Unabated*(Student_Fam == 0)]
  Data[, F_Benefits_Accommodation_Unabated := 
         F_Benefits_Accommodation_Unabated*(!Over_NZS_Limit)]
  Data[, No_Abate := Benefit_Family | (F_Attributes_OnSuper > 0)]
  Couple_Parent_Vanish <- Abatement_Vanishing_Point(Benefits_JSS_AbatementScale, 2*52.2*Benefits_JSS_Rate_CoupleParent)
  Couple_Vanish <- Abatement_Vanishing_Point(Benefits_JSS_AbatementScale, 2*52.2*Benefits_JSS_Rate_Couple)
  Single_Vanish <- Abatement_Vanishing_Point(Benefits_JSS_AbatementScale, 52.2*Benefits_JSS_Rate_Single)
  Sole_Parent_Vanish <- Abatement_Vanishing_Point(Benefits_SPS_AbatementScale, 52.2*Benefits_JSS_Rate_SoleParent)
  Data[, AS_Abate_Point := (!No_Abate)*(((F_Attributes_IsCouple) & F_Counts_Dependents == 0)*Couple_Vanish +
                                          ((F_Attributes_IsCouple) & F_Counts_Dependents >= 1)*(Couple_Vanish*(modelyear <= 2016) + Couple_Parent_Vanish*(modelyear >= 2017)) +
                                          ((!F_Attributes_IsCouple) & F_Counts_Dependents >= 1)*Sole_Parent_Vanish +
                                          ((!F_Attributes_IsCouple) & F_Counts_Dependents == 0)*Single_Vanish)]
  Data[, AS_Abate_Point := ceiling(AS_Abate_Point)]
  Data[, F_Benefits_Accommodation_Abated := pmax(0, Periods*F_Benefits_Accommodation_Unabated -
                                                   pmax(0, Periods*Assessable_Income - AS_Abate_Point)*Accommodation_AbatementRate)/Periods]
  Data[No_Abate & T, F_Benefits_Accommodation_Abated := F_Benefits_Accommodation_Unabated]
  Data[, P_Benefits_Accommodation_Abated := ((F_Attributes_IsCouple) & (!P_Attributes_Dependent))*(F_Benefits_Accommodation_Abated/2) +
         ((!F_Attributes_IsCouple) & (!P_Attributes_Dependent))*F_Benefits_Accommodation_Abated]
  Extra <- c("Mortgage", "Rent", "Expense_Type", "Weekly_Housing", "HH_Exp_Weight", 
             "Fam_Exp_Weight", "Fam_Weekly_Housing", "Benefit_Type", "Student", 
             "Benefit", "Student_Fam", "Benefit_Family", "NZS_Ind", "NZS_Income_Limit", 
             "Assessable_Income", "Over_NZS_Limit", "NQS_Potential", "NQS", 
             "Base_Rate", "Single", "Max_Rate", "AS_Type_2", "AS_Type_3", "Base_Rate_Threshold", 
             "Annual_Housing", "AS_Entitlement", "No_Abate", "AS_Abate_Point")
  Data[, (Extra) := NULL]
  if (UseAccomSup == 0){
    Data[, F_Benefits_Accommodation_BaseRate := 0] 
    Data[, F_Benefits_Accommodation_Unabated := 0]
    Data[, F_Benefits_Accommodation_Abated := 0]
    Data[, P_Benefits_Accommodation_Abated := 0]
  }
  if (UseAccomSup == 1){
    Data[, F_Benefits_Accommodation_BaseRate := 0] 
    Data[, F_Benefits_Accommodation_Unabated := 0]
    Data[, P_Benefits_Accommodation_Abated := P_Benefits_Accommodation_Amount_RawSurvey]
    Data[, F_Benefits_Accommodation_Abated := sum(P_Benefits_Accommodation_Amount_RawSurvey), by = F_ID]
  }
}
attr(AccommodationSupplement, "output") <- c("F_Benefits_Accommodation_BaseRate","F_Benefits_Accommodation_Unabated",
                                             "F_Benefits_Accommodation_Abated","P_Benefits_Accommodation_Abated")
attr(AccommodationSupplement, "input")  <- c("F_Attributes_IsCouple","F_Attributes_OnSuper",
                                             "P_Income_PrivateChargeable","P_Super_OnSuper",
                                             "P_Benefits_JSS_CalculatedRecipientByPeriod","P_Benefits_SLP_CalculatedRecipientByPeriod",
                                             "P_Benefits_SPS_CalculatedRecipientByPeriod","P_Super_Amount_Gross","H_Renting_TenureType",
                                             "H_Renting_LandlordType","H_Renting_AnnualRent","H_Mortgage_InterestPaid","H_Mortgage_PrinciplePaid",
                                             "H_Counts_Adults","H_Counts_DependentKids","F_Counts_Adults","F_Counts_Dependents",
                                             "H_Attributes_ASArea",
                                             "P_Benefits_Accommodation_Amount_RawSurvey","P_Attributes_FullOrPartTimeEducation",
                                             "P_Attributes_Dependent","F_ID")
