
Superannuation <- function(Data, 
                           Super_AbatementScale, 
                           Super_Rates_MarriedPerson, 
                           Super_Rates_Single, 
                           Super_Rates_SingleSharing, 
                           Super_Rates_NQSCouple, 
                           Tax_BaseScale, 
                           Periods = 1){
  weeks_in_year = 52.2
  weeks_on_super = 52.2/Periods
  Data[,
       P_Super_Amount_Gross := 0]
  Data[F_Attributes_OnSuper == 1 & P_Attributes_Dependent == 0 & F_Attributes_IsCouple == 1,
       `:=` (TemporaryVariable_Assessable_income = sum(P_Income_PrivateChargeable), 
             TemporaryVariable_AnyOnBenefit = 
               sum(P_Benefits_CoreBenefits_CalculatedRecipientByPeriod)>0),
       by = .(F_ID, Period)]
  Data[F_Attributes_OnSuper == 1 & P_Attributes_Dependent == 0 & F_Attributes_IsCouple == 1,
       TemporaryVariable_NQSAmount := Abate(T, 
                                            Super_Rates_NQSCouple*weeks_in_year ,
                                            Super_AbatementScale,
                                            TemporaryVariable_Assessable_income*Periods)/weeks_in_year ]
  Data[F_Attributes_OnSuper == 1 & P_Attributes_Dependent == 0 & F_Attributes_IsCouple == 1 & 
         !(TemporaryVariable_AnyOnBenefit) & 
         TemporaryVariable_NQSAmount > Super_Rates_MarriedPerson,
       P_Super_Amount_Gross := TemporaryVariable_NQSAmount*weeks_on_super/2]
  Data[F_Attributes_OnSuper == 1 & 
         P_Attributes_Dependent == 0  & F_Attributes_IsCouple == 1 & 
         (TemporaryVariable_AnyOnBenefit |
            TemporaryVariable_NQSAmount <= Super_Rates_MarriedPerson),
       P_Super_Amount_Gross := 
         Super_Rates_MarriedPerson*weeks_on_super*P_Super_OnSuper/2]
  Data[F_Attributes_OnSuper == 2 & P_Super_OnSuper == 2 & F_Attributes_IsCouple == 1,
       P_Super_Amount_Gross := Super_Rates_MarriedPerson*weeks_on_super]
  Data[P_Super_OnSuper == 2 & F_Attributes_IsCouple == 0,
       P_Super_Amount_Gross := Super_Rates_Single*weeks_on_super]
  Data[P_Super_OnSuper == 1 & F_Attributes_IsCouple == 0,
       P_Super_Amount_Gross := Super_Rates_SingleSharing*weeks_on_super]
  Data[,
       P_Super_Amount_Tax := Apply(P_Super_Amount_Gross*Periods, Tax_BaseScale)/Periods]
  Data[, `:=` (TemporaryVariable_Assessable_income = NULL,
               TemporaryVariable_AnyOnBenefit = NULL,
               TemporaryVariable_NQSAmount = NULL)]
}
attr(Superannuation, "output") <- c("P_Super_Amount_Gross","P_Super_Amount_Tax")
attr(Superannuation, "input")  <- c("F_ID", 
                                    "Period",
                                    "P_Income_PrivateChargeable",
                                    "P_Benefits_CoreBenefits_CalculatedRecipientByPeriod",
                                    "F_Attributes_IsCouple",
                                    "F_Attributes_OnSuper",
                                    "P_Super_OnSuper",
                                    "P_Attributes_Dependent")
