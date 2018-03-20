
WinterEnergy <- function(Data, 
                         Benefits_WinterEnergy_Rates_Single, 
                         Benefits_WinterEnergy_Rates_CoupleOrDeps, 
                         Periods = 1){
  Data[, P_Benefits_WinterEnergy := 0]
  Data[, WinterEnergy_Eligible := 1*(sum(P_Super_Amount_Gross+
                                 P_Benefits_JSS_Amount_Abated+
                                 P_Benefits_SLP_Amount_Abated+
                                 P_Benefits_SPS_Amount_Abated)>0), by=F_ID]
  Data[(WinterEnergy_Eligible==1) & (F_Counts_Adults==1) & (F_Counts_Dependents==0) & (P_Attributes_PrincipalEarner), P_Benefits_WinterEnergy := Benefits_WinterEnergy_Rates_Single]
  Data[(WinterEnergy_Eligible==1) & (F_Counts_Adults==1) & (F_Counts_Dependents>0) & (P_Attributes_PrincipalEarner), P_Benefits_WinterEnergy := Benefits_WinterEnergy_Rates_CoupleOrDeps] 
  Data[(WinterEnergy_Eligible==1) & (F_Counts_Adults==2) & (P_Attributes_Dependent == 0), P_Benefits_WinterEnergy := Benefits_WinterEnergy_Rates_CoupleOrDeps / F_Counts_Adults]
  Data[, WinterEnergy_Eligible := NULL]
}
attr(WinterEnergy, "output") <- 
  c("P_Benefits_WinterEnergy") 
attr(WinterEnergy, "input")  <- 
  c("F_ID", 
    "P_Super_Amount_Gross",
    "P_Benefits_JSS_Amount_Abated",
    "P_Benefits_SLP_Amount_Abated",
    "P_Benefits_SPS_Amount_Abated",
    "F_Counts_Adults",
    "F_Counts_Dependents",
    "P_Attributes_PrincipalEarner",
    "P_Attributes_Dependent")
