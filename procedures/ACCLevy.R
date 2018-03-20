
ACCLevy <- function(Data, ACC_LevyRate, ACC_MaxLeviableIncome, Periods = 1){
   Data[, P_ACC_LevyPayable := pmin(P_Income_LeviableForACC,ACC_MaxLeviableIncome)] 
   Data[, P_ACC_LevyPayable := ACC_LevyRate * P_ACC_LevyPayable]
}
attr(ACCLevy, "output") <- 
    c("P_ACC_LevyPayable"
     )
attr(ACCLevy, "input")  <- 
    c("P_Income_LeviableForACC"
     )
