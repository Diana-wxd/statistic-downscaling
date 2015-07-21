# summary function
# this summary function should be used to summary performance if we want to use "PSS" as a metric
# to select the final model
score <- function (data, lev = levels(data$obs), model = NULL) {
  if (length(lev)==2){
    out <- c(twoClassSummary(data, lev = levels(data$obs), model = NULL)) 
    pss <- out["Sens"]-(1-out["Spec"])
    c(out, PSS = pss)
  }
  else{
    isNA <- is.na(data$pred)
    pred <- data$pred[!isNA]
    obs <- data$obs[!isNA]
    matrix <- as.matrix(table(pred,obs))
    library(verification)
    out <- multi.cont(matrix)
    PSS.Sens <- out$pss
    pc <- out$pc
    hss <- out$hss
    c(PSS.Sens=PSS.Sens,PC=pc,HSS=hss) 
  }
}
