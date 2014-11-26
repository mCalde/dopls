doplsPerf <- function(y, mod){  
  ############################################################################
    
    # doplsPerf calculates the performance of the regression model.
    # It is not meant to be used alone, it should be called from dopls.
    #
    # Arguments: 
    #   mod: pls regression model
    #   y: experimental responce (real, measured data)
    # Value: 
    #   out: performance (Xexpvar, Yexpvar, RMSE, RMSECV, LV)
    #   responce: array with measured, calculated and CV calculated responce value
    #   vip: vip scores
    #   coefficients: model coefficients
    #   modello = model value, from the doplsReg function
    #
    # Source:
    #   source('/Users/marco/Documents/_DOCUMENTI/materiale/R/_repositoryR/sviluppo funzioni con R/dotools/dopls/doplsPerf.R')
    #
    # Examples: 
    #   out.perf <- doplsPerf(mod)
    #
    # MarcoCalderisi @KodeSolutions (c) 2014
    # Version 1.0
    #
    # TO DO:
    
  ############################################################################ 
  
  #----------------------------------------------------------------------  
  # VIP
  VIP <- function(object) {
    SS <- c(object$Yloadings)^2 * colSums(object$scores^2)
    Wnorm2 <- colSums(object$loading.weights^2)
    SSW <- sweep(object$loading.weights^2, 2, SS / Wnorm2, "*")
    sqrt(nrow(SSW) * apply(SSW, 1, cumsum) / cumsum(SS))
  }
  #----------------------------------------------------------------------
  
  ############################################
  ## model performance
  mod.l <- mod$model
  # explained variance
    Xexpvar <- sum(explvar(mod.l))  # per i predittori  (in %)
    Yexpvar <- R2(mod.l, estimate="train")$val[mod.l$ncomp + 1]*100 # (in %)
  # RMSE
    tmp <- c(RMSEP(mod.l, ncomp = mod.l$ncomp, estimate ="train", intercept = FALSE)$val)
    rmse <- tmp[length(tmp)]
  # RMSECV
    tmp <- c(RMSEP(mod.l, ncomp = mod.l$ncomp, estimate ="CV", intercept = FALSE)$val)
    rmsecv <- tmp[length(tmp)]
  perf <- data.frame(Xexpvar, Yexpvar, rmse, rmsecv, mod.l$ncomp)
  colnames(perf) <- c('Xexpvar', 'Yexpvar', 'RMSE', 'RMSECV', 'LV')
  
  ############################################
  ## variables performance
  # VIP
    tmp <- VIP(mod.l)
    ifelse(mod.l$ncomp == 1, vip <- tmp, vip <- tmp[mod.l$ncomp,])
  # coefficienti
    cf <- coef(mod.l, ncomp = mod.l$ncomp, intercept = F)
  
  ############################################
  ## objects performance
  ll <- nrow(mod$scale.attr) 
  # predicted (calibration)
  risp <- predict(mod.l, ncomp = mod.l$ncomp, type = "response")
  if(ll==2){
      rispor <- (risp * mod$scale.attr[2,1] + mod$scale.attr[1,1]) 
  }else{
      rispor <- risp + mod$scale.attr[1,1]  
  }
  # predicted (CV)
  rispCV <- mod.l$validation$pred[, , mod.l$ncomp]
  if(ll==2){
      risporCV <- (rispCV * mod$scale.attr[2,1] + mod$scale.attr[1,1])
  }else{
      risporCV <- rispCV + mod$scale.attr[1,1]
  }
  mispred <- cbind(y, rispor, risporCV)
  colnames(mispred) <- c('mis', 'pred', 'pred CV') 
  
  out.perf <- list(performance = perf, responce = mispred, vip = vip, coefficients = cf, modello = mod)
  return(out.perf)
}