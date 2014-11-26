doplsPlot <- function(x, y, perf, mod, graph = T){
  ############################################################################
    # doplsPlot makes plots and calculated coefficients, VIP and predicted values.
    # It is not meant to be used alone, it should be called from dopls.
    #
    # Arguments: 
    #   x: predictors 
    #   y: responce variable
    #   perf: outpur of doplsPerf
    #   mod: regression model (from doplsReg)
    #   graph: print plot or not
    #
    # Value: 
    #   graphs
    #
    # Source:
    #   source('/Users/marco/Documents/_DOCUMENTI/materiale/R/_repositoryR/sviluppo funzioni con R/dotools/dopls/doplsPlot.R')
    #
    # Examples: 
    #   out.plot <- doplsPlot(x, y, perf, mod, graph)
    #
    # MarcoCalderisi @KodeSolutions (c) 2014
    # Version 1.0
    #
    # TO DO: 
    #  1) mettere il comando per fare o meno i plot in cima ai plot e non solo per i plot 
    #     delle variabili risposta
  ############################################################################
  if (isTRUE(graph)){ # se non voglio i grafici proprio non faccio nulla
    
  # data preparation
  rispor <- perf$responce[,2]
  risporCV <- perf$responce[,3]
  vip <- perf$vip
  varib <- mod$var
  mod <- mod$model
  cf <- perf$coefficients
    
  
  # plots (model and objects)
  par(mfrow=c(2,2))
  # RMSE / RMSECV
  plot(mod, "validation", estimate = c("train", "CV"), ylab="RMSE", 
       main=paste("Number of LV =", mod$ncomp))
  legend("topright", legend=c("train", "CV"), col=1:2, lty=1:2, bty="n")
  # residui vs y
  plot(cbind(y, residuals(mod, ncomp = mod$ncomp)[, , mod$ncomp]), xlab="Resp variable", 
       ylab="Residuals")
  text(cbind(y, residuals(mod, ncomp = mod$ncomp)[, , mod$ncomp]), labels=1:nrow(x), 
         cex=0.6, pos=1)
  abline(h=0, col="grey", lty=2)
  # measured / predicted
  plot(cbind(y, rispor), xlab = 'Measured', ylab = 'Predicted', main = 'Training set')
  abline(lm(rispor ~ as.matrix(y)), col="red")
  abline(a=0, b=1, col="green")
  abline(h=0.5, col="gray", lty=2)
  # measured / predicted CV
  plot(cbind(y, risporCV), xlab = 'Measured', ylab = 'Predicted', main = 'Cross-validation set')
  abline(lm(risporCV ~ as.matrix(y)), col = "red")
  abline(a = 0, b = 1, col = "green")
  abline(h = 0.5, col = "gray", lty = 2)  

  # plots (variables)
  # see if there are variable names. In case they were missing automatic names are used.
  if(is.null(colnames(x))){
    colnames(x) <- paste("x", 1:varib, sep="")
  } else{
    colnames(x) <- colnames(x)
  }
  
  tipo <- ifelse(varib > 30, "l", "p")
  par(mfrow=c(2,1))

    # reg coeff
    plot(mod, plottype = "coefficients", main = "Regression coefficients", 
         type = tipo, xlab = "Var", ylab="")
#     if(ncol(x) < 30) {text(1:ncol(mod.xy), cf, label = colnames(mod.xy), 
#                                 cex = 0.7, pos = 3)}
    if(varib < 30) {text(1:varib, cf, label = colnames(x), 
                                cex = 0.7, pos = 3)}
    # vip
    plot(seq(1, ncol(x), 1), vip, type = tipo, lty = 1, main = "VIP scores", 
         xlab = "Var", ylab = "")
    if(varib < 30) {text(1:varib, vip, label = colnames(x), 
                                cex = 0.7, pos = 3)}
  }
}