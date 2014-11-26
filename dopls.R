dopls <- function(x, y, ncomp, scale=c("mean", "autoscaling"), graph = T){
  ############################################################################
    # dopls is the wrapper function of: doplsReg, doplsPerf, doplsPlot. It calculates
    # the regression model and all the statistic relted to it. If no scaling is stated, 
    # it uses a mean centering scaling. If no number of latent variables is imputed  
    # it chooses the one corresponding to the lowest RMSECV. 
    #
    # Arguments: 
    #   x: predictor matrix or data frame
    #   y: responce variable (vector, matrix or data frame)
    #   ncomp: number of latent variables desidered (it can be omitted)
    #   scale: mean centering (default), autoscaling
    #   graph: print plot or not
    #
    # Value: 
    #   modello: pls regression model 
    #   performance: performance (Xexpvar, Yexpvar, RMSE, RMSECV, LV)
    #   responce: array with measured, calculated and CV calculated responce value
    #   vip: vip scores
    #   coefficients: model coefficients
    #   modello: model value, from the doplsReg function
    #      obj: number of samples
    #      var: number of variables
    #      scale.attr: scaling attributes
    #
    # Source:
    #   source('/Users/marco/Documents/_DOCUMENTI/materiale/R/_repositoryR/sviluppo funzioni con R/dotools/dopls/doplsReg.R')
    #
    # Examples: 
    #   mod <- doplsReg(x, y)
    #
    # MarcoCalderisi @KodeSolutions (c) 2014
    # Version 1.0
    #
    # TO DO:
  ############################################################################
# functions needed  
  source('doplsReg.R')
  source('doplsPerf.R')
  source('doplsPlot.R')

# list of steps
  mod <- doplsReg(x, y, ncomp, scale)
  perf <- doplsPerf(y, mod)
  doplsPlot(x, y, perf, mod, graph)
  return(perf)
}