doplsReg <- function(x, y, ncomp, scale=c("mean", "autoscaling")){  
  ############################################################################
    
    # doplsReg calculates the regression model. If no scaling is stated, it uses a
    # mean centering scaling. If no number of latent variables is imputed it chooses 
    # the one corresponding to the lowest RMSECV. 
    # It is not meant to be used alone, it should be called from dopls.
    #
    # Arguments: 
    #   x: predictor matrix or data frame
    #   y: responce variable (vector, matrix or data frame)
    #   ncomp: number of latent variables desidered (it can be omitted)
    #   scale: mean centering (default), autoscaling
    #
    # Value: 
    #   out: pls regression model
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
    
  ## input data become a data.frame
  mod.xy <- data.frame(y,x)

  ## scaling
  scaling <- match.arg(scale, c("mean", "autoscaling"))
  mod.xy <- switch(scaling,
            mean = scale(mod.xy, center = T, scale = F),
            autoscaling = scale(mod.xy, center = T, scale = T))
  sa <- rbind(attr(mod.xy,"scaled:center"), attr(mod.xy,"scaled:scale"))
  mod.xy <- data.frame(mod.xy)
  
  ## number of components
  nobj <- dim(x)[1]
  npred <- dim(x)[2]
  # check the number of components if stated in input
  if (!missing(ncomp)) {
    if (ncomp > min(nobj - 2, npred))
      stop("The number of components is too high!")    
  }
  
  ## PLS regression
  if (missing(ncomp)) {
    # -2 because the number of components is calculated on the basis of a CV which remove one more dof.
    ncmp <- min(nobj - 2, npred) 
    mod <- mvr(y ~ ., data = mod.xy, ncomp = ncmp, method = "oscorespls", 
               scale = F, validation = "LOO")
    tmp <- c(RMSEP(mod, estimate = "CV")$val)
    tmp <- tmp [2:length(tmp)]
    nn <- which.min(tmp) # LV number
    # use the minimum number of LV, corresponding to the lowest value of RMSECV 
    mod <- mvr(y ~ ., data = mod.xy, ncomp = nn, method = "oscorespls", 
               scale = F, validation = "LOO")
  }else{
      mod <- mvr(y ~ ., data = mod.xy, ncomp=ncomp, method="oscorespls", 
                     scale=F, validation="LOO")
    }
  return(list(model = mod, obj = nrow(x), var = ncol(x), 
         scale.attr= sa))
}