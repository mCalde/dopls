doplsPred <- function(out.dopls, newData){
    ############################################################################
    
    # doplsPred predict new values using data from the dopls model.
    #
    # Arguments: 
    #   out.dopls: dopls regression model output
    #   newData: new data for prediction
    #
    # Value: 
    #   out: predicted values
    #
    # Source:
    #   source('/Users/marco/Documents/_DOCUMENTI/materiale/R/_repositoryR/sviluppo funzioni con R/dotools/dopls/doplsPred.R')
    #
    # Examples: 
    #   out.pred <- doplsPred(mod, newdata)
    #
    # MarcoCalderisi @KodeSolutions (c) 2014
    # Version 1.0
    #
    # TO DO: 
    
    ############################################################################
    # newData must be of class 'matrix' and must have the same number of variable 
    # of the predictors matrix (it means that the number of variables must be the 
    # same of the number of, i.e., coefficients.     
    
    # class check
    if(is.matrix(newData) == F){
        newData <- as.matrix(newData)
    }
    # dimension check 
    if(dim(newData)[2] != dim(out.dopls$coefficients)[1]){
        newData <- t(newData)
    }
    
    # prediction (two step procedure: first preprocess the predictors according 
    # to scaling values, because the initial model is calculated on scaled data,
    # then scale back the predicted values)
    mod.l <- out.dopls$modello$model
    ll <- dim(out.dopls$modello$scale.attr)[1]
    newData <- sweep(newData, MARGIN = 2, 
                     out.dopls$modello$scale.attr[1, 2:dim(out.dopls$modello$scale.attr)[2]], FUN = '-')
    if(ll==2){
      newData <- sweep(newData, MARGIN = 2, 
                     out.dopls$modello$scale.attr[2, 2:dim(out.dopls$modello$scale.attr)[2]], FUN = '/')
      }
    risp <- predict(mod.l, comps = 1:mod.l$ncomp, newdata = newData, type = "response")
      if(ll==2){
      rispor <- (risp * out.dopls$modello$scale.attr[2,1] 
                      + out.dopls$modello$scale.attr[1,1]) 
      }else{
      rispor <- risp + out.dopls$modello$scale.attr[1,1]  
      }
    return(rispor)    
}



