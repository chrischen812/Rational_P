# version 0.01
# Copyright CHOC Children's
# For bugs/corrections, please send an email to: lehwerhemuepha@choc.org
# 3/5/2016

makeFormula <- function(stringVector, hasIntercept=TRUE){
  base <- ifelse (hasIntercept, ".~.+", ".~.-1+")
  return (paste(base, paste(stringVector,collapse="+"), sep=""))					
}

getAIC <- function(model.formula, model){return (AIC(update(model, model.formula))[2])}

svystep <- function(model, keep = NULL){
  # Variable selection for svyglm objects based on stepwise reduction of AIC
  model.summary <- summary(model)
  
  candidate.coeffs <- names(attr(model.summary$terms, "dataClasses"))
  candidate.coeffs <- candidate.coeffs[-c(1,length(candidate.coeffs))] # remove dependent variable and "(weights)"
  
  if (!is.null(keep)){ # are we forcing any variable in the model?
    stopifnot(all(keep %in% candidate.coeffs))
    candidate.coeffs <- candidate.coeffs[-which(candidate.coeffs %in% keep)]
    
    null.model <- update(model, paste(".~1+", paste(keep, collapse = "+")))
  } else {
    null.model <- update(model, ".~1")
  }
  
  null.aic <- AIC(null.model)[2]
  
  iteration.step <- 0 # use to determine when backward elimination should be attempted
  
  while (TRUE){
    iteration.step <- iteration.step+1
    # forward selection
    candidate.formula <- apply(expand.grid(candidate.coeffs), 1, makeFormula)
    candidate.aic <- sapply(candidate.formula, getAIC, null.model)
    
    if (all(candidate.aic > null.aic)) {
      return (null.model)
    }
    
    null.model <- update(null.model, candidate.formula[which.min(candidate.aic)])# update statement had model
    null.aic <- AIC(null.model)[2]; stopifnot(!is.na(null.aic))
    candidate.coeffs <- candidate.coeffs[-which.min(candidate.aic)]
    
    # display current model
    m <- summary(null.model); bestTerms <- names(attr(m$terms, "dataClasses"))
    bestTerms <- bestTerms[c(-1,-length(bestTerms))]; cat(paste(bestTerms, collapse = "+++"), "\n")
    
    # backward elimination 
    if (iteration.step > 2) { # backward elimination makes sense only when we have at least 3 variables in the model
      null.summary <- summary(null.model)
      null.coeffs <- names(attr(null.summary$terms, "dataClasses"))
      null.coeffs <- null.coeffs[-c(1,length(null.coeffs))] 
      
      if (!is.null(keep)){
        null.coeffs <- null.coeffs[-which(null.coeffs %in% keep)]
      }
      
      backward.elimination.aic <- sapply(paste(".~.-", null.coeffs), getAIC, null.model)

      if (any(backward.elimination.aic < null.aic)){ 
        null.model <- update(null.model, paste(".~.-", null.coeffs[which.min(backward.elimination.aic)]))
        candidate.coeffs <- c(candidate.coeffs, null.coeffs[which.min(backward.elimination.aic)]) # put variable back into pool of candidate coeffs
        
        m <- summary(null.model); bestTerms <- names(attr(m$terms, "dataClasses"))
        bestTerms <- bestTerms[c(-1,-length(bestTerms))]; cat(paste(bestTerms, collapse = "---"), "\n")
      }
    }
  }
}