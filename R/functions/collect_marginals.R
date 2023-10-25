collect_marginals <- function(model, fixed=TRUE, hyper=FALSE, to_sd = TRUE) {
  
  # to_sd = transfrom marginal posteriors of the hyperparameter precisions
  #         to standard deviations
  
  # This function collects the marginal posteriors from the model object,
  # smooths the posteriors and returns a list of marginal posterior matrices.
  
  #fixed effect posteriors
  if(fixed && !hyper) {
    #collect coefficient names
    names <- model$names.fixed
    # collect the list containing the marginal posteriors of the hyper parameters
    marginals.list <-  model$marginals.fixed
    # smooth the marginal posteriors with inla.smarginal
    marginals.list <- lapply(marginals.list, 
                                    function(x) inla.smarginal(x))
    
  #loop all the coefficients
  marginals <- list()
  for (i in 1:length(names)) {
    var_name <- names[i]
    #create matrix of the coefficient posterior
    coef <- marginals.list[[var_name]] %>%
            lapply(unlist) %>%  
            do.call(cbind, .)  
    
    #collect the posterior matrix to a list
    marginals[[i]] <- coef
    
  }
  
  names(marginals) <- names
  
  }
  
  #hyperparameter posteriors
  if(hyper) {
    #collect coefficient names
    names <- names(model$marginals.hyperpar)
    # collect the list containing the marginal posteriors of the hyper parameters
    marginals.list <-  model$marginals.hyperpar
    
    if (to_sd) {
      n <- length(names)
      #transform marginal posteriors of the precisions of the bym model to 
      #standard deviations
      sigma_nu<-inla.tmarginal(function(x) x^(-1/2), 
                               model$marginals.hyperpar$`Precision for ID (iid component)`)
      
      sigma_u<-inla.tmarginal(function(x) x^(-1/2), 
                               model$marginals.hyperpar$`Precision for ID (spatial component)`)
      
      # replace the old values of the precisions with standard deviations
      
      marginals.list[[(n-1)]] <- sigma_nu
      marginals.list[[n]] <- sigma_u
      
      new_names <- names[-(n-1)]
      new_names <- new_names[-(n-1)]
      
      names(marginals.list) <- c(new_names, 
                                 "SD for ID (iid component)", 
                                 "SD for ID (spatial component)")
      
    }
    
    marginals <- marginals.list
    
  }

  #return the marginals list
  return(marginals)
}