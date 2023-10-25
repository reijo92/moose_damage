backward_modsel_log <- function(covar, 
                                spatial_model = FALSE, 
                                family, 
                                data, 
                                graph = NULL, 
                                prior_mean_fixed = 0, 
                                prior_prec_fixed = 0.001){
  # This is a model selection function that implements backward model selection
  # using R-INLA
  require(INLA); require(ggplot2); require(magrittr); require(tidyverse)
  source("functions/make_prior_list.R") 
  source("functions/inla_formula.R") 
  prior.list <- NULL
  
  #spatial model
  if (spatial_model) {
    
    prior.list <- make_prior_list(prior_num = 1, 
                                  shape = list(1), 
                                  rate = list(0.001))
      
    f1 <- inla_formula(covar, prior.list[[1]])
  }
  
  #model without spatial random effect
  else {
    f1 <- inla_formula(covar)
  }
  
  #assign lists to collect model information
  ModelList <- AllMLList <- list()
  MLList <- dBFList <- list()
  print("Starting model selection...")
  
  #Saturated model (full model with all the covariates)
  #the prior choice for fixed effect can be changed through parameters
  #prior_mean_fixed = 1, and prior_prec_fixed = 0.001
  FullModel <- inla(f1,
                    family = family,
                    data = data,
                    control.compute = list(mlik=TRUE),
                    control.fixed = list(mean = prior_mean_fixed, 
                                         prec = prior_prec_fixed))
  
  
  #add log-marginal likelihood to the list
  MLList[[1]] <- as.vector(FullModel$mlik[1,])
  
  # save marginal likelihood(s) to the list to return after the model selection
  AllMLList[[1]] <- MLList[[1]]
  AllMLList[[1]] <- setNames(AllMLList[[1]], c("full_model"))
  
  # initialize iteration vars and max iteration  var "n" for full model
  n <- length(covar)
  i <- 1
  j <- 1
  k <- 1
  
  # initialize best.mod variable with original full model
  # this is needed, because we want one variable (covar) to have all the origi-
  # nal variables and one variable (best.mod) to update during the model
  # selection
  best.mod <- covar
  
  cov.name <- c()
  print(paste0("Length of the full model: ", n))
  print(paste0("Marginal likelihood of the full model: ", MLList[[1]]))
  
  #loop to remove covariates one by one and fitting a model for each model
  while(TRUE){
    print(paste0("Round: ", k))
    #remove covariates one by one. the "i" variable must be reset each round
    # to i=1
    covar2 <- paste(best.mod[-i], collapse = " + ")
    
    #spatial model
    if (spatial_model) {
      
      prior.list <- make_prior_list(prior_num = 1, 
                                    shape = list(1), 
                                    rate = list(0.001))
        
      f2 <- inla_formula(covar2, prior.list[[1]])
    }
    
    #model without spatial random effect
    else {
      f2 <- inla_formula(covar2)
    }
    # assign new formula without one covariate
    
    # print current covariates in selection
    print(covar2)
    
    
    # try max 4 times to fit the model in case of convergence error
    while (j<5) {
      # use error handling to prevent function crash in case of convergence 
      # error
      tryCatch(
        {
          #fit new model without one covariate
          Model1 <- inla(f2,
                         family = family,
                         data = data,
                         control.compute = list(mlik=TRUE),
                         control.fixed = list(mean = prior_mean_fixed, 
                                              prec = prior_prec_fixed))
        },
        error = function(e) {
          message("INLA convergence error")
          print(e)
        }
      )
      
      # make sure that there is marginal likelihood value. if not, fit the 
      # model again
      if (any(is.na(Model1$mlik))) {
        next
      }
      
      # if the convergence error didn't happen, exit the while loop
      if (!is.null(Model1)) {
        break
      }
      j <- j + 1
    }
    
    
    #collect the new model
    ModelList[[i]] <- Model1
    
    # if we have iterated through the model parameters, get new results
    if (i==n) {
      
      # collect all the log marginal likelihoods from this round
      MLList[[2]] <- sapply(ModelList, function(y) as.vector(y$mlik[1,]))
      
      print(MLList[[2]])
      
      # assign names to the marginal likelihoods from this round
      MLList[[2]] <- setNames(MLList[[2]], best.mod)
      
      print("Log-marginal likelihoods of the candidate models: ")
      print(MLList[[2]])
      
      # save all the marginal likelihood results to a list to return after
      # model selection
      AllMLList[[(k+1)]] <- MLList[[2]]
      
      # calculate Bayes Factors using best model from previous round as a 
      # reference
      dBFList[[k]] <- 2*(MLList[[2]] - MLList[[1]])
      
      # assign names to the Bayes Factors from this round
      dBFList[[k]] <- setNames(dBFList[[k]], best.mod)
      
      print("Bayes factors: ")
      print(round(dBFList[[k]],3))
      
      # if the 2log Bayes Factor is not larger than two, exit the loop and 
      # stop model selection
      max<-max(unlist(dBFList[[k]]))
      print("Max BF value:")
      print(max)
      
      if (max(unlist(dBFList[[k]]))<2) {
        break
      }
      
      
      # find the index of the highest Bayes Factor (best model)
      # that index has the name of the removed covariate that can be used
      # to remove it from the new model
      ind <- which(unlist(dBFList[[k]]) == max(unlist(dBFList[[k]])))
      
      # what if two variables have exact same max BF value?
      # get length of the max value vector
      len <- length(ind)
      if (len > 1) {
        #choose randomly which variable will be removed
        v <- 1:len
        i <- sample(v, 1)
        ind <- ind[i]
      }
        
      cov <- dBFList[[k]][ind]
      cov.name <- names(cov)
      
      # update the best.mod variable to contain new model by removing the one
      # covariate
      best.mod <- best.mod[-c(which(cov.name ==  best.mod))]
      
      print(paste0("Losing: ", cov.name))
      
      # get new max iteration length "n" (decreasing number)
      n <- length(best.mod)
      
      # check that there is more than one covariate to remove. otherwise stop
      # the model selection
      if (n == 1) {
        break
      }
      
      #use the marginal likelihood from the best model as reference
      MLList[[1]] <- MLList[[2]][ind]
      
      # reset the variables
      ModelList <- list()
      MLList[[2]] <- list()
      k <- k + 1 
      i <- 1
      
      # go back to the beginning of the loop without increasing the "i" value
      # from 1
      next
    }
    # reset the iteration parameter "j"
    j<-1
    # add one  to "i" each round until i = n
    i <- i+1
  }
  final.model <- best.mod
  print(paste0("Final model: "))
  print(final.model)
  #ALLMLList contains marginal likelihoods and variable names
  return(AllMLList)
}

