inla_formula <- function(cov, spatial_prior=NULL) {
  # This function takes covariates and priors for the precisions of the BYM
  # model and constructs an INLA formula with fixed effects and spatial random 
  # effect.
  
  # Formula with only fixed effects
  f <- as.formula(paste0("DAMAGE ~ 1 + ", paste(cov, collapse = "+")))
                         
  
  # Add spatial random effect (optional)
  if(!is.null(spatial_prior)) {
    
    # get the prior list name from spatial_prior variable
    #prior_list_name <- deparse(substitute(spatial_prior))
    
    # add the spatial random effect
    f <- as.formula(paste0("DAMAGE ~ 1 + ", paste(cov, collapse = "+"),
                          "+ f(ID, 
                          model='bym',  
                          graph=adj.mat,  
                          scale.model = TRUE, 
                          hyper =", 
                          spatial_prior,
                          ")" ))
  }
  
  return(f)
}