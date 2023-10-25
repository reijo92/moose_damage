proportion_of_variance <- function(model) {
  # this function calculates the proportion of variance explained by each
  # bym models component "nu" and "u". Returns the variance explained by
  # the spatially structured effect "u".
  
  #get the proportion explained by spatial random effect u
  marg.hyper <- inla.hyperpar.sample(100000, model)
  
  names <- colnames(marg.hyper)
  
  #formula u_var = tau_v / tau_v + tau_u
  tau_v <- marg.hyper[,which(colnames(marg.hyper) == "Precision for ID (iid component)")]
  tau_u <- marg.hyper[,which(colnames(marg.hyper) == "Precision for ID (spatial component)")]
  perc.var.u1 <- mean(tau_v / (tau_v + tau_u))
  
  return(perc.var.u1)
}