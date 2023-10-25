marginal_transform <- function(x_name, model) {
  # This function transforms marginal likelihoods to natural 
  # scale (exp transformation)
  marg <- inla.tmarginal(function(x) exp(x), model$marginals.fixed[c(x_name)][[1]])
  marg
}