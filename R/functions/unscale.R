unscale <- function(summdf, x_mean, x_sd) {
  # Unscaling function
  # formula: x_scaled = (x - x_mean) / x_sd) <-> x = x_scaled * x_sd + x_mean
  
  # take coefficient names (without intercept)
  var_names <- rownames(summdf[-1,])
  
  # make sure the columns are numeric vectors
  summdf <- sapply(summdf, as.numeric)
  
  # collect coefficient rows from summary output
  x_scaled <- summdf[-1,]
  
  # select only the correct coefficients 
  # (note! the x_mean and x_sd contains all the coefficient means and 
  # standard deviations)
  x_mean <- x_mean[var_names]
  x_sd <- x_sd[var_names]
  
  # unscale to get the original relative frequency covariate values
  x <- x_scaled * x_sd + x_mean
  # combine
  new_summdf <- rbind(summdf[1,],x) %>% as.data.frame()
  rownames(new_summdf) <- c("Intercept", var_names)
  
  return(new_summdf)
}
