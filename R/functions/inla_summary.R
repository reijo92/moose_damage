inla_summary <- function(summ_list, var_names) {
  # this function makes the summary table of the INLA model results
  library(dplyr)
  
  # set length of the inla.zmarginal summaries
  n <- length(summ_list)
  # set names of the covariates + intercept (rownames)
  names <- names(summ_list[[1]])
  # collect the summary of the intercept
  m <- summ_list[[1]]
  
  # rowbind summaries of the covariates to the summary row of the intercept 
  #the m is now longer vector than the summ. why?
  for (i in 1:(n-1)) {
    summ <- summ_list[[i]]
    print(summ)
    m <- rbind(m, summ)
  }
  
  # transfer matrix to data frame
  summdf <- as.data.frame(m)
  colnames(summdf) <- names
  rownames(summdf) <- var_names
  
  return(summdf)
}
