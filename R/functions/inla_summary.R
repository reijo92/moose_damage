inla_summary <- function(summ_list, var_names) {
  library(dplyr)
  
  n <- length(summ_list)
  names <- names(summ_list[[1]])
  m <- summ_list[[1]]
  
  for (i in 1:(n-1)) {
    temp <- summ_list[[i]]
    m <- rbind(m, temp)
  }
  
  summdf <- as.data.frame(m)
  #there the error happens
  colnames(summdf) <- names
  rownames(summdf) <- var_names
  
  return(summdf)
}
