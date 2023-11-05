make_prior_list <- function(prior_num = 1, 
                            prior_dist = "loggamma", 
                            shape = list(1), 
                            rate = list(0.001) ) {
  # this function makes the INLA prior list. prior_num parameter controls
  # how many priors the list contains.
  # prec.unstruct = defines prior for the Gaussian iid noise parameter
  #                 "v" in the BYM model
  # prec.spatial = defines prior for the spatially structured effect "u"
  #                in the BYM model
  
  prior.list <- list()
  
  for (i in 1:prior_num) {
    prior.list[[i]] <- paste0('list(prec.unstruct = list(prior = "', prior_dist, '", 
                                        param = c(', shape[[i]], 
                                        ',', rate[[i]],')),
                                       prec.spatial = list(prior = "', 
                                       prior_dist,'", 
                                       param = c(',shape[[i]],
                                       ',', rate[[i]], ')))')
    
    #remove new lines
    prior.list[[i]] <- gsub("\n", " ", prior.list[[i]])
    
    
  }
  
  prior.list
  
}
    