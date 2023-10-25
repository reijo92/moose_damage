plot_marginals <- function(marginal_list, 
                           fixed = TRUE, 
                           hyper = FALSE,
                           greek_letters = NULL) {
  # greek letters = greek letters to put in the place of original hyperparameter
  #                 names. Needs to be as many as hyperparameters
  
  # collect x and y values of the marginal posteriors to two columns
  marginals <- data.frame(do.call(rbind, marginal_list))
  
  print(marginals)
  # collect names of the marginals
  names <- names(marginal_list)
  n <- length(names)
  
  # make column named "value" that tells the variable name associated to 
  # each value of x and y
  value <- c()
  for (i in 1:n) {
   temp <- c(rep(names[i], times = nrow(marginal_list[[i]])))
   value <- c(value, temp)
  }
  
  marginals$value <- value
  
  if(hyper && !is.null(greek_letters)) {
    # 1. measure the each variable length
    # 2. use rep() to put the desired letters in the place of original hyperparameter
    #     names
    n <- sapply(marginal_list, nrow)
    value <- c()
    for (i in 1:length(greek_letters)) {
      temp <- c(rep(greek_letters[i],n[i]))
      value <- c(value,temp)
    }
    
    marginals$value <- value
    
  }
    
  #factorize the value
  marginals$value <- as.factor(marginals$value)
  
  # plot the fixed effects marginal posteriors
  if (fixed && !hyper) {
    g <- ggplot(marginals, aes(x = x, y = y)) + 
      geom_line() +
      facet_wrap(~ value, scales = "free") +
      labs(x = "", y = "Density") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle("Posteriors of the fixed effects")
  }
  # plot the hyperparamater marginal posteriors
  if (hyper) {
    g <- ggplot(marginals, aes(x = x, y = y)) + 
      geom_line()
    # if greek letters are used, then use labeller = label_parsed
      if (!is.null(greek_letters)) {
        g <- g + facet_wrap(~ value, scales = "free", labeller = label_parsed)
      }
      else {
        g <- g + facet_wrap(~ value, scales = "free")
      }
      g <- g + labs(x = "", y = "Density") + 
               theme_bw() + 
               ggtitle("Posteriors of the hyperparameters") + 
               theme(plot.title = element_text(hjust = 0.5))
  }
  
  g
  
}