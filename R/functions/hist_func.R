hist_func <- function(data, 
                      x = "DAMAGE", 
                      title = "",
                      bwidth = NULL,
                      facet = FALSE) {
  
  # pick binwidth value
  if(x=="DAMAGE") {
    bwidth = 1
  }
  else if (!facet) {
    library(sf)
    data <- as.data.frame(data)
    y <- unlist(data[x] %>% st_drop_geometry())
    breaks <- pretty(range(y), n = nclass.FD(y), min.n = 1)
    bwidth <- breaks[2]-breaks[1]
  }
  
  # make facet_wrap plot from covariates
  if(facet) {
    temp <- c("ID", "DAMAGE", "LAT", "LON", "AREA")
    X <- data[, !names(data) %in% temp] 
    # change data frame to long format for facet wrapping
    X.rows <- as.data.frame(do.call(rbind, X))
    X.rows$vars <- colnames(X)
    X.long <- X.rows %>% tidyr::gather(key, value, -vars)
    X.long$value <- as.numeric(X.long$value)
    
    g <- ggplot(X.long) + 
      # Add the histogram layer
      geom_histogram(aes(x = value),
                     color = "black", 
                     fill = "white",
                     bins = 100) +
      facet_wrap(~ vars) +
      ylab("Frequencies") +
      ggtitle(paste(title)) +
      theme(plot.title = element_text(hjust = 0.5))
    
  }
  # make one plot from response/covariate
  else {
    g <- ggplot(data, aes(x = as.numeric(get(x)))) + 
      # Add the histogram layer
      geom_histogram(binwidth = bwidth, 
                     color = "black", 
                     fill = "white") +
      xlab(x) +
      ylab("Frequencies") +
      ggtitle(paste(title)) +
      theme(plot.title = element_text(hjust = 0.5))
  }
  
  g
    
}
