damage_plot <- function(data, 
                        model = NULL,
                        fitted = FALSE, 
                        finland_map = FALSE) {
  # This function draws the map of damage and fitted damage values. The map of 
  # Finland is optional.
  
  g <- ggplot()
  # define the red discreet palette for plotting
  
  th <- theme()
  background_color <- "grey"
  
  
  if(finland_map) {
    library("rnaturalearth")
    library("rnaturalearthdata")
    
    finland <- ne_countries(scale = "medium", 
                            country="finland", 
                            returnclass = "sf")
    
    g <- g + geom_sf(data = finland, fill = "grey") + 
      annotate("text", x = 24.5, y = 62.5, label = "Finland")
    
    #change the background color for cyan colored sea
    background_color <- "cyan"
    
  }
  
  if(fitted) {
    #fitted damage maps
    # get the marginal posterior means of the fitted values
    fitted.val <- model$summary.fitted.values$mean
    # round values to integers
    fitted.val <- round(fitted.val)
    
    max_fitted_val <- max(fitted.val)
    
    
    fitted.cutoff <- c(0,0.99, 1,2,3,4,5,10, max_fitted_val)
    cat.fitted <- cut(fitted.val, breaks=fitted.cutoff, include.lowest = TRUE)
    data$fitted.value <- cat.fitted
    
    g <- g + geom_sf(data = data, aes(fill = fitted.value)) + 
      scale_fill_brewer(palette = "Reds",  
                        labels=c("0","1","2","3","4","5","[6,10]",  
                                 paste("[11,", max_fitted_val, "]"))) + 
      labs(fill = "FITTED") +
      th +
      theme(panel.background = element_rect(fill = background_color),
            panel.grid.major = element_line(colour = NA))
  }
  else {
    
    damage.cutoff <- c(0,0.99, 1,2,3,4,5,10, max(data$DAMAGE))
    cat.damage <- cut(data$DAMAGE, breaks=damage.cutoff, include.lowest = TRUE)
    data$DAMAGE_CAT <- cat.damage
    
    g  <- g + geom_sf(data = data, aes(fill = DAMAGE_CAT)) + 
      scale_fill_brewer(palette = "Reds",  
                        labels=c("0","1","2","3","4","5","[6,10]","[11,15]")) +  
      labs(fill = "DAMAGE") +
      th +
      theme(panel.background = element_rect(fill = background_color),
            panel.grid.major = element_line(colour = NA))
    
  }

  # this needs to be at the end so the ggplot zooms to the right area at the end
  # when entire map has been created
  if(finland_map) {
   g <- g +  
      coord_sf(xlim = c(20, 27), ylim = c(61.5, 65), expand = FALSE)
  }
  g
  
}
