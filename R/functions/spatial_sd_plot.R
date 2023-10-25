spatial_sd_plot <- function(model, data, finland_map = FALSE) {
  
  g <- ggplot()
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
  
  # create spatial maps from spatial random effects
  Nareas <- nrow(data)
  
  # collect the spatial random effects u + v
  # NOTE! The values of spatially structured effect u without unstructured 
  # effect v are saved in elements (Nareas+1):(2 * Nareas) (rest of the rows)
  csi <- model$marginals.random$ID[1:Nareas]
  
  # calculate standard deviations
  a <- 0
  prob.zone <- lapply(csi, function(x) {1 - inla.pmarginal(a, x)})
  
  # define the cutoff values for standard deviations
  prob.zone.cutoff <- c(0, 0.1, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
  
  cat.prob.zone <- cut(unlist(prob.zone),
                       breaks = prob.zone.cutoff,
                       include.lowest = T)
  
  data$cat.prob.zone <- cat.prob.zone
  
  g <- g + geom_sf(data = data, aes(fill = cat.prob.zone)) +
            scale_fill_brewer(palette= "BuPu")+ 
    labs(fill = expression(paste("p(",zeta[i]>1,"|",bold(y),")")))+
    th +
    theme(panel.background = element_rect(fill = background_color),
          panel.grid.major = element_line(colour = NA))
  
  # this needs to be at the end so the ggplot zooms to the right area at the end
  # when entire map has been created
  if(finland_map) {
   g <- g +  
      coord_sf(xlim = c(20, 27), ylim = c(61.5, 65), expand = FALSE)
  }
  
  g
}