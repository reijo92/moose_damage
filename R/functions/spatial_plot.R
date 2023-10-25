spatial_plot <- function(model, data, finland_map = FALSE) {
  # this function collects the spatial random effects of the bym model, 
  # transforms them to natural scale and plots the effect of zeta = exp(u+v)
  
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
  # transform the spatial random effects to the natural scale (exp transformation)
  zeta <- lapply(csi, function(x) inla.emarginal(exp, x))
  #Define the cutoff for zeta
  zeta.cutoff <- c(0,1,5,10, 50,100, max(unlist(zeta)))
  #Transform zeta in categorical variable
  cat.zeta <- cut(unlist(zeta),breaks=zeta.cutoff,
                      include.lowest=TRUE)
  data$zeta <- cat.zeta
  
  th <- theme()
  #NOTE: by adding col=NA to aes in geom_sf, you can make grid lines disappear
  g <- g + geom_sf(data = data, aes(fill = zeta)) +
            scale_fill_brewer(palette= "Greens") + 
    labs(fill = expression(zeta))+
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