make_squares <- function (centroids) {
  
  library(sp)
  # set the radius for the plots
  radius <- 2500 # radius in meters
  
  # define the plot edges based upon the plot radius. 
  
  yPlus <- centroids$lat+radius
  xPlus <- centroids$lon+radius
  yMinus <- centroids$lat-radius
  xMinus <- centroids$lon-radius
  
  # calculate polygon coordinates for each plot centroid. 
  square=cbind(xMinus,yPlus,  # NW corner
               xPlus, yPlus,  # NE corner
               xPlus,yMinus,  # SE corner
               xMinus,yMinus, # SW corner
               xMinus,yPlus)  # NW corner again - close ploygon
  
  # Extract the plot ID information
  ID=centroids$id
  
  
  # First, initialize a list that will later be populated
  # a, as a placeholder, since this is temporary
  a <- vector('list', length(2))
  
  # loop through each centroid value and create a polygon
  # this is where we match the ID to the new plot coordinates
  for (i in 1:nrow(centroids)) {  # for each for in object centroids
    a[[i]]<-Polygons(list(Polygon(matrix(square[i, ], ncol=2, byrow=TRUE))), ID[i]) 
    # make it an Polygon object with the Plot_ID from object ID
  }
  
  # convert a to SpatialPolygon and assign CRS
  polysB<-SpatialPolygons(a,proj4string=CRS(as.character("+proj=utm +zone=35 +units=m +no_defs +ellps=GRS80 ")))
  
  return(st_as_sf(polysB))
   
}







