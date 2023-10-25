scale_covariates <- function(freq_data) {
  # this function uses the scale_func to z-score standardize the covariates
  # that are in relative frequencies of the original covariates
  
  # collect the variables other than covariates to put them back to the 
  # result data frame
  geometry <- freq_data$geometry
  ID <- freq_data$ID
  DAMAGE <- freq_data$DAMAGE
  LON <- freq_data$LON
  LAT <- freq_data$LAT
  AREA <- freq_data$AREA
  
  # model matrix
  X <- freq_data %>% dplyr::select(!c(ID,DAMAGE,LON,LAT,AREA)) %>% st_drop_geometry()
  
  
  # define z-score standardization function (same as scale() function)
  scale_func <- function(x) {
    (x - mean(x)) / sd(x)
  }
  
  # scale the relative frequency variables 
  X <- apply(X,2,scale_func) %>% as.data.frame()
  X <- X %>% 
    data.frame(ID=data$ID, 
               DAMAGE=data$DAMAGE, 
               ., 
               LON=LON,
               LAT=LAT,
               AREA=AREA,
               geometry=geometry) %>% 
    st_as_sf()
  
  return(X)
}