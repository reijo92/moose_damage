library(MASS)
library(boot)
library(tidyverse)
library(sf)

source("functions/backward_modsel.R")
source("functions/scale_covariates.R")

path <- ""
data <- readRDS(paste0(path, "freq_data.rds"))

# z-score standardize the relative frequency data frame
df <- scale_covariates(data)
  
# select the covariate names
covar <- df %>% 
  dplyr::select(!c(ID,DAMAGE,LON,LAT,AREA)) %>% 
  st_drop_geometry() %>% 
  names() 


#collect the adjacency matrix object to a variable
adj.mat <- paste(getwd(), "/Lattice.graph", sep="")

###############################################################################
### RUN BACKWARD MODEL SELECTION ##############################################

#model without spatial random effect
for (i in 1:10) {
  res<-backward_modsel(covar, 
                           family = "nbinomial", 
                           data = df,
                           spatial_model = FALSE,
                           return_all = FALSE) 
  # use return_all argument to only return the best model without other 
  # models
  
  #check that models folder exists
  path <- file.path(getwd(), "models")
  # Check if the folder exists, and if not, create it
  if (!file.exists(path)) {
    dir.create(path)
    cat("Folder created:", path, "\n")
  }
  #SAVE MODEL
  saveRDS(res, paste0("models/nbin_model",i ,".rds"))
}


for (i in 1:10) {
  res_spatial<-backward_modsel(covar,   
                               spatial_model = TRUE,  
                               family = "nbinomial",   
                               data = df, 
                               return_all =FALSE
                           )
  
  #check that models folder exists
  path <- file.path(getwd(), "models")
  # Check if the folder exists, and if not, create it
  if (!file.exists(path)) {
    dir.create(path)
    cat("Folder created:", path, "\n")
  }
  #SAVE MODEL
  saveRDS(res_spatial, paste0("models/nbin_model_spatial",i ,".rds"))
}

###############################################################################
### LOAD THE SAVED MODEL SELECTION RESULTS ####################################

# load the regular INLA Negative Binomial backward model selection results
mod_names <- paste("nbin_model", 1:10, ".rds", sep = "")
path <- file.path(getwd(), "models")

nbin_mods <- lapply(mod_names, function(file) {
  file_path <- file.path(path, file)
  readRDS(file_path)
})

# load the INLA spatial Negative Binomial backward model selection results
mod_names_spat <- paste("nbin_model_spatial", 1:10, ".rds", sep = "")

nbin_mods_spat <- lapply(mod_names_spat, function(file) {
  file_path <- file.path(path, file)
  readRDS(file_path)
})

nbin_mods[[1]]
nbin_mods_spat[[1]]
