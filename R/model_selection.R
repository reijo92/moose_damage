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
  
#### IMPORTANT VARIABLE #######################################################
# select the covariates
# Make sure that this contains all the variables you want to be part of the
# model selection
covar <- df %>% 
  dplyr::select(!c(ID,DAMAGE,LON,LAT,AREA)) %>% 
  st_drop_geometry() %>% 
  names() 
###############################################################################

#collect the adjacency matrix object to a variable
adj.mat <- paste(getwd(), "/Lattice.graph", sep="")

###############################################################################
### RUN BACKWARD MODEL SELECTION ##############################################

#iterations
n <- 10 
mod.list <- list()

#model without spatial random effect
for (i in 1:n) {
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


# SPATIAL MODEL ################################################################
for (i in 1:n) {
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
  # this needs to be done every round to not lose previous results if
  # the model selection gets interrupted/error happens
  saveRDS(res_spatial, paste0("models/nbin_model_spatial",i ,".rds"))
  
  # AUTOMATIC MODEL AVERAGING ##################################################
  if (i==n) {
    # load the INLA spatial Negative Binomial backward model selection results
    mod_names_spat <- paste("nbin_model_spatial", 1:n, ".rds", sep = "")
    
    nbin_mods_spat <- lapply(mod_names_spat, function(file) {
      file_path <- file.path(path, file)
      readRDS(file_path)
    })
  
      # calculate the occurence of each variable in the models selected by
      # backward model selection function.
      # Idea:
      # 1. calculate the number of occurence of each covariate in each selected
      #    model
      # 2. calculate the total sum of occurence of each variable
      # 3. calculate the percentage if the variable is in >50 % of the models
      #    (model averaging)
      
      cov.list <- list()
      for (i in 1:length(covar)) {
        temp.list <- list()
        # covariate
        cov <- covar[i]
        for (j in 1:n) {
          # check if covariate is in the model. returns boolean value TRUE/FALSE
          temp<- cov %in% nbin_mods_spat[[j]][[1]]
          temp.list[[j]] <- temp
          
        }
        cov.list[[i]] <- list(temp.list)
      }
      # calculate the occurence of each covariate
      # each covariate is in list indexes 1:21 and each of those indexes 
      # have list length of n and its values are the occurence of current 
      # variable in each iteration
      temp <- lapply(cov.list, unlist)
      v <- lapply(temp, table)
      #IMPORTANT THRESHOLD VARIABLE THAT CONTORLS THE AVERAGING ################
      threshold <- 0.5
      #########################################################################
      best.cov <- list()
      for (i in 1:length(v)) {
        
        #select TRUE values
        ind <- which(names(v[[i]]) == TRUE)
        if (!(identical(ind, integer(0)))) {
          true.values <- v[[i]][[ind]]
          if( (true.values / n) >= threshold ) {
            # select the associated covariate
            best.cov[[i]] <- covar[[i]] 
          }
        }
      }
  ##############################################################################
      best.cov <- unlist(best.cov)
      saveRDS(best.cov, paste0("models/best_cov.rds"))
  }
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
