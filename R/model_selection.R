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

res_list <- list()

#model without spatial random effect
res<-backward_modsel_log(covar, 
                         family = "nbinomial", 
                         data = df,
                         spatial_model = FALSE)

for (i in 1:1) {
  res_spatial<-backward_modsel_log(covar,  
                                   spatial_model = TRUE, 
                                   family = "nbinomial",  
                                   data = df
                           )
  res_list[[i]] <- res_spatial
}


# regular mod
names <- c("OTHERPLANT", "PINETHIPEATDR",        "MATURE",       "DENSITY" )
mlik <- c(-955.8871,     -973.6269,     -952.3712,     -959.4444 )

max_mlik <- max(mlik)

# spatial_mod
names2 <- c("OPENPEAT", "PINETHIMIN",      "WATER",  "INHABITED",       "AGRI",    "DENSITY" )
mlik2 <- c(-572.5017,  -557.8849,  -544.2926,  -529.0390,  -546.2804,  -530.5564 )


max_mlik2 <- max(mlik2)

#best log marginal likelihoods
max_mlik ; max_mlik2

log_bf <- 2*(max_mlik2-max_mlik)
log_bf
#846.6644 > 150 = very strong evidence
