## Attach packages
library(mapview)
library(RColorBrewer)
library(utils)
library(sf)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(patchwork)
library(tidyverse)
library(GGally)
library(corrplot)
library(psych)


source("functions/damage_plot.R")
source("functions/hist_func.R")

## Load the data
# define your data path (empty string if the data is in the code folder)
path <- ""

data <- readRDS(paste0(path, "data.rds"))
borders <- readRDS(paste0(path, "borders.rds"))
glimpse(data)

# filter only the damage areas
border_names <- c("Rannikko-Pohjanmaa - Pohjanmaa 1", 
                  "Rannikko-Pohjanmaa - Pohjanmaa 2",
                  "Rannikko-Pohjanmaa - Pohjanmaa 3",
                  "Oulu 5")


### SUMMARY ####################################################################
summary(data)
str(data)
################################################################################


### HISTOGRAMS #################################################################

#select only covariates from data frame
temp <- c("ID", "DAMAGE", "LAT", "LON", "AREA")
X <- data[, !names(data) %in% temp] 

#covariate names without sf geometry column
n <- length(X)
cov_names <- names(X)[-n]

# damage histogram
hist_func(data)

# facet covariance plot (plots are very small because they use the same
# x and y limits)
hist_func(data, facet = TRUE)


# Plot covariate to separate plots

#covariate histograms
#binwidths are determined automatically except damage which is bwidth = 1.
#if you want manually set the bwidth argument, then you have to give a vector
#of bwidth values for the function
hist_list <- list()
#bwidth <- c(rep(30,length(cov_names)))
bwidth = NULL
for (i in 1:length(cov_names)) {
  plot <- hist_func(data, x = cov_names[i], bwidth = bwidth)
  hist_list[[i]] <- plot
}

hist_list[[1]]
hist_list[[2]]
hist_list[[3]]

################################################################################


### MAPS #######################################################################

# GGPLOTS 

damage_plot(data, finland_map = TRUE)

# Covariate maps




# Load the ggpubr package if not already loaded
library(ggpubr)

# Create an empty list to store ggplot objects
plot_list <- list()

# Draw the maps in a for loop
for (i in 1:length(cov_names)) {
  g <- ggplot() +
    geom_sf(data = X, aes_string(fill = cov_names[i])) +
    scale_fill_viridis_c()
  
  # save plot to a list
  plot_list[[i]] <- g
}

#first two covariate plots
plot_list[[1]]
plot_list[[2]]

# MAPVIEW

# relevant areas 
data_borders <- borders %>% dplyr::filter(AREA %in% border_names)
# moose management areas in Finland
mapview(borders)
# 4 relevant moose management areas 
mapview(data_borders)

# mapview map with damage areas and relevant moose management areas in the 
# background
mapview(data_borders, legend = NULL) + 
  mapview(data, 
          zcol = "DAMAGE", 
          col.regions = brewer.pal(9, "Reds"))

################################################################################


### CORRELATIONS ###############################################################

ff1 <- data[,c(2:23)] %>% st_drop_geometry() %>% cor()
ff1

# Extract the indices of the correlation coefficients greater than 0.5
cor_indices <- which(abs(ff1) > 0.5 & upper.tri(ff1), arr.ind = TRUE)

# Extract the correlation coefficients greater than 0.5
significant_correlations <- data.frame(
  var1 = rownames(ff1)[cor_indices[, 1]],
  var2 = colnames(ff1)[cor_indices[, 2]],
  corr = ff1[cor_indices]
)
significant_correlations

#correlation plot
corrplot(ff1, method = "color", 
         type = "lower", 
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.7)

################################################################################