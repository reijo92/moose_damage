library(ggplot2)
library(INLA)
library(tidyverse)
library(sf)
library(patchwork)
library(gridExtra)
library(RColorBrewer)
library(spdep)

source("functions/inla_formula.R")
source("functions/inla_summary.R")
source("functions/unscale.R")
source("functions/marginal_transform.R")
source("functions/collect_marginals.R")
source("functions/plot_marginals.R")
source("functions/proportion_of_variance.R")
source("functions/spatial_plot.R")
source("functions/spatial_sd_plot.R")
source("functions/damage_plot.R")
source("functions/make_prior_list.R")
source("functions/fit_inla.R")
source("functions/scale_covariates.R")

path <- ""
# relative frequency data with z-score standardization
data <- readRDS(paste0(path,"data.rds"))
freq_data <- readRDS(paste0(path,"freq_data.rds"))

glimpse(freq_data)

# STANDARDIZATION ############################################################


X <- freq_data %>% dplyr::select(!c(ID,DAMAGE,LON,LAT,AREA)) %>% st_drop_geometry()

# collect original mean and sd of the coefficients for unscaling the results
x_mean <- apply(X, 2, mean)
x_sd <- apply(X, 2, sd)

# scale the relative frequency variables 
df <- scale_covariates(data)

# use scaled freq_data with the analysis
freq_data <- df

###############################################################################


#prior distribution means and variances can be checked by sampling from 
#the log-gamma distribution with shape and rate parameter values corresponding
#to the set priors

#example
#y<-log(rgamma(10000,shape=1,rate=0.01))
#breaks <- pretty(range(y), n = nclass.FD(y), min.n = 1)
#bwidth <- breaks[2]-breaks[1]
#g1<-ggplot() + geom_histogram(aes(x=y), binwidth = bwidth,
#                              color = "black", fill = "white")+
#  ggtitle("Default loggamma prior")
#g1

#seeing the distribution helps to build intuition of the effect of the prior
#choice to the precision values of the BYM model

#make a list of priors for the spatial random effect
#first argument is the prior for the precision of the unstructured effect "\nu"
#(noise term)
#second argument is for the spatially structured effect "u"

# give prior.list function as many shape and rate parameters inside a list
# as number of different priors. if no arguments are given, then the function
# makes prior list for the bym model parameters with shape = 1 and rate = 0.001

#prior.list <- make_prior_list()

prior.list <- make_prior_list(prior_num = 2, 
                              shape = list(1,2), 
                              rate = list(0.001, 0.0005))

#collect the adjacency matrix object to a variable
adj.mat <- paste(getwd(), "/Lattice.graph", sep="")

#covariates selected by backward model selection
cov <- c("OTHERPLANT", "PINETHIPEATDR", "MATURE", "DENSITY")
#spatial model
#spatial_cov <- c("MATURE", "PINETHIPEATDR", "DENSITY")
spatial_cov <- c("OPENPEAT", "PINETHIMIN", "WATER", "INHABITED", "AGRI", "DENSITY" )

# MODEL FITTING
# Notice uninformative priors for the fixed effects. Priors for the fixed 
# effects can be changed through the argument control.fixed

# The function inla_formula can be used only for the "bym" model

f <- inla_formula(cov)
zinb <- fit_inla(data = freq_data, 
                 formula = f, 
                 model = "nbinomial")

# spatial model with 1. prior
f2 <- inla_formula(spatial_cov, prior.list[[1]])
zinb2 <- fit_inla(data = freq_data, 
                formula = f2, 
                model = "nbinomial",
                spatial = TRUE)

# spatial model with 2. prior
#f3 <- inla_formula(cov, prior.list[[2]])
#mod3 <- fit_inla(data = freq_data, 
#                formula = f3, 
#                model = "nbinomial",
#                spatial = TRUE)


###############################################################################
# SUMMARIES ###################################################################

round(zinb$summary.fixed, 3)
round(zinb$summary.hyperpar, 3)
zinb$mlik[1,]

round(zinb2$summary.fixed, 3)
round(zinb2$summary.hyperpar, 3)
zinb2$mlik[1,]

###############################################################################
# NOTE! This section assumes the model is spatial model, so if fitting
# regular INLA model, then don't run sections that assume the model is using
# spatial random effects. Those sections are 
# porpotion of variance and 2 spatial plots
#

# MARGINAL TRANSFORMS ##########################################################

# Transform the fixed value marginals to natural scale (exp transform)
marginal.list <- list()
summ.list <- list()
#posterior marginal of the intercept
int.marg <- marginal_transform("(Intercept)", zinb2)
marginal.list[[1]] <- int.marg
n <- length(spatial_cov)

#posterior marginals of the covariates 
for (i in 1:n) {
  marg <-marginal_transform(spatial_cov[i], zinb2)
  marginal.list[[i+1]] <- marg
}

###############################################################################

int.summ <- inla.zmarginal(int.marg)
summ.list[[1]] <- int.summ

for (i in 1:n) {
  summ <- inla.zmarginal(marginal.list[[i+1]])
  summ.list[[i+1]] <- summ
}

# make summary table of the transformed results
summdf <- inla_summary(summ.list, c("Intercept", spatial_cov))
summdf          
          

# REMOVE STANDARDIZATION #######################################################

res.summ.unscaled <- unscale(summdf, x_mean, x_sd)
round(res.summ.unscaled,3 )

###############################################################################

# COLLECT AND PLOT MARGINAL POSTERIORS ########################################

# FIXED EFFECT POSTERIORS 

# smoothed marginal posterior list of the fixed effects
fixed.marginals <- collect_marginals(zinb2)

# fixed effect plots
plot_marginals(fixed.marginals)

# HYPERPARAMETER POSTERIORS 

# smoothed marginal posterior list of the hyperparameters
hyperpar.marginals <- collect_marginals(zinb2, hyper = TRUE, to_sd = TRUE)

# assign a vector of greek letters to use for plotting the hyperparameter
# marginal posterior plots. These are for the negative binomial models
# hyperparameters. ZINB model would also need the letter "pi[0]" for
# the zero-probability
greek_letters <- c("alpha", "sigma[nu]", "sigma[u]")

# hyperparameter plots
plot_marginals(hyperpar.marginals, hyper = T, greek_letters = greek_letters)

###############################################################################

### VARIANCE EXPLAINED BY THE SPATIALLY STRUCTURED EFFECT #####################

proportion_of_variance(zinb2)

###############################################################################

### PLOTS #####################################################################
# use the "data" that is data frame with sf geometry column
# these plots are plotted with and without Finland map on purpose as an 
# example

# original damage map with finland map in the background
p1 <- damage_plot(data, zinb2, finland_map = TRUE)

# fitted map without finland map
p2 <- damage_plot(data, zinb2, fitted = TRUE)

grid.arrange(p1,p2, ncol=1)

# spatial plot of zeta = exp(u + v)
g1 <- spatial_plot(data, zinb2)

# spatial standard deviation plot 
g2 <- spatial_sd_plot(data, zinb2)

grid.arrange(g1,g2, ncol=1)

###############################################################################


