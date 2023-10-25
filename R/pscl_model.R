require(pscl)
require(MASS)
require(boot)

source("functions/scale_covariates.R")

data <- readRDS("freq_data.rds")
# rename abbreviated column names
geometry <- data$geometry


# model matrix
X <- data %>% dplyr::select(!c(ID,DAMAGE,LON,LAT,AREA)) %>% st_drop_geometry()

# scale the relative frequency variables 
df <- scale_covariates(data)

m1 <- zeroinfl(DAMAGE ~ MATURE + DENSITY + PINETHIPEATDR,
               data = df, dist = "negbin")
summary(m1)

