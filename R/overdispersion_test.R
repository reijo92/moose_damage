library(dplyr)
library(sf)
library(MASS)

data <- readRDS("data.rds")

#remove geometry column
data <- data %>% st_drop_geometry()
#fit poisson model
pois_mod <- glm(DAMAGE~., data = data, family = poisson(link = "log"))

#overdispersion test
AER::dispersiontest(pois_mod, trafo=1)

#fit negative binomial model
nb_mod <- glm.nb(DAMAGE ~ ., data = data)


#Residual plot for Poisson regression
p_res <- resid(pois_mod)
plot(fitted(pois_mod), p_res, col='steelblue', pch=16,
     xlab='Predicted Offers', ylab='Standardized Residuals', main='Poisson')
abline(0,0)

#Residual plot for negative binomial regression 
nb_res <- resid(nb_mod)
plot(fitted(nb_mod), nb_res, col='steelblue', pch=16,
     xlab='Predicted Offers', ylab='Standardized Residuals', main='Negative Binomial')
abline(0,0)
