library(INLA)
setwd("C:/Users/reijo/OneDrive/Documents/Thesis/R")
data <- readRDS("C:/Users/reijo/Desktop/yliopisto/master_thesis/moose_forest_damages/data/Data_Frames/data.rds")
library(tidyverse)
library(sf)
X <- data[3:23] %>% st_drop_geometry()
y <- data[2] %>% st_drop_geometry()
y <- unname(unlist(as.vector(y)))

sum <- apply(X, 2, sum)
temp <- as.data.frame(sapply(1:ncol(X), function(i) X[,i] / sum[i]))
colnames(temp) <- colnames(X)
X <- temp
X <- scale(X)

z <- y
z[z>0] <- 1
y <- y[y>0]

length(y)
length(z[z==1])
#same length as espected

n <- length(z)
n.y <- length(y)

Y = matrix(NA, n + n.y, 2)
Y[1:n, 1] = z
Y[n + 1:n.y, 2] = y

# create new ID to match the new response matrix Y 
# is this actually needed?
ID <- c(data$ID, data$ID[which(z>0)])

# Define a different intercept for each likelihood
Intercept1 <- c(rep(1, n), rep(NA, n.y))
Intercept2 <- c(rep(NA, n), rep(1, n.y))

# there is some issues here because no NA values for appropriate values
ldat <- list(
  Y = Y,
  ID = ID,
  I1 = Intercept1,
  I2 = Intercept2,
  OPENMIN.z = c(X[,1], rep(NA, n.y)),
  OPENPEAT.z = c(X[,2], rep(NA, n.y)),
  PINEPLAMIN.z = c(X[,3], rep(NA, n.y)),
  PINEPLAPEATND.z = c(X[,4], rep(NA, n.y)),
  PINEPLAPEATDR.z = c(X[,5], rep(NA, n.y)),
  OTHERPLANT.z = c(X[,6], rep(NA, n.y)),
  PINETHIMIN.z = c(X[,7], rep(NA, n.y)),
  PINETHIPEATND.z = c(X[,8], rep(NA, n.y)),
  PINETHIPEATDR.z = c(X[,9], rep(NA, n.y)),
  OTHERTHI.z = c(X[,10], rep(NA, n.y)),
  MATURE.z = c(X[,11], rep(NA, n.y)),
  WATER.z = c(X[,12], rep(NA, n.y)),
  INHABITED.z = c(X[,13], rep(NA, n.y)),
  AGRI.z = c(X[,14], rep(NA, n.y)),
  MANMADE.z = c(X[,15], rep(NA, n.y)),
  MAINROAD.z = c(X[,16], rep(NA, n.y)),
  REGROAD.z = c(X[,17], rep(NA, n.y)),
  CONNROAD.z = c(X[,18], rep(NA, n.y)),
  FORROAD.z = c(X[,19], rep(NA, n.y)),
  OTHERROAD.z = c(X[,20], rep(NA, n.y)),
  DENSITY.z = c(X[,21], rep(NA, n.y)),
  OPENMIN.y = c(rep(NA, n), X[,1][which(z>0)]),
  OPENPEAT.y = c(rep(NA, n), X[,2][which(z>0)]),
  PINEPLAMIN.y = c(rep(NA, n), X[,3][which(z>0)]),
  PINEPLAPEATND.y = c(rep(NA, n), X[,4][which(z>0)]),
  PINEPLAPEATDR.y = c(rep(NA, n), X[,5][which(z>0)]),
  OTHERPLANT.y = c(rep(NA, n), X[,6][which(z>0)]),
  PINETHIMIN.y = c(rep(NA, n), X[,7][which(z>0)]),
  PINETHIPEATND.y = c(rep(NA, n), X[,8][which(z>0)]),
  PINETHIPEATDR.y = c(rep(NA, n), X[,9][which(z>0)]),
  OTHERTHI.y = c(rep(NA, n), X[,10][which(z>0)]),
  MATURE.y = c(rep(NA, n), X[,11][which(z>0)]),
  WATER.y = c(rep(NA, n), X[,12][which(z>0)]),
  INHABITED.y = c(rep(NA, n), X[,13][which(z>0)]),
  AGRI.y = c(rep(NA, n), X[,14][which(z>0)]),
  MANMADE.y = c(rep(NA, n), X[,15][which(z>0)]),
  MAINROAD.y = c(rep(NA, n), X[,16][which(z>0)]),
  REGROAD.y = c(rep(NA, n), X[,17][which(z>0)]),
  CONNROAD.y = c(rep(NA, n), X[,18][which(z>0)]),
  FORROAD.y = c(rep(NA, n), X[,19][which(z>0)]),
  OTHERROAD.y = c(rep(NA, n), X[,20][which(z>0)]),
  DENSITY.y = c(rep(NA, n), X[,21][which(z>0)])
)




form <- Y ~ -1 + I1 + I2 + OPENMIN.z + OPENPEAT.z + PINEPLAMIN.z + PINEPLAPEATND.z + PINEPLAPEATDR.z + OTHERPLANT.z + PINETHIMIN.z + PINETHIPEATND.z + PINETHIPEATDR.z + OTHERTHI.z + MATURE.z + WATER.z + INHABITED.z + AGRI.z + MANMADE.z + MAINROAD.z + REGROAD.z + CONNROAD.z + FORROAD.z + OTHERROAD.z + DENSITY.z + OPENMIN.y + OPENPEAT.y + PINEPLAMIN.y + PINEPLAPEATND.y + PINEPLAPEATDR.y + OTHERPLANT.y + PINETHIMIN.y + PINETHIPEATND.y + PINETHIPEATDR.y + OTHERTHI.y + MATURE.y + WATER.y + INHABITED.y + AGRI.y + MANMADE.y + MAINROAD.y + REGROAD.y + CONNROAD.y + FORROAD.y + OTHERROAD.y + DENSITY.y + 
f(ID, model = "bym",
  graph = damages.adj,
  scale.model = TRUE,
  hyper = list(
    prec.unstruct = list(prior = "loggamma", param = c(1,0.001)),   # precision for the unstructured effect (residual noise)
    prec.spatial =  list(prior = "loggamma", param = c(1,0.001))    # precision for the spatial structured effect
       )
     )

damages.adj <- paste(getwd(), "/damages.graph", sep="") # name the object


res <- inla(form, data=ldat,
		family=c("binomial", "zeroinflatedpoisson0"),
		control.compute=list(dic=TRUE,cpo=TRUE, waic = TRUE, 
                                   return.marginals.predictor = TRUE,
                                   config=TRUE,residuals = TRUE),
              control.predictor = list(compute = TRUE),
              control.inla = list(int.strategy = "grid", diff.logdens = 4, 
                                  strategy = "laplace", npoints = 21)) #better cpo values




summary(res)
round(res$summary.fixed[1:5],3)

# Extract the coefficient summary for the zero-inflated covariates
zero_inflation_summary <- res$summary.fixed[grep("\\.z$", rownames(res$summary.fixed)), ]

# Extract the coefficient summary for the count distribution covariates
count_distribution_summary <- res$summary.fixed[grep("\\.y$", rownames(res$summary.fixed)), ]

# Print the two separate summary outputs
cat("Summary for Zero-Inflation Covariates:\n")
print(round(zero_inflation_summary, 3))

cat("\nSummary for Count Distribution Covariates:\n")
print(round(count_distribution_summary, 3))



