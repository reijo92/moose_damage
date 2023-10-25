library(dplyr)
library(sf)
setwd("C:/Users/reijo/OneDrive/Documents/Thesis/R")
data <- readRDS("C:/Users/reijo/Desktop/yliopisto/master_thesis/moose_forest_damages/data/Data_Frames/data.rds")
# rename abbreviated column names
colnames(data) <-  c("ID", "DAMAGE", "OPENMIN", "OPENPEAT", "PINEPLAMIN", "PINEPLAPEATND", "PINEPLAPEATDR", "OTHERPLANT", "PINETHIMIN", "PINETHIPEATND", "PINETHIPEATDR", "OTHERTHI", "MATURE", "WATER", "INHABITED", "AGRI", "MANMADE", "MAINROAD", "REGROAD", "CONNROAD", "FORROAD", "OTHERROAD", "DENSITY", "LON", "LAT", "AREA", "geometry")
data <- data %>% st_drop_geometry()
data <- data[-c(1,24:26)]
covar <- colnames(data)

# data scaling
#data[covar] <- scale(data[covar] %>% st_drop_geometry())

# model matrix
X <- data[covar]
head(X)

#frequencies
s <- apply(X, 2, sum)
s
newdf <- sapply(1:ncol(X), function(i) X[,i] / s[i] *100)
colnames(newdf) <- colnames(X)

# min, mean and max values
summary(newdf)

# standard deviations
apply(newdf,2,sd)

# dispersion test for overdispersion in the data
#https://www.sciencedirect.com/science/article/abs/pii/030440769090014K

library(AER)
rd <- glm(DAMAGE ~ ., data = data, family = poisson)
dispersiontest(rd,trafo=1)


#create relative frequency data frame
s <- apply(X, 2, sum)
freqX <- sapply(1:ncol(X), function(i) X[i] / s[i] * 100)
freqX <- as.data.frame(do.call(cbind,freqX))

#parse summary output to create summary table
summ <- summary(X)
summ.relative <- summary(freqX)
summ.relative

min <- max <- mean <- median <- c()
min.relative <- max.relative <- mean.relative <- median.relative <- c()
for (i in 1:ncol(summ)) {
  min <- c(min, get_num(summ, 1, i)) 
  max <- c(max, get_num(summ, 6, i)) 
  mean <- c(mean, get_num(summ, 4, i)) 
  median <- c(median, get_num(summ, 3, i)) 
  min.relative <- c(min.relative, get_num(summ.relative, 1, i)) 
  max.relative <- c(max.relative, get_num(summ.relative, 6, i)) 
  mean.relative <- c(mean.relative, get_num(summ.relative, 4, i)) 
  median.relative <- c(median.relative, get_num(summ.relative, 3, i)) 
}

sd<-round(apply(X, 2, sd))
sd.relative<-round(apply(freqX, 2, sd))

covars <- colnames(X)
summdf<-data.frame(variable = covars, 
                   min = round(min), 
                   median = round(median), 
                   mean = round(mean), 
                   max = round(max),
                   sd = sd)
summdf
summ.relative.df<-data.frame(variable = covars, 
                   min = round(min.relative, 2), 
                   median = round(median.relative, 2), 
                   mean = round(mean.relative, 2), 
                   max = round(max.relative, 2))
summ.relative.df

#data histograms
#breaks <- pretty(range(y), n = nclass.FD(y), min.n = 1)
#bwidth <- breaks[2]-breaks[1]

ggplot() + 
  geom_histogram(aes(x=df$DAMAGE), 
                 color = "black", fill = "white",
                 bins = 15) + 
  ggtitle("") +
  xlab("DAMAGE")

ggsave("damage_hist.pdf", width = 6, height = 6, dpi = "screen")


temp.vars <- c("PINEPLAPEATDR","PINEPLAPEATND","PINETHIPEATDR","PINETHIPEATND", "CONNROAD", "FORROAD", 
               "MAINROAD", "REGROAD","OTHERROAD", "DENSITY")
temp.vars <- c("CONNROAD", "FORROAD",  "MAINROAD", "REGROAD","OTHERROAD")
tempdf <- data[3:23] %>% st_drop_geometry()
X2 <- tempdf[temp.vars]
X <- tempdf[which(!(colnames(tempdf) %in%  temp.vars))] 

X <- as.data.frame(scale(X))
X<-df[3:23] %>% st_drop_geometry()
X.rows <- as.data.frame(do.call(rbind, X))
X.rows$vars <- colnames(X)
X.long <- X.rows %>% tidyr::gather(key, value, -vars)

X2.rows <- as.data.frame(do.call(rbind, X2))
X2.rows$vars <- colnames(X2)
X2.long <- X2.rows %>% tidyr::gather(key, value, -vars)

ggplot(data = X2.long) + 
  geom_histogram(aes(x = value), color = "black", fill = "white", bins = 100) + 
  facet_wrap(~ vars) +
  ggtitle("") +
  xlab("x")

ggsave("x_hist_facet2.pdf", width = 6, height = 6, dpi = "screen")

long_func <- function(data, ind) {
  x<-data[ind]
  x <- as.data.frame(do.call(rbind, x))
  x$vars <- colnames(data[ind])
  x.long <- x %>% tidyr::gather(key, value, -vars)
  return(x.long)
}

dat.list <- list()
X <- data[3:23] %>% st_drop_geometry()
for (i in 1:ncol(X)) {
  x <- long_func(X, i)
  dat.list[[i]] <- x
}

g.list <- list()
for (i in 1:ncol(X)) {
  x <- dat.list[[i]]
  p <- ggplot(x) + 
           geom_histogram(aes(x = value), 
                          color = "black", 
                          fill = "white", 
                          bins = 40) + 
           facet_wrap(~ vars) +
           ggtitle("") +
           xlab("x")
  g.list[[i]] <- p
}

grid.arrange(g.list[[1]],
             g.list[[2]],
             g.list[[3]],
             g.list[[4]],
             g.list[[5]],
             g.list[[6]],
             g.list[[7]],
             g.list[[8]],
             g.list[[9]],
             g.list[[10]],
             g.list[[11]], 
             g.list[[12]], 
             g.list[[13]],
             g.list[[14]],
             g.list[[15]],
             g.list[[16]],
             g.list[[17]],
             g.list[[18]],
             g.list[[19]],
             g.list[[20]],
             g.list[[21]],
             ncol=7)

ggsave("xhist.pdf", width = 12, height = 12, dpi = "screen")
library("ggpubr")

xhist<-ggarrange(plotlist = g.list, ncol=2, nrow=2)
xhist[[1]]
xhist[[2]]
ggexport(xhist, filename = "xhist.pdf")

grid.arrange(g.list[[1]],
             g.list[[2]],
             g.list[[3]],
             g.list[[5]],
             g.list[[6]],
             g.list[[4]],
             ncol=2)

grid.arrange(g.list[[7]],
             g.list[[8]],
             g.list[[9]],
             g.list[[10]],
             g.list[[11]], 
             g.list[[12]],
             ncol=2)


ggsave("xhist7.pdf", width = 12, height = 12, dpi = "screen")

grid.arrange(g.list[[12]], 
             g.list[[13]],
             g.list[[14]],
             g.list[[15]],
             g.list[[16]],
             g.list[[17]],
             ncol=2)
grid.arrange(g.list[[18]],
             g.list[[19]],
             g.list[[20]],
             g.list[[21]],
             ncol=2)

get_num <- function(summ, row, col) {
  as.numeric(sub('.*:', '', summ[row,col]))
}

