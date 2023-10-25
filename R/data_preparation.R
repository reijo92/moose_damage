
#Load the packages

library(sf)
library(tidyverse)
library(sp)
library(tidyr)
library(spdep)
library(INLA)

# Loading data
# define your data path (empty string if the data is in the code folder)
path <- ""

# Moose damage data by grid
damage <- read.csv(paste0(path, "Gridvahinkoja.csv"), header = TRUE)
moose_density <- read.csv(paste0(path, "moose_density.csv"), header = TRUE)
# road length data
roads <- read.csv(paste0(path, "Tiepituuspergricell.csv"), header = TRUE)
# environment variable data
environment <- read.csv(paste0(path, "MLVMIareaingridcells.csv"), header = TRUE)
# read moose economy name and Finland borders
borders <- st_read(paste0(path, "HTA_MAA"),  "HTA_Luke_2016", options = "ENCODING=WINDOWS-1252")



# Data cleaning ############################################################

glimpse(moose_density)
#rename index X to ID
moose_density <- moose_density %>% rename(ID = X)
#calculate the mean of the moose density
mean_density <- apply(moose_density[,4:9],1,mean)
#save the coordinates
LAT=moose_density$POINT_Y
LON=moose_density$POINT_X

density <- data.frame(ID=moose_density$ID, 
                      DENSITY=mean_density)

# Remove not needed variables
damage <- damage[,c(4,10)]
colnames(damage) <- c("ID", "DAMAGE")
# Replace NA values with zeroes 
damage <- damage %>%
  mutate_all(~ replace_na(., 0))
glimpse(damage)

# remove unnecessary columns
borders <- borders[,c(2)]
colnames(borders) <- c("AREA","geometry")
glimpse(borders)

# Rename the columns
roads <- roads[,2:7] 
colnames(roads) <-  c("ID", "MAINROAD","REGROAD","CONNROAD","FORROAD","OTHERROAD")
glimpse(roads)

environment <- environment[,2:17] 
colnames(environment) <-  c("ID", "OPENMIN", "OPENPEAT", "PINEPLAMIN", "PINEPLAPEATND", "PINEPLAPEATDR", "OTHERPLANT", "PINETHIMIN", "PINETHIPEATND", "PINETHIPEATDR", "OTHERTHI", "MATURE", "WATER", "INHABITED", "AGRI", "MANMADE")
glimpse(environment)


# merge data frames by id
data <- merge(damage, environment, by = "ID", all = TRUE)
data <- merge(data, roads, by = "ID", all = TRUE)
data <- merge(data, density, by = "ID", all = TRUE)
data$LON <- LON
data$LAT <- LAT
glimpse(data)
############################################################################
# MAKE SF DATA FRAME

#transform to sf data frame 
# (note that st_as_sf function removes the LON, LAT columns and put them
# in the geometry column)
data <- st_as_sf(data,coords = c("LON","LAT"), crs=25835)

#make sure the coordinate system is the same with damage data and moose 
#management area data
data <- st_transform(data, st_crs(borders))

#save the original coordinates 
data$LON <- LON
data$LAT <- LAT

# add area names
data <- st_join(data, borders, join = st_within, largest = TRUE)

## Remove data from other than main areas
areas <- c("Rannikko-Pohjanmaa - Pohjanmaa 1", 
           "Rannikko-Pohjanmaa - Pohjanmaa 2", 
           "Rannikko-Pohjanmaa - Pohjanmaa 3", 
           "Oulu 5")

#specify the package, because dplyr packages could be masked by other packages
#with the same function names
data <- data %>% dplyr::filter(AREA %in% areas)

#replace the old ID column with missing ID values with continuous ID column
ID <- 1:1065
data$ID <- ID

############################################################################
## CONVERT POINTS TO POLYGONS

source("functions/make_squares.R")
# use data points as centroids of the polygons
centroids <- data.frame(id=data$ID,lon=data$LON,lat=data$LAT)
# make polygons from the centroids
squares <- make_squares(centroids)
(data <- data %>% 
    st_drop_geometry() %>% 
    cbind(squares) %>% 
    st_as_sf() %>% 
    st_transform(st_crs(borders)))

### RELATIVE FREQUNCY DF #######################################################
#Create data frame that has scaled relative frequency values of the coefficients

# save the geometry column in order to be able to convert df back to sf_df
geometry <- data$geometry

# convert sf_df to df and remove unnecessary columns
X <- data %>% dplyr::select(!c(ID,DAMAGE,LON,LAT,AREA)) %>% st_drop_geometry()

#relative frequencies (%)
sum <- X %>% summarise(across(everything(), ~ sum(.))) %>% unlist()
freq_df <- sapply(1:ncol(X), function(i) X[i] / sum[i] * 100) %>% as.data.frame()

# add the coordinates + area names to the relative frequency data frame
freq_df$LON <- data$LON
freq_df$LAT <- data$LAT
freq_df$AREA <- data$AREA

freq_df <- freq_df %>% 
  data.frame(ID=data$ID, 
             DAMAGE=data$DAMAGE, 
             ., 
             geometry=geometry) %>% 
  st_as_sf()

glimpse(freq_df)



###############################################################################
## SAVE THE SF DATA FRAMES

# put your path to the variable df_path where you want to save the modified
# sf data frames
df_path <- ""

saveRDS(data, paste0(df_path, "data.rds"))
saveRDS(freq_df, paste0(df_path, "freq_data.rds"))
saveRDS(borders, paste0(df_path, "borders.rds"))

############################################################################

# MAKE ADJACENCY MATRIX FOR BYM MODEL ##########################################

# Specify the adjacency matrix
Lattice_Temp <- poly2nb(data)  # construct the neighbour list

# create the adjacency matrix in INLA format and save it to the work directory
# for later use
nb2INLA("Lattice.graph", Lattice_Temp) 

# draw the adjacency matrix plot
LDN.adj <- paste(getwd(),"/Lattice.graph",sep="")
H <- inla.read.graph(filename="Lattice.graph")
image(inla.graph2matrix(H),xlab="",ylab="")

############################################################################