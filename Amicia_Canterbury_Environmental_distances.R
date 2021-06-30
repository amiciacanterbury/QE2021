# Author: Amicia Canterbury
# Date: 30 June 2021
# Quantitative Ecology
# Environmental distances

library(vegan) # used for multivariate analysis & diversity analysis
library(ggplot2)
library(geodist) # for calculating geographical distances between latitudes & longitudes
library(ggpubr) # to arrange the multipanel graphs
library(dplyr)

# Load data & find the size of the d.f.

library(readr)
xyz_data <- "https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/diversity/Euclidian_distance_demo_data_xyz.csv"
xyz <- read_csv(url(xyz_data))

dim(xyz)# to look at how many rows & columns there are
# 7 rows and 4 columns 

glimpse(xyz)
xyz 
View(xyz)

# First column = site names 

#-------------------Calculating euclidean distances-----------------------------

# use vegdist() for the dissimilarity index
# views the lower triangle

xyz_euc <- round(vegdist(xyz[, 2:4], method = "euclidian", 
                         upper = FALSE, diag = TRUE), 4) # select only cols 2, 3 and 4
xyz_euc

# round() rounds decimals to the number at the end (4)
# method = what type of dissimilarity index you want 
# if upper = TRUE, shows the upper triangle too
# diag = FALSE, does not show the diagonal (identical site comparisons)

# convert the data to a d.f 

xyz_df <- as.data.frame(round(as.matrix(xyz_euc), 4)) # as.matrix = converting d.f into a matrix
                                                      # as.data.frame = check if the object is a data frame
xyz_df

# the data now shows both the upper and lower triangle with the diagonal

# Distance matrices have the same properties as dissimilarity matrices:
# - The matrix is square (# of columns = # of rows)
# - Diagonal is filled with 0 (identical site comparisons)
# - Matrix = symmetrical (upper triangle = lower triangle)

# the larger the number, the more dissimilar sites are i.t.o environmental conditions

# substitute the x,y and z with environmental variables

xyz_fic <- "https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/diversity/Euclidian_distance_demo_data_env.csv"

env_fict <- read_csv(url(xyz_fic)) # This data set is just renaming the environmental variables

env_fict

head(env_fict)

env_fict1 <- round(vegdist(env_fict[, 2:4], method = "euclidian", 
                         upper = FALSE, diag = TRUE), 4)

env_fict1

#--------------------------SEAWEED DATA-----------------------------------------

load("Quantitative_Ecology-main/exercises/diversity/SeaweedEnv.RData")

dim(env) # Looks at the number of columns & rows

# 58 rows, 18 columns

round(env[1:5, 1:5], 4) # Looking at the first five rows, rounded to 4 decimals

round(env[(nrow(env) - 5):nrow(env), (ncol(env) - 5):ncol(env)], 4) # Last 5 rows, 4 decimals

# each row = site, each column = environmental variable

# what are the names of the environmental variables?

colnames(env) # data is true definition of multivariate data

# Select only certain thermal variables:

env1 <- dplyr::select(env, febMean, febRange, febSD, augMean,
                      augRange, augSD, annMean, annRange, annSD) # Names of the variables we looking at 
env1

#-----------------------------------Z-SCORES------------------------------------

# z-scores help standardized the data. 
# it is necessary when the variables are measured in different units.

E1 <- round(decostand(env1, method = "standardize"), 4) # decostand() to standardized the data,
                                                        # instead of doing it manually.

E1[1:5, 1:5] # Looks at the first 5 rows and columns. 

#-------------------CALCULATE EUCLIDIAN DISTANCES-------------------------------

E1_euc <- round(vegdist(E1, method = "euclidian", upper = TRUE), 4)
E1_df <- as.data.frame(as.matrix(E1_euc))
E1_df[1:10, 1:10] # the first 10 rows and columns

# make a plot with the above matrix

ggplot(data = E1_df, (aes(x = 1:58, y = `1`))) +
  geom_line(col = "seagreen4", size = 0.75) + 
  labs(title = "Environmental distances of a coastal section in South Africa",
       subtitle = "Using Euclidian distances", x = "Coastal Section(W to E)",
       y = "Environmental distance")+
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50,60))+
  theme_bw()

#--------------------EUCLIDEAN DISTANCES WITH GEOGRAPHICAL DATA-----------------

# When we calculate Euclidean distances between geographic lat/long coordinate, 
# the relationship between sections will be the same (but scaled) as actual 
# geographic distance.

geo <- read_csv("Quantitative_Ecology-main/exercises/diversity/sites.csv")

dim(geo)

# 58 rows, 2 columns

head(geo) # Looks at the top of the dataset

# First column must be removed.

# Calculate geographic distances (in meters) between coordinate pairs:

dists <- geodist(geo, paired = TRUE, measure = "geodesic") # geodist() = the distance between the coordinates
dists_df <- as.data.frame(as.matrix(dists)) # creating d.f
colnames(dists_df) <- seq(1:58) # 
dists_df[1:5, 1:5] # 

# creates data set with both triangles and the diagonal present

# create plots:

plt1 <- ggplot(data = dists_df, (aes(x = 1:58, y = `1`/1000))) +
  geom_line() + xlab("Coastal section, west to east") + ylab("Distance (km)") 
+ ggtitle("Actual geographic distance")

# data for the euclidian data

dists_euc <- vegdist(geo, method = "euclidian")
dists_euc_df <- round(as.data.frame(as.matrix(dists_euc)), 4)
dists_euc_df[1:5, 1:5]

plt2 <- ggplot(data = dists_euc_df, (aes(x = 1:58, y = `1`))) +
  geom_line() + xlab("Coastal section, west to east") + ylab("Euclidian distance") 
+ ggtitle("Euclidian distance")

ggarrange(plt1, plt2, nrow = 2) # Puts both the plots together.


