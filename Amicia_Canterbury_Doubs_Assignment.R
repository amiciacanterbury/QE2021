# Author: Amicia Canterbury
# Date: 3 July 2021
# Quantitative Ecology
# Doubs Data Assignment 

# Load packages: 

library(tidyverse) 
library(betapart) # Partitioning beta diversity into turnover & nestedness components
library(vegan)
library(gridExtra) # Arrange multiple grid-based plots on a page and draw tables 
library(BiodiversityR)
library(grid)
library(gridBase)
library(tidyr)
library(readr)

# Load the data: 

DoubsSpe <- read_csv("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/DoubsSpe.csv")
View(DoubsSpe)

dim(DoubsSpe)# used to look at the dimensions of the data 

# 30 rows & 28 columns - one row has to be filtered out 
# Rows = sites 
# Columns = fish species 

unique(DoubsSpe) # used to delete duplicate values/ rows
summary(DoubsSpe)
head(DoubsSpe) # Looks at the top half of the data 
tail(DoubsSpe) # Looks at the bottom half of the data

# Filter data in order to remove first column name:
Doubs <- dplyr::select(DoubsSpe, -1)

#----------------------------QUESTIONS------------------------------------------

# Question 1: 
# The DoubsSpe data set looks at the different types of fish species found on the
# Doubs River. The data was obtained by looking at 30 different site on the river
# where the abundance of the fish species were taken at the specific sites. There 
# are 27 different species of fish found here. 

# Question 2:
# One would use the Bray-Curtis dissimilarity index as it is used for 
# the species abundance of the data. The closer the number is to 1, the more 
# dissimilar the data is when doing the pair-wise comparisons. The closer the 
# value is to 0, the more similar the two sites are to one another when doing 
# the pair-wise comparisons. 

# Question 3: 
# Applying the Bray-Curtis calculation to the data: 

Doubs.dis <- round(vegdist(Doubs,method ="bray"),4)
Doubs.dis

# Question 4: 
# The results are a mix of similar and dissimilar localities, however the data does
# not have a clear pattern as certain dissimilarity indices are much higher than others
# when being compared to sites that are basically next to one another. There is 
# an inconsistency when looking at the data. 

# Question 5: 
# Due to the data being taken from a mountain range, the environments tend to 
# be more diverse but less similar to one another due to the altitude. Certain 
# localities that are found within the data set tends to be more similar than others
# however there are large spikes within the dissimilarity index. Numbers are 
# irregular and does not show an exact pattern in terms of how similar or dissimilar
# the environments are over physical distance. Towards the end of the data set,
# the dissimilarity indices fall within a range of just below or above 0.1. As 
# mentioned above, the closer the value is to 0, the more similar the environments
# are. The similarity that is found at the end of the data set may be due to the 
# environment becoming lower in altitude and thus the environment becoming less 
# heterogeneous compared to the other initial localities. The climate has a big 
# impact on the way the environments develop. The higher in altitude one goes,
# the larger the difference in climate. Due to the this, the flora and fauna of
# the environments would have to adapt and thus changing their "sweet spots" 
# or changing the environments that they once lived in for more comfortability.
# The general pattern is that the further away from the original site, the more
# dissimilar the sites are to one another due to range of altitude the data was
# taken from. 


# Question 6:
# Create a data matrix:
species_data <- as.matrix(Doubs.dis)

# Create a data frame:
species <- as.data.frame(species_data) # transforms the data

dim(species) # Checks the dimensions of the data matrix 

# 30 rows & 30 columns 
# rows = sites/location
# columns = species abundance of fish species

view(species)

# Create a plot with the data matrix:

ggplot(data = species, aes(x = 1:30, y = species[,1]))+
  geom_line(size = 0.75, col = "springgreen4")+
  labs(title = expression(underline("Species dissimilarity of fish species in the Doubs River")),
       subtitle = "Using Bray-Curtis dissimilarity index", 
       x = "Location", y = "Species dissimilarity")


# Question 7:
# This plot shows that the further away from the original location, the higher the 
# dissimilarity of the different sites. Certain sites on the river shows higher 
# similarity than others indicating that the environment on the Doubs river 
# has environmental gradients that make locations on the river over physical distance
# similar and others become more dissimilar. Initially the data has a sharp 
# increasing, showing the initial sites are very dissimilar. There are spikes in
# between ranging from site 8 to site 13. The similarity index then flattens out
# showing that the environment in the area where the sites occur is very similar.
# Due to the data being taken from near the France-Switzerland border in a mountain
# range, the dissimilarity of certain localities may be due to the altitude of
# where the sites occur. The altitude plays a big role in how similar or dissimilar
# environments are to one another. Climate changes with altitude, which may affect the 
# water properties of the river water. Certain localities may fall within a zone 
# where there is minimal oxygen or an area where the water is very pure. 


# Question 8: 

# Creating a new data matrix using method = presence/ absence: 
Spec1 <- round(decostand(species, method = "pa", 4)) # decostand() to standardized the data,
                                                              # instead of doing it manually.

Spec1[1:5, 1:5] # Looks at the first 5 rows and columns.

view(Spec1)
head(Spec1) # Looking at the top half of the data
tail(Spec1) # Looking at the bottom half of the data 

# Use Sørensen index to create P/A data matrix: 
sor <- vegdist(Spec1, binary = TRUE) # binary = TRUE sets to presence/absence data
sor_df <- round((as.matrix(sor)),4) # creates data matrix rounded to 4 decimals
sor_df[1:30, 1:30] # the first 30 rows & columns 

# creates a dissimilarity matrix 


# Question 9:
# Create a data frame:
doubs <- as.data.frame(sor_df) # transforms the data 

# Create a graph: 
ggplot(data = doubs, aes(x = 1:30, y = species[,1]))+
  geom_line(size = 0.75, col = "green")+
  labs(title = expression(underline("Species dissimilarity of fish species in the Doubs River")),
       subtitle = "Using Sørensen dissimilarity index", 
       x = "Location", y = "Species dissimilarity")

