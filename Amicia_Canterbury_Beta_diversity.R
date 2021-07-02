# Author: Amicia Canterbury
# Date: 1 July 2021
# Quantitative Ecology
# Beta-diversity - PART 2

library(tidyverse) 
library(betapart) # Partitioning beta diversity into turnover & nestedness components
library(vegan)
library(gridExtra) # Arrange multiple grid-based plots on a page and draw tables 
library(BiodiversityR)
library(grid)
library(gridBase)
library(tidyr)

spp <- read_csv("Quantitative_Ecology-main/exercises/diversity/SeaweedsSpp.csv")

View(spp)
unique(spp)
summary(spp)
head(spp) # To look at the top half of the data
tail(spp) # To look at the bottom half of the data
dim(spp) # To get the dimensions of the data 

# First number is for the number of rows, second number for the columns

# How to look at what is happening within the rows & columns

spp[1:5, 1:5] # Looks at the first five rows and columns

spp[(nrow(spp) - 5):nrow(spp), (ncol(spp) - 5):ncol(spp)] # Looks at the last five rows & columns

# Rows = sites , columns = species


#-------------------------BETA DIVERSITY----------------------------------------
# Filter data in order to remove first column name:
spp <- dplyr::select(spp, -1)

# Decompose total Sørensen dissimilarity into turnover and nestedness-resultant components:
Y.core <- betapart.core(spp)
Y.pair <- beta.pair(Y.core, index.family = "sor") #Pair-wise functional dissimilarities

Y.core <- betapart.core(spp)

view(Y.core)

# Let Y1 be the turnover component (beta-sim):
Y1 <- as.matrix(Y.pair$beta.sim)

# Let Y2 be the nestedness-resultant component (beta-sne):
Y2 <- as.matrix(Y.pair$beta.sne)

round(Y1[1:10, 1:10], 4) # rounds decimals to 4 spaces

round(Y2[1:10, 1:10], 4)

Y1_sub1 <- data.frame(Y1[1:1, 1:58]) # Creating a d.f.

Y1_sub1

# Make a species-turnover graph:

ggplot(data = Y1_sub1, (aes(x = 1:58, y = Y1_sub1[,1])))+
  geom_line(size = 0.75, col = "turquoise", linetype = "dashed")+
  labs(title = "Species turnover across a coastal section of South",
       subtitle = "??-diversity graphical analysis",
       x = "Coastal section, west to east (km)",
       y = "Species Turnover (??sim)")

# Question 1: 
# As the environment is changing over larger physical distances, there is a larger 
# amount of dissimilarity between species. This affects the species turnover as well. 
# The further away from the starting point of the coastal section, more species are
# lost or gained, as their "sweet spot" has changed and new species are introduced. This
# changes the species composition from section to section, but does not affect the 
# alpha-diversity of the section. The species turnover is the highest at 60km and the lowest 
# species turnover is at starting point of 0km. The increase in species turnover may 
# be due to different environmental conditions or gradients that in turn affect the species
# composition of the area, and NOT affecting the species composition of the coastal section.


# Question 2: 
# Nested- resultant beta-diversity is defined as the processes that influence
# the gain or lost of species and the community with the lowest alpha-diversity 
# is the subset of the richer community. Small scale habitat heterogeneity does not affect
# the beta-diversity of a 50km coastline. When beta-diversity is measured at the scale of 
# a country, the beta diversity is high and is affected by the processes that change the 
# species turnover. The West coast of South Africa shows high rates of nestedness-resultant
# beta-diversity that may be due to the neutral assembly process that occur in this region.
# The temperature gradient that occurs in August is of ecological importance as it 
# shows the constant dissimilarity in thermal distance. The upwelling phenomenon that 
# occurs on the west coast contributes to the low temperature gradient that occurs there 
# which adds to the low alpha-diversity in the area rather than the alpha diversity 
# that occurs in the sub-tropical, warm temperate waters and the east coast of South 
# Africa. Beta-diversity is lower within individual bioregions due to the shorter 
# coastlines. In bioregions, beta-diversity is dominated by species turnover rather than
# nestedness-resultant. Benguela Marine Province has a 58& nestedness-resultant 
# beta-diversity. The steep environmental gradients and niche influences affect 
# this percentage. The dominance of the nestedness-resultant beta-diversity component
# within the Benguela Marine Province suggests the environment is complex. 

#(Smit et al., 2017)
# 
