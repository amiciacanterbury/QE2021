# Author: Amicia Canterbury
# Date: 29 June 2021
# Quantitative Ecology
# Measures of Biodiversity Code

library(tidyverse)
library(betapart)
library(vegan)
library(gridExtra)
library(BiodiversityR)
library(grid)
library(gridBase)
library(tidyr)
library(readr)

spp <- read_csv("diversity/SeaweedsSpp.csv")

View(spp)
unique(spp)
summary(spp)
head(spp)
tail(spp)
dim(spp) # To get the dimensions of the data 

# First number is for the number of rows, second number for the columns

# How to look at what is happening within the rows & columns

spp[1:5, 1:5] # Looks at the first five rows and columns

spp[(nrow(spp) - 5):nrow(spp), (ncol(spp) - 5):ncol(spp)] # Looks at the last five rows & columns

# Rows = sites , columns = species

#---------------------------Species richness------------------------------------

spp_richness <- diversityresult(spp, index = 'richness', method = 'each site')

ggplot(data = spp_richness, (aes(x = 1:58, y = richness))) +
  geom_line(size = 1.5, col ="green4") + 
  labs(title = "Species richness from a coastal section \nrunning from West to East of South Africa", 
       x = "Coastal section, west to east(km)", 
       y = "Species richness", col = "green4", lwd = 2)+
  theme_bw()

# If the BiodiversityR doesn't work, use specnumber() in {vegan}

# Use 'MARGIN = 1' to calculate the number of species within each row (site)
specnumber(spp, MARGIN = 1)

#------------Univariate diversity indices(Shannon/Simpson)----------------------

# Another way to show alpha-diversity 
# always use abundance data 

light <- read_csv("diversity/light_levels.csv")

summary(light)
head(light)
tail(light)
dim(light)

light

# Calculate species richness & then use diversity() in {vegan}

light_div <- data.frame(
  site = c("low_light", "mid_light", "high_light"),
  richness = specnumber(light[, 2:7], MARGIN = 1),
  shannon = round(diversity(light[, 2:7], MARGIN = 1, index = "shannon"), 2),
  simpson = round(diversity(light[, 2:7], MARGIN = 1, index = "simpson"), 2)
)

light_div

#-----------------------Dissimilarity indices-----------------------------------

# If data is P/A data, use Sørensen Index

#The vegan function vegdist() provides access to the dissimilarity indices

# Calculate Sørensen Index: 

sor <- vegdist(spp, binary = TRUE) # binary = TRUE sets to presence/absence data
sor_df <- round(as.matrix(sor), 4)
sor_df[1:20, 1:20] # the first 20 rows and columns

# This creates a square dissimilarity matrix
# Important characteristics:
# - Whereas the raw species data (Y) is rectangular (number rows ??? number columns), 
#   the dissimilarity matrix is square (number rows = number columns).
# - The diagonal is filled with 0.

#-----------------------------Gamma diversity-----------------------------------

# the number of columns gives the total number of species in this example:

ncol(spp)

#848 species

#----------------??-diversity: traditional interpretations-----------------------

# True ??-diversity:

true_beta <- data.frame(
  beta = specnumber(spp, MARGIN = 1) / ncol(spp),
  section_no = c(1:58)
)
# true_beta
ggplot(data = true_beta, (aes(x = section_no, y = beta))) +
  geom_line(size = 1.5, col ="blue") + 
  labs(title = "True beta-diversity from a coastal section \nrunning from West to East of South Africa", 
       x = "Coastal section, west to east(km)", 
       y = "True beta-diversity")+
  theme_bw()

# Absolute species turnover:

# substract ??-diversity for each section from the region's ??-diversity

abs_beta <- data.frame(
  beta = ncol(spp) - specnumber(spp, MARGIN = 1),
  section_no = c(1:58)
)

# abs_beta
ggplot(data = abs_beta, (aes(x = section_no, y = beta))) +
  geom_line(size = 1.5, col = "red2")+
  labs(title = "Absolute beta-diversity from a coastal section \nrunning from West to East of South Africa", 
                   x = "Coastal section, west to east(km)", 
                   y = "Absolute beta-diversity")+
  theme_bw()




