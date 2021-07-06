# Author: Amicia Canterbury
# Date: 5 July 2021
# Quantitative Ecology
# Topic 6: Correlations & associations 

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
library(Hmisc) # Used for rcorr() 

#----------------------PEARSON'S CORRELATION COEFFICIENT------------------------
# Load data: 
env <- read_csv("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/DoubsEnv.csv")

dim(env) # To see the dimensions of the data 

# 30 rows & 12 columns 
# rows = localities
# columns = environmental variables 

# filter the data to remove the first row:
env <- dplyr::select(env, -1)

head(env) # Looks at the top half of the data 
tail(env) # Looks at the bottom half of the data 

# Because data is in different units, one would need to standardize the data 

view(env)

env.pearson <- cor(env) # default method = "pearson"

# Pearson's product-moment correlation: 
# - measure used for linear correlation
# - creates normalized data for covariance 
# - values are between -1 and 1

env.pearson2 <- round(env.pearson, 2) # Rounds the decimals to 2 decimal places

# No need to transpose/ standardize as the cor() function does it for you
# Use correlations in order to see how environmental variables relate to one
# another across sample sites. 

# If you want to see the associated p-values for statistical significance:

rcorr(as.matrix(env)) # Creates a matrix of Pearson/ Spearman's correlation 
                      # coefficients for all possible pairs of columns of a matrix.

#-----------------------ASSOCIATION - Species P/A-------------------------------

# Data is an example of abundance data

# Load data: 
spe <- read_csv("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/DoubsSpe.csv")

# Filter data in order to remove first column name:
species <- dplyr::select(spe, -1)

head(species) # Looks at the top half of the data set.

view(species)

# First transpose data to calculate the association of the data matrix:
spp_t <- t(species)

spp_t
# Calculate the association of the data matrix:
spp_assoc1 <- vegdist(spp_t, method = "jaccard")
as.matrix((spp_assoc1))[1:10, 1:10] # display only a portion of the data

spp_assoc2 <- vegdist(spp_t, method = "jaccard", binary = TRUE)
as.matrix((spp_assoc2))[1:10, 1:10] # display only a portion of the data

#--------------------------------QUESTIONS--------------------------------------

# PART A:

# Question 1: Create a plot for pairwise correlation: 

library(corrplot)

corrplot(env.pearson2, method = "color",
         col = colorRampPalette(c("orangered2","yellow1", "red4" ))(100),
         title = "Correlation matrix for environmental variables 
         in the Doubs River",
         mar = c(0, 0, 5, 0))

# Question 2: Name to two top positive and two top negative 
# statistically-significant correlations:

env.correlation <- rcorr(as.matrix(env))# To get the p-values
view(env.correlation)

# Positive = the closest value to 1
# Negative = the closest value to -1
# No relationship = 0

# Flo & dfs and amm & pho is the top two positive statistically-significant
# correlation.

# Flo & alt and dfs & alt is the top two negative statistically-significant 
# correlation. 

# Question 3: For each, discuss the mechanism behind the relationships. 
# Why do these relationships exist?

# Flo represents the mean minimum discharge (m^3.s^-1) and dfs represents the 
# distance from the source(km). The strong correlation between the two is due to
# the fact the further away from the source, the more increased the mean minimum
# discharge will be. The relationship exists because discharge is the flow of 
# water that is transported over an area. The further away from the source, the 
# higher the mean minimum discharge will be as it is going over a larger distance.

# The second top two positive statistically-significant correlation occurs between
# the concentrations of ammonium and phosphate (both measured in mg/L^-1). Both 
# of these are related to the nutrient pollution that is found in the water of the 
# river. Due to the high correlation, one can assume that the two will have 
# a negative effect on the water quality in the river. Having an excess of both
# will cause the degradation of the habitats found in river. Due to the river 
# being found near a mountain range, the source of the nutrients may be from the 
# soil and also decaying plant material. The chemicals both cause a reduction
# of the amount of oxygen in the water. 

# The first top two negative statistically-significant correlation is between 
# mean minimum discharge (m^3.s^-1) and the altitude(m as.1). These two 
# environmental variables are correlated because the higher one would go in altitude,
# the lower the amount of discharge would be found. The correlation is negative
# because the p-value is higher than 0.05. 

# The second top negative statistically-significant correlation is between 
# distance from the source (km) and altitude (m as.1). This relationship occurs 
# because the higher one would go in altitude, the closer one would get to the 
# source. 



# PART B:

# Question 1:

# In the original data matrix, the correlation is between the the different sites
# and the different species of fish found in the Doubs River. When one transposes
# the data, the correlation changes to the correlation between the fish species 
# and the occurrence of the fish species in each site. The reason we have to 
# transpose the data is to see the occurrence of the different fish species 
# at specific sites on the river. The new correlation matrix will be thus 
# identified and one could associate the correlations to the environmental
# variables  present. 

# Question 2: 

# Properties of a transposed data matrix is as follows: 
# When the data is in its original state, after correlation tests are run, the 
# correlation between species will be evident. When we transpose the data, 
# the sites becomes the main subject of the correlation tests. If this is done,
# the correlation between sites can be tested against the environmental 
# conditions/gradients present.The y-plain is now the species and 
# the x-plane is now the sites. 

# PART C: 

# Question 1: 
# An association matrix, also known as coefficients/ indices, is in the form of 
# a square and symmetrical matrix made up of dimensions. The structure of the 
# association matrix is similar to that of the species dissimilarity matrix and
# the correlation matrix. When looking at the association matrix, it is made up 
# of two different categories. The first category is the Q-mode that is used for 
# when pairs of objects are compared. The second category is the R-mode, which 
# looks at the comparisons of the pairs of descriptors. Q-mode uses dissimilarity
# and similarity indices such as Jaccard and the R-mode uses co-variance or
# the correlation coefficients. The 0-value diagonal is present in the species 
# dissimilarity matrix and the correlation matrix, but is difficult to interpret.
# When looking at the association matrix, it is easy to establish the correlation 
# when looking at the 0-value diagonal. They differ because species dissimilarity
# matrices looks at the dissimilarity or similarity between the different species.
# The correlation matrix shows the correlation between the coefficients.
# In contrast, the association matrix looks at the comparisons between pairs 
# of descriptors or when objects are compared. 

# Question 2: 
# The first association data matrix is using the Jaccard index in order to 
# create a species abundance data matrix. When using the binary = TRUE for 
# the second association data matrix, one is creating a presence/absence data 
# matrix. The two association data matrices are different because the one is 
# looking at the species abundance using pair-wise comparison between the different
# fish species, whereas the other one is showing the presence/absence data for
# the different fish species. The presence/absence association data matrix 
# is weighting the rare species. 

# Question 3: 
# The data can be used for quick analysis between the species association matrix.
# When looking at presence/absence data, the binary coefficients in the R-mode
# can be used to compare species. Association matrices are intermediate system 
# which is rarely used. 

