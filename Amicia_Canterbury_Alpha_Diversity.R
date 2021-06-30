# Author: Amicia Canterbury
# Date: 29 June 2021
# Quantitative Ecology
# Alpha diversity Question - PART 1

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
dim(spp)

# 58 rows, 848 columns

#------------------------------Questions:---------------------------------------

# 1. Dissimilarity matrices use pairwise comparison between the sampling units.
# The square that occurs with the dissimilarity matrix is due to the fact the 
# number of rows are equal to the number columns, regardless if it is abundance 
# data or presence-absence data. It also means that the dissimilarity matrix is 
# symmetrical and the matrix would be the same if it had to be reversed.

# 2. The values of the dissimilarity matrix range from 0 to 1. The diagonal falls
# on the 0. The sites are pair-wise comparisons, comparing the one site to itself 
# and other sites. Due to the sites being compared to itself and every other site, 
# it creates a diagonal because a site compared to itself = 0 (identical).

# 3. The non-diagonal elements of the dissimilarity matrix occurs when sites are 
# compared to every other site except themselves. It shows the dissimilarity between
# each site and the site that it is being compared to. 

# Question 4:

spp[1:1, 1:5] # To see the first 5 rows and columns

spp[(nrow(spp) - 5):nrow(spp), (ncol(spp) - 5):ncol(spp)] # To see the last 5 rows & columns

spp

dissimilar_spp <- vegdist(spp, binary = TRUE) # Creates a dissimilarity index, binary = TRUE sets to presence/absence data
dissimilar_df <- round(as.matrix(dissimilar_spp),4) #Rounds the numbers to a decimal of 4 

view(dissimilar_df)
dim(dissimilar_df) # To see the number of columns & rows 

# 58 rows, 58 columns

dissimilar_data <- as_tibble(dissimilar_df[1:1, 1:58]) # converts data to d.f.

view(dissimilar_data)

ggplot(data = dissimilar_data, aes(y = value, x = 1:58))+
  geom_line(size = 1, col = "coral2")+
  labs(title = "Dissimilarity index of a coastal section of South Africa", 
       subtitle = "Using Sørensen dissimilarity index", 
       x = "Coastal section (W to E)", 
       y = "Dissimilarity Index")+
  theme_bw()

# Question 5: 

# As the geographical distance between the sites increase, the more dissimilar 
# the sites become to one another. New biodiversity occurs more often further away
# from site A, meaning the different species have different optimal temperature 
# "sweet spots". Due to the this, more similar species will be found in gradients
# with similar temperature conditions. Other species will then become more scarce,
# depending on which species one looks at. The shape of the graph alludes to the 
# principle of the closer the sites are to one another, the similar the species,
# and the further they are from one another when doing pair-wise comparisons, the 
# larger the dissimilarity index. 
