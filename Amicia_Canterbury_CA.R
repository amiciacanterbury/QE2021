# Author: Amicia Canterbury
# Date: 14 July 2021
# Quantitative Ecology
# Topic 9: Correspondence Analysis (CA)

# Load necessary packages: 

library(tidyverse)
library(vegan)

# Load the data - Using Doubs data but sp. data:

spe <-  read.csv("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/DoubsSpe.csv")
spe <- dplyr::select(spe, -1) # Remove first column
head(spe, 8) # To look at the top 8 rows instead of 6

#--------------------------DOING THE CA-----------------------------------------

# We use the {vegan} package to use the cca() function for the CA and for the 
# Constrained correspondence analysis (CCA):

spe_ca <- cca(spe) # Finds the CA
spe_ca # Views the data in the console 

# Error occurs: all row sums must be >0 in the community data matrix
# Remove rows that are = 0 

apply(spe, 1, sum) # Looks at the sums of the rows, one needs to eliminate the 
                   # row that is equal to 0

# Row 8 has a sum of 0, therefore we omit it from the dataset: 

spe <- spe[rowSums(spe) > 0, ] # To omit the row 
head(spe, 8) # Now looks at the data without row 8

# Do the PCA with the new data matrix:

spe_ca <- cca(spe)
spe_ca

summary(spe_ca)

# Calculate the total inertia manually: 

round(sum(spe_ca$CA$eig), 5)

# 1.16691

# The inertia for the first axis (CA1) is:

round(spe_ca$CA$eig[1], 5)

# CA1 = 0.60099

# The inertia of CA1 and CA2 is:

round(sum(spe_ca$CA$eig[1:2]), 5)

# 0.74536

# The fraction of the variance explained by CA1 and CA2 is:

round(sum(spe_ca$CA$eig[1:2]) / sum(spe_ca$CA$eig) * 100, 2) # result in %

# 63.87%

#------------------------------NOTES--------------------------------------------
# - The above outcomes are the same that one would find in the Cumulative 
#   Proportion in the summary() under the CA2 column.
# - Sp. scores = actual sp. scores (using sp. data)
# - Most positive & negative eigenvectors (loadings), indicate the sp. that are 
#   most dominant in their influence with the CA axes. 
# - Site scores are also as seen earlier in PCA. The highest positive or negative 
#   loadings indicate sites that are dispersed far apart on the biplot 
#  (in ordination space). They will have large differences in fish community composition.

#-------------------------ORDINATION DIAGRAMS-----------------------------------

plot(spe_ca, scaling = 1, main = "CA fish abundances - biplot scaling 1")
plot(spe_ca, scaling = 2, main = "CA fish abundances - biplot scaling 2")

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(spe, tmp <- ordisurf(spe_ca ~ Satr, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Satr"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_ca ~ Scer, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Scer"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_ca ~ Teso, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Teso"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_ca ~ Cogo, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Cogo"))
abline(h = 0, v = 0, lty = 3)

# a posteriori projection of environmental variables in a CA
env <- read.csv("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/DoubsEnv.csv")
env <- dplyr::select(env, -1)

# we removed the 8th row in spe, so do it here too
env <- dplyr::slice(env, -8)

# the last plot produced (CA scaling 2) must be active
# scaling 2 is default
(spe_ca_env <- envfit(spe_ca, env, scaling = 2))
plot(spe_ca_env, col = "grey40")
plot(spe_ca_env, p.max = 0.05, col = "red") # plot significant variables with a 
                                            # different colour

#--------------------------------QUESTIONS--------------------------------------

# Question 1:-------------------------------------------------------------------
# How would you explain the patterns seen in the four panels of the above figure?

# The patterns that can be seen in the above plot are seperated into four 
# individual plots representing the top two positive and top two negative species 
# sites when calculating the Correspondence Analysis (CA).
# The four individual plots constitute for the most abundant species within the 
# CA1 which equate to the sum of 51.5% variation explained when looking at the 
# CA1.

# The first individual plot is for Satr. The bubbles that are found on the plot 
# represent the amount of abundance of the species at a specific site. The green
# lines represent contour lines the same way it would be described in geography, 
# where the contours represent the connection between sites with the elevation.
# In the case of these contour lines, the sites are being connected with other 
# sites with similar species abundance. The abundance of the species Satr. is 
# mainly abundant in the upper and lower right side of the graph. It shows that 
# this species has a gradient that flows from high abundance to low abundance.
# The environmental gradients that are plotted indicate that Satr. has the highest
# species abundance in areas with high altitude with large slopes. 

# The second individual plot represents the species Scer. The bubbles that are 
# found on this plot is mainly found on the left-hand side of the graph. This 
# indicates that the species abundance is most in this specific area. The contour
# lines of this graph is closer to one another indicating that there is a steeper
# slope here than with Satr. The contour lines indicate that the species abundance
# is more present in this area where environmental gradients such as biological
# oxygen demand (mg.L^-1) are most present. The biological oxygen demand(BOD) 
# represents the amount of oxygen that bacteria would consume via anerobic 
# respiration at a certain temperature in the water. This indicates that this 
# species of fish feeds on the bacteria and microorganisms in the water and 
# due to the high rates of BOD, the Scer would be most abundant. The smaller 
# bubbles indicate that the abundance is not as much in certain areas due to 
# the above mentioned theory.

# The third individual plot represents the species Teso. The species abundance 
# (indicated by the large bubbles) is concentrated in the right hand side of the 
# graph where oxygen (mg.L^-1) is concentrated. The contour lines of this specific
# plot is curved in a wide U-shape where it may indicate a gradient from high
# abundance to low abundance of the species. The oxygen gradient that is present
# in this area of concentration alludes to the fact that this species of fish 
# thrives in areas with large quantities of oxygen. Large amounts of oxygen 
# in the river is due to the groundwater discharge as the river is in constant 
# motion. Due to the large amounts of oxygen found in the water, eutrophication
# may potentially happen if the bacteria consumes the oxygen as organic matter.
# This may not be the case as the bacteria is found in the opposite direction 
# of the oxygen gradient where the BOD gradient is present. 

# The last individual plot represents the species Cogo. The species abundance is 
# concentrated in the right hand side of the graph where the oxygen gradient is 
# present. The contour lines of this specific species is circular indicating that 
# the sites are all near one another in only this area. Due to the contour lines
# being circular, the gradient that is present here is constant between the high
# and low abundances of fish. The smaller bubbles do still indicate that there are
# sites with less abundance. As mentioned above this species thrives in water 
# that is oxygen-rich. Teso and Cogo have similar "sweet spots" when looking 
# at what their environmental needs are to thrive. 


# Question 2:-------------------------------------------------------------------

# BIRD DATA: 

# Load the data 

ybirds.spe <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/ybirds_spe.txt', row.names = 1)
birds_spe <-  ybirds.spe # Just renaming

birds_spe # View the data in the console 

dim(birds_spe)

# 50 rows & 59 columns

head(birds_spe)

# Do the CA: 
birds_ca<- cca(birds_spe) # Finds the CA
birds_ca 

summary(birds_ca)

# Calculate the total inertia manually: 

round(sum(birds_ca$CA$eig), 5)

# 2.00751

# The inertia for the first axis (CA1) is:

round(birds_ca$CA$eig[1], 5)

# CA1 = 0.74767

# The inertia of CA1 and CA2 is:

round(sum(birds_ca$CA$eig[1:2]), 5)

# 1.08145

# The fraction of the variance explained by CA1 and CA2 is:

round(sum(birds_ca$CA$eig[1:2]) / sum(birds_ca$CA$eig) * 100, 2) # result in %

# 53.87% 

# Make the plots:

# In the console: 
# par(mfrow = c(1,1)) # To remove graphs from being put together 

plot(birds_ca, scaling = 1, main = "CA bird abundances - biplot scaling 1")
plot(birds_ca, scaling = 2, main = "CA bird abundances - biplot scaling 2")

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(birds_spe, tmp <- ordisurf(birds_ca ~ ALA , bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Prunella collaris"))
abline(h = 0, v = 0, lty = 3)
with(birds_spe, tmp <- ordisurf(birds_ca ~ WRN, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Troglodytes troglodytes"))
abline(h = 0, v = 0, lty = 3)
with(birds_spe, tmp <- ordisurf(birds_ca ~ SWP, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Lophura swinhoii"))
abline(h = 0, v = 0, lty = 3)
with(birds_spe, tmp <- ordisurf(birds_ca ~ ILT, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Turdus poliocephalus"))
abline(h = 0, v = 0, lty = 3)

# Load environmental data: 

bird_env <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/ybirds_env.txt', row.names = 1) %>% 
  select(-`Veg.`, -Veg_ext)
bird_env <- dplyr::select(bird_env, - 1)

bird_env

# the last plot produced (CA scaling 2) must be active
# scaling 2 is default

(birds.env_ca <- envfit(birds_ca, bird_env, scaling = 2))
plot(birds.env_ca, p.max = 0.05, col = "blue") # plot significant variables with a different colour


# ALPINE DATA: 

# Load the data:

library(readr)
alpine_sp <- read_csv("Quantitative_Ecology-main/aravo_sp.csv") %>% 
  select(-`X1`)

view(alpine_sp)

dim(alpine_sp) # Read dimensions
# 75 rows & 82 columns
head(alpine_sp)

# Do the CA:

alpine_ca <- cca(alpine_sp) # Finds the CA
alpine_ca

summary(alpine_ca)

# Calculate the total inertia manually: 

round(sum(alpine_ca$CA$eig), 5)

# 4.21441

# The inertia for the first axis (CA1) is:

round(alpine_ca$CA$eig[1], 5)

# CA1 = 0.066068

# The inertia of CA1 and CA2 is:

round(sum(alpine_ca$CA$eig[1:2]), 5)

# 1.08111

# The fraction of the variance explained by CA1 and CA2 is:

round(sum(alpine_ca$CA$eig[1:2]) / sum(alpine_ca$CA$eig) * 100, 2) # result in %

# 25.65%

# Make the plots:

# In the console: 
# par(mfrow = c(1,1)) # To remove graphs from being put together 

plot(alpine_ca, scaling = 1, main = "CA vascular plant abundances - biplot scaling 1")
plot(alpine_ca, scaling = 2, main = "CA vascular plant abundances - biplot scaling 2")

# Check summary for high species scores:

summary(alpine_ca)

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(alpine_sp, tmp <- ordisurf(alpine_ca ~ Bart.alpi , bubble = 3,
                                family = quasipoisson, knots = 2, col = 6,
                                display = "sites", main = "Bartsia alpina L."))
abline(h = 0, v = 0, lty = 3)
with(alpine_sp, tmp <- ordisurf(alpine_ca ~ Sali.retu, bubble = 3,
                                family = quasipoisson, knots = 2, col = 6,
                                display = "sites", main = "Salix reticulata L."))
abline(h = 0, v = 0, lty = 3)
with(alpine_sp, tmp <- ordisurf(alpine_ca ~ Alch.pent, bubble = 3,
                                family = quasipoisson, knots = 2, col = 6,
                                display = "sites", main = "Alchemilla pentaphyllea L."))
abline(h = 0, v = 0, lty = 3)
with(alpine_sp, tmp <- ordisurf(alpine_ca ~ Poa.supi, bubble = 3,
                                family = quasipoisson, knots = 2, col = 6,
                                display = "sites", main = "Poa supina Schrader"))
abline(h = 0, v = 0, lty = 3)


# Load environmental data: 
library(readr)
alpine_env <- read_csv("Quantitative_Ecology-main/aravo_env.csv") %>% 
  select(-X1, -ZoogD) # Descriptive variables must be taken out

alpine_env

# the last plot produced (CA scaling 2) must be active
# scaling 2 is default

(alpine.env_ca <- envfit(alpine_ca, alpine_env, scaling = 2))
plot(alpine.env_ca, p.max = 0.05, col = "blue") # plot significant variables with a different colour


# Question 3--------------------------------------------------------------------

# Bird data: 

# Species Prunella collaris (ALA) has the strongest influence on the variation
# found in CA1. This species of bird was most abundant in sites 45 to 50. This 
# species of bird thrives more in an environment with high amounts of ground 
# cover, low tree density, herb cover and aspect. 

# Species Troglodytes troglodytes (WRN) is found concentrated in sites 37 to 50.
# This species of bird thrives in an environment with ground cover, slope, 
# conifers, elevation and tree diameter at breast height. If there are high 
# levels of these environmental gradients, the species will thrive. This 
# species also thrives in areas where the slope is higher.

# Due to ALA and WRN being the highest negative eigenvectors, these species will 
# occur in a similar area. The environment will have to undergo similar intensities 
# of the gradients in order for the species to coexist with one another. The WRN
# species will occur higher up than that of ALA. 

# Species Lophura swinhoii (SWP) had one large bubble indicating that it is most
# abundant in just one area of the mountains. It is concentrated in an area where
# environmental gradients such as tree cover (TC) and shrub cover(SC) is high. 
# This species is abundant mostly at site 2. Site 2 is found in the lower 
# parts of the mountain slope. 

# Species Turdus poliocephalus (ILT) is found within the same region as SWP. 
# This species of bird is also found in the lower part of the mountain slope. 
# It is found in between two environmental gradients namely the tree density(TD) 
# and shrub cover (SC). When tree density is high the shrub cover will be lower 
# as there is limited access to resources such as sunlight. When shrub cover is 
# higher, this indicates that there is less tress. This species is found predominantly
# in areas near to shrub cover and thus is found in sites 1 to 5. 


# ALPINE DATA: 
# Bartsia alpina L. is found mainly in an area with the physical gradient of 
# physical distance. It is concentrated in one area where the largest bubbles overlap. 

# Species Salix reticulata L. is found in the same area as that of Bartesia alpina.
# These two species of vascular plants would coexist in an area with minimal 
# environmental gradients.

# Species Alchemilla pentaphyllea L. is a species of vascular plant that 
# is concentrated in an area with the environmental gradients of form 
# and snow. This species of vascular plants has a gradient of high abundance to low 
# abundance due to the u-shaped contour lines. This species is found throughout 
# the two environmental gradients. One cannot see which type of slope this specific 
# species of plant is most abundant in, however regardless of the shape, it thrives 
# in any type of slope. It is recorded that this species of plant was highly 
# abundant when the data was taken in 1997 - 1999 when the snow environmental 
# gradient was the highest. 

# Species Poa supina Schrader is a species that is concentrated in in an area where
# the most dominant environmental gradient is Form. The form gradient indicates the 
# shape of the slope. It is coexisting with the A. pentaphyllea as both species 
# depend on the different forms of the slopes. 

